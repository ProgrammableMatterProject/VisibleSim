/**
 * @file   coatingMessages.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Thu Jan  9 10:26:27 2020
 *
 * @brief
 *
 *
 */

#include "coatingMessages.hpp"
#include "coatingBlockCode.hpp"

void SupportSegmentCompleteMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);

    // cb.catom->setColor(YELLOW);

    // not a support module, forward message along the segment
    if (cb.expectedSegments.count(sourceInterface->hostBlock->position)) {
        if (++cb.numCompletedSegments == cb.expectedSegments.size()) {
            if (not cb.isSupportPosition(cb.catom->position)) {
                cb.segmentsAckBlacklist.insert(sourceInterface->hostBlock->position);
                cb.notifyAttracterOfSegmentCompletion(cb.segmentsAckBlacklist,
                                                      destinationInterface);
            } else {
                // cb.catom->setColor(LIGHTBLUE);
                unsigned int layer = cb.getGLayer(cb.catom->position);

                if (cb.supportsReadyRequestItf != nullptr) {
                    // Notify of segment completion if previously asked
                    // Segments are done being built
                    cb.sendMessage(new SupportReadyResponse(cb.numCompletedSegments),
                                   cb.supportsReadyRequestItf, MSG_DELAY, 0);
                    cb.supportsReadyRequestItf = nullptr;
                }

                if (++cb.planeSupportsReady[layer] == cb.planeSupports[layer].size()) {
                    if (layer == 0) {
                        if (cb.lattice->isFree(cb.G_SEED_POS)) {
                            cb.world->addBlock(0, cb.buildNewBlockCode, cb.G_SEED_POS, CYAN);
                        } else { // Might be on a segment
                            if (cb.isCoatingCornerCell(cb.G_SEED_POS)) {
                                static_cast<CoatingBlockCode*>(
                                    cb.lattice->getBlock(cb.G_SEED_POS)->blockCode)
                                    ->handleBorderCompletion(Cell3DPosition(-1,-1,-1), false);
                            } else {
                                static_cast<CoatingBlockCode*>(
                                    cb.lattice->getBlock(cb.G_SEED_POS)->blockCode)
                                    ->startBorderCompletion();
                            }
                        }
                    } else {
                        // NOTE: Should be useless now, along with planeSupportsReady
                        // cb.attractPlane(layer);
                    }
                }
            }
        }
    } else if (cb.isSupportPosition(cb.catom->position)) {
        // Wrong support
        cb.sendMessage(new SegmentCompleteWrongSupport(), destinationInterface, MSG_DELAY, 0);
    }
}

void SegmentCompleteWrongSupport::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);

    VS_ASSERT(not cb.isSupportPosition(cb.catom->position));

    cb.notifyAttracterOfSegmentCompletion(cb.segmentsAckBlacklist, destinationInterface);
}

void BorderCompletionMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);

    if (cb.handledBorderCompletion) return; // Algorithm over for border, we went full circle

    const Cell3DPosition& sender = sourceInterface->hostBlock->position;
    if (sender[2] == cb.catom->position[2])
        cb.handleBorderCompletion(sender, stopAtCorner);
    else // This is the first message, coming from the seed module
        cb.startBorderCompletion();
}

void NextPlaneSupportsReadyMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);

    cb.segmentsDetected = segmentsDetected;
    cb.supportsReadyBacktraceItf = destinationInterface;

    if (cb.isSeedPosition(cb.catom->position)) {
        set<Cell3DPosition> processed;
        for (const auto& seedPair : cb.planeSeed[0]) {
            Cell3DPosition lowest;
            cb.seeding->findLowestOfBorderFrom(seedPair.first, lowest);
            const Cell3DPosition& firstPos = cb.getStartPositionAboveSeed(lowest);

            if (processed.count(lowest)) continue;

            CoatingBlockCode& cs = *static_cast<CoatingBlockCode*>(cb.lattice->getBlock(lowest)->blockCode);

            if (segmentsDetected) {
                // Check if module right above is in place
                if (cs.lattice->isFree(firstPos)) {
                    // Mark for border completion and attract
                    cs.borderCompleted.insert(firstPos);
                    cs.completionNeighbors.insert(firstPos);
                    cs.nextBorderItf = cb.catom->getInterface(firstPos);
                    cs.sendAttractSignalTo(firstPos);
                } else {
                    cs.startBorderCompletionAlgorithm();
                }
            } else {
                if(cs.lattice->isFree(firstPos))
                    cs.sendAttractSignalTo(firstPos);
            }
        }

        return;
    }

    // Border module, keep forwarding along the border
    // It is assumed that a module can only have one support neighbor
    bool hasSupportNeighborAbove = false;
    P2PNetworkInterface *supportItf = nullptr;
    for (const Cell3DPosition& nPos : cb.lattice->getNeighborhood(cb.catom->position)) {
        if (nPos[2] > cb.catom->position[2] and cb.isSupportPosition(nPos)) {
            hasSupportNeighborAbove = true;
            supportItf = cb.catom->getInterface(nPos);
        }
    }

    cb.supportsReadyBlacklist.insert(sourceInterface->hostBlock->position);

    if (not hasSupportNeighborAbove) {
        // Forward message further along the border
        // const Cell3DPosition& sender = sourceInterface->hostBlock->position;
        Cell3DPosition next = cb.findNextCoatingPositionOnLayer
            (cb.supportsReadyBlacklist,
             //  ensure that we always stay below the next
             //   coating layer
             // FIXME: This is guaranteed to lead to issues
             //         later on. e.g., if seed isn't below the coating layer
             [&cb](const Cell3DPosition& p) {
                 for (const Cell3DPosition& np : cb.lattice->getNeighborhood(p)) {
                     if (np[2] > p[2] and cb.isInG(np)) {
                         return true;
                     }
                 }

                 return false;
             });

        P2PNetworkInterface* nextItf = cb.catom->getInterface(next);
        if (nextItf != nullptr and nextItf->isConnected()) {
            cb.sendMessage(new NextPlaneSupportsReadyMessage(segmentsDetected),
                           nextItf, MSG_DELAY, 0);
            cb.supportsReadyBlacklist.insert(next);
        } else {
            // Notify sender that we are stuck and backtrace to next other border option
            cb.sendMessage(new NextPlaneSupportsReadyReturn(segmentsDetected),
                           cb.supportsReadyBacktraceItf, MSG_DELAY, 0);
        }
    } else {
        // cb.lastBorderFollowingPosition = sourceInterface->hostBlock->position;
        VS_ASSERT(supportItf != nullptr and supportItf->isConnected());
        cb.sendMessage(new SupportReadyRequest(), supportItf, MSG_DELAY, 0);
    }
}

void NextPlaneSupportsReadyReturn::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);

    cb.segmentsDetected = cb.segmentsDetected or segmentsDetected;

    // Forward message further along the border
    // const Cell3DPosition& sender = sourceInterface->hostBlock->position;
    Cell3DPosition next = cb.findNextCoatingPositionOnLayer
        (cb.supportsReadyBlacklist,
         //  ensure that we always stay below the next
         //   coating layer
         // FIXME: This is guaranteed to lead to issues
         //         later on. e.g., if seed isn't below the coating layer
         [&cb](const Cell3DPosition& p) {
             for (const Cell3DPosition& np : cb.lattice->getNeighborhood(p)) {
                 if (np[2] > p[2] and cb.isInG(np)) {
                     return true;
                 }
             }

             return false;
         });

    P2PNetworkInterface* nextItf = cb.catom->getInterface(next);
    if (nextItf != nullptr and nextItf->isConnected()) {
        cb.sendMessage(new NextPlaneSupportsReadyMessage(cb.segmentsDetected),
                       nextItf, MSG_DELAY, 0);
        cb.supportsReadyBlacklist.insert(next);
    } else {
        // Notify sender that we are stuck and backtrace to next other border option
        cb.sendMessage(new NextPlaneSupportsReadyReturn(cb.segmentsDetected),
                       cb.supportsReadyBacktraceItf, MSG_DELAY, 0);
    }
}

void SupportReadyRequest::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);

    VS_ASSERT(cb.isSupportPosition(cb.catom->position));

    cb.supportsReadyRequestItf = destinationInterface;

    // Segments are done being built
    if (cb.supportInitialized // If not itinialized, wait also
        and cb.numCompletedSegments == cb.expectedSegments.size()) {
        // Answer right away
        cb.sendMessage(new SupportReadyResponse(cb.expectedSegments.size() > 0),
                       cb.supportsReadyRequestItf, MSG_DELAY, 0);
        cb.supportsReadyRequestItf = nullptr;
    } // else, wait for them to complete and send reply only then
}

void SupportReadyResponse::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);

    // Once segmentDetected is set to true, it cannot be unset
    cb.segmentsDetected = cb.segmentsDetected or hasSegments;

    // Forward message further along the border
    Cell3DPosition next = cb.findNextCoatingPositionOnLayer(cb.supportsReadyBlacklist);

    P2PNetworkInterface* nextItf = cb.catom->getInterface(next);
    VS_ASSERT(nextItf != nullptr and nextItf->isConnected());
    cb.sendMessage(new NextPlaneSupportsReadyMessage(cb.segmentsDetected), nextItf,
                   MSG_DELAY, 0);
}