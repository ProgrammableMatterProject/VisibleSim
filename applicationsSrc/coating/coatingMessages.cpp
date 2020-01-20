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

    cb.catom->setColor(YELLOW);

    // Not a support module, forward message along the segment
    if (cb.expectedSegments.count(sourceInterface->hostBlock->position)
        and ++cb.numCompletedSegments == cb.expectedSegments.size()) {
        if (not cb.isSupportPosition(cb.catom->position)) {
            cb.notifyAttracterOfSegmentCompletion(destinationInterface);
        } else {
            cb.catom->setColor(GREY);
            unsigned int layer = cb.getGLayer(cb.catom->position);

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

    if (cb.isSeedPosition(cb.catom->position)) {
        const Cell3DPosition& firstPos = cb.getStartPositionAboveSeed(cb.catom->position);

        // Attract module above seed, or start border following depending on segmentsDetected
        if (segmentsDetected) {
            // Check if module right above is in place
            if (cb.lattice->isFree(firstPos)) {
                // Mark for border completion and attract
                cb.borderCompleted.insert(firstPos);
                cb.completionNeighbors.insert(firstPos);
                cb.nextBorderItf = cb.catom->getInterface(firstPos);
                cb.sendAttractSignalTo(firstPos);
            } else {
                cb.startBorderCompletionAlgorithm();
            }
        } else {
            VS_ASSERT(cb.lattice->isFree(firstPos));
            cb.sendAttractSignalTo(firstPos);
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
        const Cell3DPosition& sender = sourceInterface->hostBlock->position;
        Cell3DPosition next = cb.findNextCoatingPositionOnLayer(cb.supportsReadyBlacklist);

        P2PNetworkInterface* nextItf = cb.catom->getInterface(next);
        VS_ASSERT(nextItf != nullptr and nextItf->isConnected());
        cb.sendMessage(new NextPlaneSupportsReadyMessage(segmentsDetected),
                       nextItf, MSG_DELAY, 0);

        cb.supportsReadyBlacklist.insert(next);
    } else {
        // cb.lastBorderFollowingPosition = sourceInterface->hostBlock->position;
        VS_ASSERT(supportItf != nullptr and supportItf->isConnected());
        cb.sendMessage(new SupportReadyRequest(), supportItf, MSG_DELAY, 0);
    }
}

void SupportReadyRequest::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);

    VS_ASSERT(cb.isSupportPosition(cb.catom->position));

    cb.supportReadyRequestItf = destinationInterface;

    // Segments are done being built
    if (cb.numCompletedSegments == cb.expectedSegments.size()) {
        // Answer right away
        cb.sendMessage(new SupportReadyResponse(cb.expectedSegments.size() > 0),
                       cb.supportReadyRequestItf, MSG_DELAY, 0);
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
