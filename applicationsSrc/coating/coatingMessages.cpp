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
    if (not cb.isSupportPosition(cb.catom->position)) {
        P2PNetworkInterface* prev = cb.catom->getInterface(
            cb.lattice->getOppositeDirection(cb.catom->getDirection(destinationInterface)));

        VS_ASSERT(prev != nullptr and prev->isConnected());

        cb.sendMessage(this->clone(), prev, MSG_DELAY, 0);
    } else {
        if (cb.expectedSegments.count(sourceInterface->hostBlock->position)
            and ++cb.numCompletedSegments == cb.expectedSegments.size()) {

            cb.catom->setColor(GREY);
            unsigned int layer = cb.getGLayer(cb.catom->position);

            if (++cb.planeSupportsReady[layer] == cb.planeSupports[layer].size()) {
                // for (const Cell3DPosition& seed : cb.planeSeed[layer]) {
                //     CoatingBlockCode* seedCb = static_cast<CoatingBlockCode*>(
                //         cb.lattice->getBlock(seed)->blockCode);

                // call some init function
                cb.attractPlane(layer);

                // }
            }
        }
    }
}

void BorderCompletionMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);

    if (cb.handledBorderCompletion) return; // Algorithm over for border, we went full circle

    const Cell3DPosition& sender = sourceInterface->hostBlock->position;

    // Get the next border position
    // FIXME: There should be only one for now but this won't last with planar cases
    Cell3DPosition next = cb.findNextCoatingPositionOnLayer(sender);
    VS_ASSERT(next != Cell3DPosition(-1,-1,-1));

    if (cb.lattice->isFree(next)) {
        // Module has to be attracted, do it and start monitoring its location
        //  so as to be ready to send it the message when it connects
        cb.expectingCompletionNeighbor = true;
        cb.completionNeighborPos = next;
        cb.sendAttractSignalTo(next);
    } else {
        // Forward message further along the border
        P2PNetworkInterface* nextItf = cb.catom->getInterface(next);
        VS_ASSERT(nextItf != nullptr);
        cb.sendMessage(this->clone(), nextItf, MSG_DELAY, 0);
    }

    cb.handledBorderCompletion = true;
}

void NextPlaneSegmentDetectionMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);
}
