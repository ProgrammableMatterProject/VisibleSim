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
