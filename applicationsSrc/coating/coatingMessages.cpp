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
                        static_cast<CoatingBlockCode*>(
                            cb.lattice->getBlock(cb.G_SEED_POS)->blockCode)
                            ->handleBorderCompletion();
                    }
                } else {
                    cb.attractPlane(layer);
                }
            }
        }
    }
}

void BorderCompletionMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);

    if (cb.handledBorderCompletion) return; // Algorithm over for border, we went full circle

    const Cell3DPosition& sender = sourceInterface->hostBlock->position;
    cb.handleBorderCompletion(sender);
}

void NextPlaneSegmentDetectionMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& cb = *static_cast<CoatingBlockCode*>(bc);
}
