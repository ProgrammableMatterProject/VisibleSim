/**
 * @file   coatingMessages.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Thu Oct 10 10:08:22 2019
 *
 * @brief
 *
 *
 */

#include <iostream>
#include <sstream>

#include "utils.h"

#include "teleportationEvents.h"
#include "rotation3DEvents.h"

#include "coatingRuleMatcher.hpp"
#include "coatingBlockCode.hpp"
#include "coatingMessages.hpp"

void CoaTrainRequest::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);

    if (++mabc.spawnCount > mabc.getResourcesForCoatingLayer(mabc.currentLayer)) {
        // if (mabc.shapeRequiresL1Support and mabc.spawnCount == 1)
        //     mabc.spawnCount--; // FIXME: regularize

        mabc.sendMessage(new CoaTrainIsFull(), destinationInterface, MSG_DELAY_MC, 0);
        mabc.catom->setColor(RED);
    } else {
        mabc.sendMessage(new GetOnBoard(mabc.currentLayer, mabc.useExternalCoatingOnOddLayers),
                         destinationInterface, MSG_DELAY_MC, 0);
    }
}

void GetOnBoard::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);

    mabc.currentLayer = layer;
    mabc.useExternalCoatingOnOddLayers = useExtCoating;

    mabc.initializeClosingCornerAndFBPLocations(mabc.closingCorner, mabc.firstBorderPos);

    mabc.scheduleRotationTo(mabc.nextRotationTowards(mabc.trainStart // Prioritize right
                                                     + Cell3DPosition(10,0,0)));
}

void CoaTrainIsFull::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);

    // Do nothing, just wait.
    (void)mabc;
}

void ProceedToNextLayer::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);

    if (mabc.catom->position != mabc.spawnPivot) {
        mabc.forwardPTNLToSpawnPivot();
        mabc.passNextSpawnRound = true;
    } else {
        mabc.currentLayer++;
        mabc.spawnCount = 0;
        mabc.catom->setColor(GREEN);
        if (mabc.getResourcesForCoatingLayer(mabc.currentLayer) > 0) {
            if (mabc.currentLayer == 1
                and mabc.useExternalCoatingOnOddLayers
                and not mabc.isInCSG(mabc.cornerTilePos)) {
                mabc.shapeRequiresL1Support = true;
                // introduce a support catom for accessing layer offset odd layer #1,
                //  which would be impossible without it
                static const Cell3DPosition& l1oSupport = Cell3DPosition(4,2,3);
                mabc.sendMessage(new HeadToSupportLocation(l1oSupport),
                                 mabc.catom->getInterface(mabc.catom->position
                                                     + GetOnBoard::defaultDst),
                                 MSG_DELAY_MC, 0);
            } else {
                mabc.sendMessage(new GetOnBoard(mabc.currentLayer,
                                                mabc.useExternalCoatingOnOddLayers),
                                 mabc.catom->getInterface(mabc.catom->position
                                                          + GetOnBoard::defaultDst),
                                 MSG_DELAY_MC, 0);
                mabc.spawnCount++;
            }

        } else {
            mabc.sendMessage(new CoaTrainIsFull(),
                             mabc.catom->getInterface(mabc.catom->position
                                                      + GetOnBoard::defaultDst),
                             MSG_DELAY_MC, 0);
            mabc.catom->setColor(RED);
            mabc.coatingIsOver = true;
        }
    }
}

void HeadToSupportLocation::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);

    mabc.calledInToSupportLocation = true;
    mabc.supportLocation = tPos;
    mabc.scheduleRotationTo(mabc.nextRotationTowards(mabc.supportLocation));
}

void ProbePivotLightStateMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);
    (void)mabc;
}

void GreenLightIsOnMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);
    (void)mabc;
}

void FinalTargetReachedMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);

    (void)mabc;
}
