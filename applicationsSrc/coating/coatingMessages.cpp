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
        mabc.sendMessage(new CoaTrainIsFull(), destinationInterface, MSG_DELAY_MC, 0);
        mabc.catom->setColor(RED);
    } else {
        mabc.sendMessage(new GetOnBoard(mabc.currentLayer),
                         destinationInterface, MSG_DELAY_MC, 0);
    }
}

void GetOnBoard::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);

    mabc.currentLayer = layer;

    if (layer == 0)
        mabc.scheduleRotationTo(mabc.catom->position + Cell3DPosition(1,0,0));
    else mabc.scheduleRotationTo(mabc.catom->position + Cell3DPosition(-1,-1,2));
}

void CoaTrainIsFull::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);

    // Do nothing, just wait.
    (void)mabc;
}

void ProceedToNextLayer::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);

    if (mabc.catom->position != mabc.spawnPivot) {
        P2PNetworkInterface* PTNL_itf = mabc.catom->getInterface(mabc.spawnPivot);
        // TODO: if (PTNL_itf == NULL)
        mabc.sendMessage(new ProceedToNextLayer(), PTNL_itf, MSG_DELAY_MC, 0);
    } else {
        mabc.currentLayer++;
        mabc.spawnCount = 1;
        mabc.catom->setColor(GREEN);
        mabc.sendMessage(new GetOnBoard(mabc.currentLayer),
                         mabc.catom->getInterface(mabc.catom->position+GetOnBoard::defaultDst),
                         MSG_DELAY_MC, 0);
    }
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
