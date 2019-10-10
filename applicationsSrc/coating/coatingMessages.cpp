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

    mabc.scheduleRotationTo(mabc.catom->position + Cell3DPosition(1,0,0));
}

void CoaTrainIsFull::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);

    // Do nothing, just wait.
    (void)mabc;
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
