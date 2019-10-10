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

void ProbePivotLightStateMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);
}

void GreenLightIsOnMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);
}

void FinalTargetReachedMessage::handle(BaseSimulator::BlockCode* bc) {
    CoatingBlockCode& mabc = *static_cast<CoatingBlockCode*>(bc);
}
