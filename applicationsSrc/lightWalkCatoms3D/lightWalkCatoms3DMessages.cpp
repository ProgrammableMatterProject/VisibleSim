/**
 * @file   lightWalkMessages.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Mon Dec 10 15:32:56 2018
 *
 * @brief
 *
 *
 */

#include <iostream>
#include <sstream>

#include "utils/utils.h"
#include "robots/catoms3D/catoms3DRotationEvents.h"

#include "lightWalkCatoms3DBlockCode.hpp"
#include "lightWalkCatoms3DMessages.hpp"


void ProbePivotLightStateMessage::handle(BaseSimulator::BlockCode* bc) {
    LightWalkCatoms3DBlockCode& mabc = *static_cast<LightWalkCatoms3DBlockCode*>(bc);

    if (mabc.catom->position[2] == mabc.ZLINE) { // module is pivot
        bool nextToSender = mabc.isAdjacentToPosition(srcPos);
        bool nextToTarget = mabc.isAdjacentToPosition(targetPos);
        Catoms3DBlock* targetLightNeighbor = mabc.findTargetLightAmongNeighbors(targetPos);

        cout << *mabc.catom << " received " << getName() << endl;
        cout << "\tnextToSender: " << nextToSender << endl;
        cout << "\tnextToTarget: " << nextToTarget << endl;
        cout << "\ttargetLightNeighbor: " << (targetLightNeighbor ?
                                              targetLightNeighbor->position.to_string()
                                              : "NULL") << endl;

        if (targetLightNeighbor
            and targetLightNeighbor->position != srcPos) { // neighbor is target light
            mabc.sendMessage(this->clone(),
                             mabc.catom->getInterface(targetLightNeighbor->position),
                             MSG_DELAY_MC, 0);
        } else if (not targetLightNeighbor and nextToTarget) { // module is target light
            if (mabc.greenLightIsOn or (mabc.isLineTip() and nextToSender)) {
                P2PNetworkInterface* itf = nextToSender ?
                    mabc.catom->getInterface(srcPos) :
                    mabc.catom->getInterface(mabc.catom->position + Cell3DPosition(-1, 0, 0));
                VS_ASSERT(itf);
                mabc.sendMessage(new GreenLightIsOnMessage(mabc.catom->position, srcPos),
                                 itf, MSG_DELAY_MC, 0);
            } else {
                // Catom will be notified when light turns green
                // NOTE: Should we rather notify just when needed, or send a message anyway
                //  to the previous pivot?
                mabc.moduleAwaitingGo = true;
                mabc.awaitingModulePos = srcPos;
                mabc.catom->setColor(ORANGE);
            }
        } else { // not neighborNextToTarget and not nextToSender
            mabc.catom->setColor(BLACK);
            VS_ASSERT_MSG(false, "error: not neighborNextToTarget and not nextToSender");
        }
    } else { // module is in motion (thus should not receive such message)
        mabc.catom->setColor(BLACK);
        VS_ASSERT(false);
    }
}

void GreenLightIsOnMessage::handle(BaseSimulator::BlockCode* bc) {
    LightWalkCatoms3DBlockCode& mabc = *static_cast<LightWalkCatoms3DBlockCode*>(bc);

    if (mabc.catom->position[2] == mabc.ZLINE
        and (mabc.hasLeftNeighbor() or not mabc.greenLightIsOn)) { // module is pivot
        bool nextToDest = mabc.isAdjacentToPosition(dstPos);

        P2PNetworkInterface* itf = nextToDest ?
            mabc.catom->getInterface(dstPos) :
            mabc.catom->getInterface(mabc.catom->position + Cell3DPosition(-1, 0, 0));

        mabc.greenLightIsOn = false;
        mabc.catom->setColor(RED);
        mabc.sendMessage(this->clone(), itf, MSG_DELAY_MC, 0);
    } else { // module is target
        VS_ASSERT(mabc.catom->position == dstPos);

        // Perform pending motion
        mabc.rotating = true;
        mabc.scheduler->schedule(
            new Catoms3DRotationStartEvent(getScheduler()->now(), mabc.catom, mabc.targetPos));
    }
}
