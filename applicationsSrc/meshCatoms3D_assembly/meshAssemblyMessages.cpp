/**
 * @file   messages.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 14:13:13 2018
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

#include "meshRuleMatcher.hpp"
#include "meshAssemblyBlockCode.hpp"
#include "meshAssemblyMessages.hpp"
#include "meshAssemblyLocalRules.hpp"


void RequestTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);    
    // cout << "[t-" << getScheduler()->now() << "] received request target cell" << endl;
    
    if (mabc.role == ActiveBeamTip) {
        // Forward message to coordinator
        P2PNetworkInterface* coordItf =
            mabc.catom->getInterface(mabc.coordinatorPos);
        VS_ASSERT_MSG(coordItf, "cannot find coordinator among neighbor interfaces");
        mabc.sendMessage(this->clone(), coordItf, MSG_DELAY_MC, 0);
        mabc.log_send_message();
    } else if (mabc.role == Support) {
        // Forward message to ActiveBeamTip for forwarding to root
        P2PNetworkInterface* btItf =
            mabc.catom->getInterface(mabc.branchTipPos);
        VS_ASSERT_MSG(btItf, "cannot find branch tip among neighbor interfaces");
        mabc.sendMessage(this->clone(), btItf, MSG_DELAY_MC, 0);
        mabc.log_send_message();
    } else if (mabc.role == Coordinator) {
        short idx = mabc.getEntryPointLocationForCell(srcPos); VS_ASSERT(idx != -1);
        MeshComponent epl = static_cast<MeshComponent>(idx);

        Cell3DPosition tPos;
        tPos = mabc.catom->position + mabc.getNextTargetForEPL(epl);                
        
        // send to requesting catom
        VS_ASSERT(destinationInterface->isConnected());
        mabc.sendMessage(new ProvideTargetCellMessage(tPos, srcPos),
                         destinationInterface, MSG_DELAY_MC, 0);
        mabc.log_send_message();
    } else {
        mabc.catom->setColor(BLACK);
        VS_ASSERT_MSG(false, "Non coordinator or active beam module should not have received a RequestTargetCellMessage");
    }
}

void ProvideTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    if (mabc.role == ActiveBeamTip or mabc.role == Support) {
        // cout << mabc.catom->blockId << "    " <<
        //     mabc.derelatify(mabc.ruleMatcher->getSupportPositionForPosition(mabc.norm(mabc.catom->position))) << endl;
        // Forward message to mobile module or support depending on case
        P2PNetworkInterface* itf =
            mabc.catom->getInterface(dstPos) ?: mabc.catom->getInterface(
                mabc.derelatify(mabc.ruleMatcher->getSupportPositionForPosition(
                                    mabc.norm(mabc.catom->position))));  
        VS_ASSERT_MSG(itf, "cannot find neither dest or support among neighbor interfaces");
        mabc.sendMessage(this->clone(), itf, MSG_DELAY_MC, 0);
        mabc.log_send_message();
    } else {
        mabc.targetPosition = tPos;
        // cout << "Target position for #" << mabc.catom->blockId << " is " << tPos << endl;
    
        if (tPos == mabc.catom->position) {
            mabc.role = mabc.ruleMatcher->getRoleForPosition(mabc.norm(mabc.catom->position));
            mabc.catom->setColor(mabc.ruleMatcher->getColorForPosition(
                                     mabc.norm(mabc.catom->position)));
        } else {
            Cell3DPosition nextHop;
            bool matched = matchLocalRules(mabc.catom->getLocalNeighborhoodState(),
                                           mabc.catom->position,
                                           mabc.targetPosition,
                                           mabc.coordinatorPos, mabc.step, nextHop);
            if (not matched) {
                mabc.catom->setColor(RED);
                cout << "#" << mabc.catom->blockId << endl;
                VS_ASSERT_MSG(matched, "DID NOT FIND RULE TO MATCH.");
            }
    
            mabc.scheduleRotationTo(nextHop);
        }
    }    
}

void ProbePivotLightStateMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);    
    
    if (mabc.role != FreeAgent) { // module is pivot
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
            if (mabc.greenLightIsOn or nextToSender) {
                Cell3DPosition nextHopPos;
                nextHopPos = nextToSender ? srcPos
                    : srcPos; // TODO
                
                P2PNetworkInterface* itf =  mabc.catom->getInterface(nextHopPos);
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
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    if (mabc.role != FreeAgent) { // module is pivot
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
            new Rotation3DStartEvent(getScheduler()->now(), mabc.catom, mabc.stepTargetPos));
    }
}
