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

void ProbeMotionValidityMessage::
forwardToFAOrReturnClearForMotion(BaseSimulator::BlockCode *bc) const {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);
    
    //// check for potential modules to forward to
    // vector<Cell3DPosition> nCells = mabc.lattice->
    //     getActiveNeighborCells(mabc.catom->position);
    // for (const Cell3DPosition& nCell : nCells) {
    //     if (not mabc.ruleMatcher->isInMesh(nCell)) { // Module must be FA
    //         //// if FA module docked on pivot, forward message
    //         P2PNetworkInterface* FAItf =
    //             mabc.catom->getInterface(mabc.catom->position);
    //         // mabc.sendmessage(FAItf,...)
    //         return;
    //     }
    // }
    //// else: no precedence, return ClearForMotion
    // mabc.sendMessage(sourceInterface,...)
}

void ProbeMotionValidityMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    // The goal here is to reach the moving module preceding the sender if it exists.
    // 1. If it indeed exists, there is nothing to do but wait for it to send a message
    //    to sender when it initiates a new motion
    // 2. If it does not exist, then the sender is clear for motion, and thus it should
    //    be notified that it is the case by modules further down its path
    
    // // TODO:
    // if (mabc.lattice->cellsAreAdjacent(mabc.catom->position, sender)) {
    //     // Two modules moving with always one gap between them cannot both be neighbors of the same pivot, hence here the course of action is to forward the message further down the path
    //     // The next module can either be:
    //     // a. branch module  (regular case, if current module is branch or support)
    //     // b. support module (if current module is vertical branch tip)
    //     // c. tile root      (if current module is horizontal branch tip,
    //     //                    in which case the sender is clear for motion)

    //     P2PNetworkInterface* nextItf;
    //     if (mabc.ruleMatcher->isZBranchModule(mabc.norm(mabc.catom->position))

    //     VS_ASSERT_MSG(nextItf, "cannot find next module along path");
    //     mabc.sendMessage(this->clone(), nextItf, MSG_DELAY_MC, 0);        
    // } else {
        
    // }
    
    // // if pivot and is tip,
    // if (mabc.ruleMatcher->isZBranchModule(mabc.norm(mabc.catom->position))) {

    //     if (mabc.ruleMatcher->isVerticalBranchTip(mabc.norm(mabc.catom->position))) {
    //         //// forward to support
    //     } else {
    //         // else pivot and is connected to sender,
    //         if (mabc.lattice->cellsAreAdjacent(mabc.catom->position, sender)) {
    //             //// send to next pivot along branch
    //         } else {
    //             forwardToFAOrReturnClearForMotion(mabc);
    //         }
    //     }
    
    // // else module is Support
    // } else if (mabc.ruleMatcher->isTileSupport(mabc.norm(mabc.catom->position))) {
    //     // else pivot and is connected to sender,
    //     if (mabc.lattice->cellsAreAdjacent(mabc.catom->position, sender)) {
    //         //// send to next pivot along branch
    //     } else {
    //         forwardToFAOrReturnClearForMotion(mabc);
    //     }
    // } else if (mabc.role == FreeAgent) { // this is the target module
    //     //// wait until motion is scheduled to respond ClearForMotion        
    // }
}

void ClearForMotionMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    // TODO:
    // if support,
    //// forward to branch tip
    // else if pivot and not connected to receiver
    //// forward to next module in branch
    // else if pivot and connected to receiver
    //// forward to receiver
    // else if receiver
    //// check motion rules and perform next move
}
