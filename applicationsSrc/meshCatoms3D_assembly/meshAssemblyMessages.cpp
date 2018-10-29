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

    if (mabc.role == ActiveBeamTip) {
        // Forward message to coordinator
        P2PNetworkInterface* coordItf =
            mabc.catom->getInterface(mabc.coordinatorPos);
        VS_ASSERT_MSG(coordItf, "cannot find coordinator among neighbor interfaces");
        mabc.sendMessage(this->clone(), coordItf, MSG_DELAY_MC, 0);        
    } else if (mabc.role == Support) {
        // Forward message to ActiveBeamTip for forwarding to root
        P2PNetworkInterface* btItf =
            mabc.catom->getInterface(mabc.branchTipPos);
        VS_ASSERT_MSG(btItf, "cannot find branch tip among neighbor interfaces");
        mabc.sendMessage(this->clone(), btItf, MSG_DELAY_MC, 0);
    } else if (mabc.role == Coordinator) {
        short idx = mabc.getEntryPointLocationForCell(srcPos); VS_ASSERT(idx != -1);
        MeshComponent epl = static_cast<MeshComponent>(idx);

        Cell3DPosition tPos;
        tPos = mabc.catom->position + mabc.getNextTargetForEPL(epl);        

        cout << "Spawnee Position: " << srcPos 
             << " -- Target Position: " << tPos
             << " -- epl Position: " << srcPos - mabc.catom->position 
             << " -- epl: " << epl << endl;
    
        // send to requesting catom
        VS_ASSERT(destinationInterface->isConnected());
        mabc.sendMessage(new ProvideTargetCellMessage(tPos, srcPos),
                         destinationInterface, MSG_DELAY_MC, 0);
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

void TileInsertionReadyMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    if (mabc.role == ActiveBeamTip) {
        static constexpr Cell3DPosition REL_X_POS = Cell3DPosition(-1, 0, 1);
        static constexpr Cell3DPosition REL_Y_POS = Cell3DPosition(0, -1, 1);
            
        // Forward to opposite horizontal branch tip
        P2PNetworkInterface* itf = mabc.catom->getInterface(
            mabc.catom->position + REL_X_POS) == destinationInterface ?
            mabc.catom->getInterface(mabc.catom->position + REL_Y_POS)
            : mabc.catom->getInterface(mabc.catom->position + REL_X_POS);

        if (proceed) {
            if (mabc.tileInsertionAckGiven) return; // too late, block it
            else mabc.tileInsertionAckGiven = true;
        }
        
        if (itf and itf->isConnected())
            mabc.sendMessage(this->clone(), itf, MSG_DELAY_MC, 0);
        else if (mabc.ruleMatcher->isOnXBorder(mabc.norm(mabc.coordinatorPos)) or
                 mabc.ruleMatcher->isOnYBorder(mabc.norm(mabc.coordinatorPos)))
            // no need to wait
            mabc.sendMessage(new TileInsertionReadyMessage(true), destinationInterface,
                             MSG_DELAY_MC, 0);   
    } else {
        if (proceed) {
            BranchIndex bi = 
                mabc.ruleMatcher->getBranchIndexForNonRootPosition(
                    mabc.norm(mabc.targetPosition));
            const Cell3DPosition& nextPosAlongBranch =
                mabc.catom->position + mabc.ruleMatcher->getBranchUnitOffset(bi);
                    
            mabc.lattice->unhighlightCell(nextPosAlongBranch);
            cout << "Ready to insert tile root at " << nextPosAlongBranch
                 << endl;

            // Update coordinate system to the one of the next tile
            mabc.coordinatorPos =
                mabc.denorm(mabc.ruleMatcher->
                       getNearestTileRootPosition(mabc.norm(mabc.catom->position)));
                            
            mabc.world->addBlock(0, mabc.buildNewBlockCode,
                                 mabc.getEntryPointForMeshComponent(R), CYAN);
        } else if (not mabc.tileInsertionAckGiven) {
            // Tell sender to proceed with TR insertion            
            mabc.sendMessage(new TileInsertionReadyMessage(true), destinationInterface,
                             MSG_DELAY_MC, 0);
            mabc.tileInsertionAckGiven = true;
        } // otherwise do nothing
    }
}
