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
        if (coordItf and coordItf->isConnected()) {
            mabc.sendMessage(this->clone(), coordItf, MSG_DELAY_MC, 0);
            mabc.sentRequestToCoordinator = true;
        } else return; // Wait for TR to get into place and send COORDINATOR_READY
    } else if (mabc.role == Support) {
        // Forward message to ActiveBeamTip for forwarding to root
        P2PNetworkInterface* btItf =
            mabc.catom->getInterface(mabc.branchTipPos);
        VS_ASSERT_MSG(btItf, "cannot find branch tip among neighbor interfaces");
        mabc.sendMessage(this->clone(), btItf, MSG_DELAY_MC, 0);
    } else if (mabc.ruleMatcher->isNFromVerticalBranchTip(mabc.norm(mabc.catom->position), 1)) {
        // Forward up the current branch to vbranch tip
        // cout << "catom : " << mabc.catom->position << endl;
        // cout << "norm : " << mabc.norm(mabc.catom->position) << endl;
        Cell3DPosition pos = mabc.catom->position +
            mabc.ruleMatcher->getBranchUnitOffset(
                mabc.getBranchIndex(mabc.catom->position)); // 00B cuz z<0

        // cout << "tip : " << pos << endl;

        P2PNetworkInterface* btipItf = mabc.catom->getInterface(pos);

        VS_ASSERT_MSG(btipItf, "cannot find branch tip among neighbor interfaces");
        mabc.sendMessage(this->clone(), btipItf, MSG_DELAY_MC, 0);
    } else if (mabc.role == Coordinator) {
        // Prevent processing double sends of request
        if (mabc.processedRQId.find(srcId) != mabc.processedRQId.end()) return;
        else mabc.processedRQId.insert(srcId);

        short idx = mabc.getEntryPointLocationForCell(srcPos); VS_ASSERT(idx != -1);
        MeshComponent epl = static_cast<MeshComponent>(idx);
        BranchIndex bi = MeshRuleMatcher::getBranchForEPL(epl); VS_ASSERT(bi < 4);
        Cell3DPosition tPos;

        if (not mabc.constructionQueue.empty() and mabc.catomsReqByBranch[bi] != 0) {
            pair<MeshComponent, MeshComponent> nextComponent = mabc.constructionQueue.front();

            // If on the righ EPL, module is eligible for building next component
            if (epl == nextComponent.second) {
                // Return correct target, then check status of each waiting module
                tPos = mabc.catom->position
                    + MeshRuleMatcher::getPositionForMeshComponent(nextComponent.first);

                // Update construction plan if nextComponent is branch component
                int ncBi =MeshRuleMatcher::getBranchIndexForMeshComponent(nextComponent.first);
                if (ncBi != -1) mabc.catomsReqByBranch[ncBi]--;

                // Update queue
                mabc.constructionQueue.pop_front();
            } else { // Not the right EPL, note that a module is waiting there
                // Messages or sometimes sent twice due to concurrency issues in
                //  communications. A RQ could be getting forwarded while the TR
                //  gets in place, which would cause it to send COORDINATOR_READY
                //  before receiving RQ_TARGET_CELL. Causing a double send of
                //  RQ_TARGET_CELL by the requesting module. It could just
                //  stay that way for now and label RQ messages with the module
                //  id so as to avoid processing the message twice, I cannot see
                //  a better solution for now.
                // VS_ASSERT(not mabc.moduleWaitingOnBranch[bi]);
                mabc.moduleWaitingOnBranch[bi] = true;
                return;
            }
        } else {
            // Else redirect to EPL corresponding to that branch
            // ONLY IF BRANCH HAD TO BE GROWN!
            if (mabc.catomsReqByBranch[bi] == -1) return;

            tPos = mabc.catom->position + MeshRuleMatcher::getTargetEPLPositionForBranch(bi);
        }

        // Send to requesting catom
        VS_ASSERT(destinationInterface->isConnected());
        mabc.sendMessage(new ProvideTargetCellMessage(tPos, srcPos),
                         destinationInterface, MSG_DELAY_MC, 0);

        // UPDATE WAITING CATOMS
        // Loop while new provide_target_cell messages are being sent
        bool moduleAwoken;
        do {
            moduleAwoken = false;
            for (int i = 0; i < 4; i++) {
                BranchIndex biw = static_cast<BranchIndex>(i);
                if (mabc.moduleWaitingOnBranch[biw]) {
                    pair<MeshComponent, MeshComponent> ncp = mabc.constructionQueue.front();
                    MeshComponent epl = MeshRuleMatcher::getDefaultEPLComponentForBranch(biw);
                    if (epl == ncp.second) {
                        tPos = mabc.catom->position +
                            MeshRuleMatcher::getPositionForMeshComponent(ncp.first);

                        // Determine position of waiting module
                        Cell3DPosition wPos = mabc.catom->position +
                            MeshRuleMatcher::getPositionForMeshComponent(epl);

                        // Determine branch tip pos and itf of waiting module branch
                        Cell3DPosition tipPos = mabc.catom->position +
                            MeshRuleMatcher::getPositionOfBranchTipUnder(biw);
                        P2PNetworkInterface* tipItf = mabc.catom->getInterface(tipPos);

                        // Update construction plan if nextComponent is branch component
                        int ncBi = MeshRuleMatcher::
                            getBranchIndexForMeshComponent(ncp.first);
                        if (ncBi != -1) mabc.catomsReqByBranch[ncBi]--;

                        // Send
                        VS_ASSERT(tipItf and tipItf->isConnected());
                        mabc.sendMessage(new ProvideTargetCellMessage(tPos, wPos),
                                         tipItf, MSG_DELAY_MC, 0);

                        // Update looping condition and waiting state
                        mabc.moduleWaitingOnBranch[biw] = false;
                        moduleAwoken = true;
                        mabc.constructionQueue.pop_front();
                    }
                }
            }
        } while (moduleAwoken);
    } else {
        mabc.catom->setColor(BLACK);
        VS_ASSERT_MSG(false, "Non coordinator or active beam module should not have received a RequestTargetCellMessage");
    }

    mabc.log_send_message();
}

void ProvideTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    if (mabc.role == ActiveBeamTip
        or mabc.role == Support
        or mabc.role == PassiveBeam) {
        // cout << mabc.catom->blockId << "    " <<
        //     mabc.derelatify(mabc.ruleMatcher->getSupportPositionForPosition(mabc.norm(mabc.catom->position))) << endl;
        // Forward message to mobile module or support depending on case
        P2PNetworkInterface* itf = NULL;

        P2PNetworkInterface* dstItf = mabc.catom->getInterface(dstPos);
        Cell3DPosition supportPos = mabc.derelatify(mabc.ruleMatcher->getSupportPositionForPosition(mabc.norm(mabc.catom->position)));
        P2PNetworkInterface* supportItf = mabc.catom->getInterface(supportPos);

        if (dstItf and dstItf->isConnected()) itf = dstItf;
        else if (supportItf and supportItf->isConnected()) itf = supportItf;
        else if (mabc.role == ActiveBeamTip) { // send down branch
            Cell3DPosition pos = mabc.catom->position -
                mabc.ruleMatcher->getBranchUnitOffset(
                    mabc.getBranchIndex(mabc.catom->position));
            itf = mabc.catom->getInterface(pos);
        }

        VS_ASSERT_MSG(itf, "cannot find neither dest, support, or 2-branch module among neighbor interfaces");
        mabc.sendMessage(this->clone(), itf, MSG_DELAY_MC, 0);
        mabc.log_send_message();
    } else {
        mabc.targetPosition = tPos;
        // cout << "Target position for #" << mabc.catom->blockId << " is " << tPos << endl;

        mabc.matchRulesAndProbeGreenLight();
    }
}

void CoordinatorReadyMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);
    Cell3DPosition dstPos = Cell3DPosition();
    
    if (mabc.role != FreeAgent) {
        dstPos = mabc.getEntryPointForModuleOnIncidentBranch(mabc.branch)
        + (mabc.denorm(mabc.ruleMatcher->getNearestTileRootPosition(mabc.catom->position))[2]
           ==
           mabc.meshSeedPosition[2] ?
           Cell3DPosition(0,0,0) :
           // If module not part of sandbox, use next tile cooridnates as reference
           //  instead of own tile. This change is necessary due to
           //  getEntryPointForModuleOnIncidentBranch using own tile as reference.
           mabc.B * mabc.ruleMatcher->getBranchUnitOffset(mabc.branch));
    }
    // cout << "branch: " <<mabc.branch << endl;
    // cout << "catom: " << mabc.catom->position << endl;
    // cout << "dstPos: " << dstPos << endl;
    // cout << "_dstPos: " << _dstPos << endl;
    // cout << "role: " << mabc.role << endl;

    if (mabc.role == ActiveBeamTip or mabc.role == Support) {
        P2PNetworkInterface* itf =
            mabc.catom->getInterface(dstPos) ?: mabc.catom->getInterface(
                mabc.derelatify(mabc.ruleMatcher->getSupportPositionForPosition(
                                    mabc.norm(mabc.catom->position))));

        VS_ASSERT(itf);
        if (not itf->isConnected()) {
            // Beam Tip and no Support nor waiting module, forward to next branch module
            Cell3DPosition pos = mabc.catom->position -
                mabc.ruleMatcher->getBranchUnitOffset(mabc.branch);

            itf = mabc.catom->getInterface(pos);
            VS_ASSERT_MSG(itf, "Cannot find tip2 among neighbor interface");
        }

        if (mabc.role != ActiveBeamTip or not mabc.sentRequestToCoordinator) {
            VS_ASSERT(itf and itf->isConnected());
            mabc.sendMessage(this->clone(), itf, MSG_DELAY_MC, 0);
            mabc.log_send_message();
        }
    } else if (mabc.ruleMatcher->isNFromVerticalBranchTip(mabc.norm(mabc.catom->position), 1)){
        // Forward to waiting module
        P2PNetworkInterface* itf = mabc.catom->getInterface(dstPos);
        VS_ASSERT_MSG(itf, "cannot find dest among neighbor interfaces");

        if (itf->isConnected()) {
            mabc.sendMessage(this->clone(), itf, MSG_DELAY_MC, 0);
            mabc.log_send_message();
        }
    } else {
        // Module is free agent waiting for something to do
        VS_ASSERT(mabc.role == FreeAgent);

        // Resend RequestTargetCell
        VS_ASSERT(mabc.requestTargetCellFromTileRoot());
    }
}

void TileInsertionReadyMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    Cell3DPosition relNeighborPos;
    if (mabc.role == ActiveBeamTip) {
        if (mabc.ruleMatcher->isOnZBranch(mabc.norm(mabc.catom->position))) {
            // Forward to incoming LZ tip
            relNeighborPos = Cell3DPosition(1,0,0);
        } else if (mabc.ruleMatcher->isOnRZBranch(mabc.norm(mabc.catom->position))) {
            // forward to incoming RevZ tip
            relNeighborPos = Cell3DPosition(1,0,0);
        } else if (mabc.ruleMatcher->isOnLZBranch(mabc.norm(mabc.catom->position))) {
            // forward to RevZ tip
            relNeighborPos = Cell3DPosition(0,1,0);
        } else if (mabc.ruleMatcher->isOnRevZBranch(mabc.norm(mabc.catom->position))) {
            // forward to RevZ EPL Pivot
            relNeighborPos = Cell3DPosition(1,1,-1);
        }

        P2PNetworkInterface* itf = mabc.catom->getInterface(mabc.catom->position
                                                            + relNeighborPos);
        VS_ASSERT(itf and itf->isConnected());
        mabc.sendMessage(new TileInsertionReadyMessage(), itf,MSG_DELAY_MC, 0);
        mabc.log_send_message();
    } else if (mabc.ruleMatcher->isNFromVerticalBranchTip(mabc.norm(mabc.catom->position), 1)){
        // Forward to module waiting on EPL
        P2PNetworkInterface* EPLItf = mabc.catom->getInterface(mabc.catom->position
                                                               + Cell3DPosition(0,0,1));
        VS_ASSERT(EPLItf);

        if (EPLItf->isConnected())
            mabc.sendMessage(new TileInsertionReadyMessage(), EPLItf,MSG_DELAY_MC, 0);
        else // No module on EPL, wait until a module arrive and notify it
            mabc.tileInsertionPending = true;
    } else {
        // Get moving towards tile root position
        mabc.targetPosition = mabc.coordinatorPos;
        // mabc.lattice->unhighlightCell(mabc.targetPosition);
        mabc.matchRulesAndRotate();
    }
}


void ProbePivotLightStateMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    if (mabc.role != FreeAgent) { // module is pivot
        bool nextToSender = mabc.isAdjacentToPosition(srcPos);
        bool nextToTarget = mabc.isAdjacentToPosition(targetPos);
        Catoms3DBlock* targetLightNeighbor =
            mabc.findTargetLightAmongNeighbors(targetPos, srcPos);

        // cout << *mabc.catom << " received " << getName() << endl;
        // cout << "\tnextToSender: " << nextToSender << endl;
        // cout << "\tnextToTarget: " << nextToTarget << endl;
        // cout << "\ttargetLightNeighbor: " << (targetLightNeighbor ?
        //                                       targetLightNeighbor->position.to_string()
        //                                       : "NULL") << endl;

        if (targetLightNeighbor
            and targetLightNeighbor->position != srcPos) { // neighbor is target light
            P2PNetworkInterface* tlitf = mabc.catom->getInterface(
                targetLightNeighbor->position);

            VS_ASSERT(tlitf and tlitf->isConnected());
            mabc.sendMessage(this->clone(), tlitf, MSG_DELAY_MC, 0);
        } else if (not targetLightNeighbor and nextToTarget) { // module is target light
            // There is a special case where these rules don't work, that's when 
            //  a catom wants to get into a central EPL position when the support is present
            // In that case, the EPL pivot is giving the greenlight whereas it should be the 
            //  support giving it. 
            if (mabc.ruleMatcher->isEPLPivotModule(mabc.norm(mabc.catom->position))
                // If coordinator is in place
                and not mabc.lattice->isFree(mabc.coordinatorPos)
                // Only if targetPos is actual EPL
                and mabc.ruleMatcher->getPositionForComponent(mabc.ruleMatcher->getTargetEPLComponentForBranch(mabc.branch)) == (targetPos - mabc.coordinatorPos)
                // Check if pivot is present and not a FA module in motion
                and (not mabc.lattice->isFree(mabc.catom->position + Cell3DPosition(-1,-1,2))
                     and static_cast<MeshAssemblyBlockCode*>(mabc.lattice->getBlock(mabc.catom->position + Cell3DPosition(-1,-1,2))->blockCode)->role != FreeAgent)) {
                // If thats the case, forward to branch tip, that will then forward to pivot 
                P2PNetworkInterface* tipItf = mabc.catom->getInterface
                    (mabc.catom->position+mabc.ruleMatcher->getBranchUnitOffset(mabc.branch));
                VS_ASSERT(tipItf and tipItf->isConnected());

                mabc.sendMessage(this->clone(), tipItf, MSG_DELAY_MC, 0);
                return;
            }

            // Another special case where these rules don't work is when 
            //  a catom wants to climb from an EPL to the position at (-1,-1,2) over that 
            //  through a shortcut
            if (mabc.ruleMatcher->isSupportModule(mabc.norm(mabc.catom->position))
                // If coordinator is in place
                and not mabc.lattice->isFree(mabc.coordinatorPos)
                // Module above Support is in place too
                and not mabc.lattice->isFree(mabc.catom->position + Cell3DPosition(-1,-1,2))
                // And if targetPos is that position at (-1,-1,2) above Support
                and (targetPos - srcPos) == Cell3DPosition(-1,-1,2)) {
                // If thats the case, forward to ?Z_1, that will then forward to 
                //  light module at ?Z_2
                P2PNetworkInterface* tipItf = mabc.catom->getInterface
                    (mabc.catom->position + 
                     mabc.ruleMatcher->getBranchUnitOffset(mabc.branch));
                VS_ASSERT(tipItf and tipItf->isConnected());

                mabc.sendMessage(this->clone(), tipItf, MSG_DELAY_MC, 0);
                return;
            }
            
            bool targetPosIsR = finalComponent == MeshComponent::R;
            if (targetPosIsR) {
                // Pivots can only grant a claim for the R position once
                //  except if module is directly connected (this is the grant module)
                if (mabc.RModuleRequestedMotion and not nextToSender) { // ignore request
                    stringstream info;
                    info << " denied probe from: " << srcPos;
                    // cerr <<  info.str() << endl;
                    mabc.scheduler->trace(info.str(), mabc.catom->blockId, BLUE);

                    return;
                } else mabc.RModuleRequestedMotion = true;
            }
            
            if (mabc.greenLightIsOn
                // FIXME: When a catom spawns on an EPL, and when
                //  the support already has a module attached to it,
                //  the support might still give the go because they are neighbor.
                //  Perhaps once the support is in place we should consider checking
                //   that the nearby support is green too
                or (nextToSender
                    and mabc.catom->getState() != BuildingBlock::State::ACTUATING)) {

                P2PNetworkInterface* itf = nextToSender ?
                    mabc.catom->getInterface(srcPos) : destinationInterface;
                VS_ASSERT(itf and itf->isConnected());                
                
                mabc.sendMessage(new GreenLightIsOnMessage(mabc.catom->position, srcPos),
                                 itf, MSG_DELAY_MC, 0);                
            } else {
                // Catom will be notified when light turns green
                // NOTE: Should we rather notify just when needed, or send a message anyway
                //  to the previous pivot?
                mabc.moduleAwaitingGo = true;
                mabc.awaitingModulePos = srcPos;
                mabc.awaitingModuleProbeItf = destinationInterface;
                mabc.catom->setColor(DARKORANGE);
            }
        } else { // not neighborNextToTarget and not nextToSender
            mabc.catom->setColor(BLACK);
            VS_ASSERT_MSG(false, "error: not neighborNextToTarget and not nextToSender");
        }
    } else { // module is in motion (thus should not receive such message)
        mabc.catom->setColor(DARKGREY);
        VS_ASSERT(false);
    }
}

void GreenLightIsOnMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    if (mabc.role != FreeAgent) { // module is pivot
        bool nextToDest = mabc.isAdjacentToPosition(dstPos);
        P2PNetworkInterface* itf;

        Cell3DPosition nnCell = Cell3DPosition(0,0,0);
        if (not nextToDest) {
            for (const auto &nCell:mabc.lattice->getActiveNeighborCells(mabc.catom->position)){
                if (mabc.lattice->cellsAreAdjacent(nCell, dstPos)) {
                    nnCell = nCell;
                    continue;
                }
            }
        }

        if (nextToDest) {
            mabc.SET_GREEN_LIGHT(false);
            itf = mabc.catom->getInterface(dstPos);
        } else if (nnCell != Cell3DPosition(0,0,0)) {
            itf = mabc.catom->getInterface(nnCell);
        } else {
            itf = mabc.catom->getInterface(mabc.catom->position -
                                           mabc.ruleMatcher->getBranchUnitOffset(
                                               mabc.getBranchIndex(mabc.catom->position)));
        }

        VS_ASSERT(itf and itf->isConnected());

        mabc.sendMessage(this->clone(), itf, MSG_DELAY_MC, 0);
    } else { // module is target
        VS_ASSERT(mabc.catom->position == dstPos);

        // Perform pending motion
        mabc.rotating = true;
        // Sender should be pivot to be used for next motion
        Catoms3DBlock* pivot = static_cast<Catoms3DBlock*>(sourceInterface->hostBlock);
        VS_ASSERT(pivot and pivot != mabc.catom);
        mabc.scheduleRotationTo(mabc.stepTargetPos, pivot);
    }
}

void FinalTargetReachedMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    VS_ASSERT(mabc.lattice->cellsAreAdjacent(mabc.catom->position, finalPos));
    if (not mabc.greenLightIsOn) {
        mabc.SET_GREEN_LIGHT(true);
    }
}
