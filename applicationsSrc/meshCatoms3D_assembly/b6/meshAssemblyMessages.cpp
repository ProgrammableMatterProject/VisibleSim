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

void RoutableScaffoldMessage::route(BaseSimulator::BlockCode *bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    Cell3DPosition nextHopPos = Cell3DPosition(-10,-10,-10);

    // Routing catom must be part of the scaffold
    VS_ASSERT(mabc.ruleMatcher->isInMeshOrSandbox(mabc.norm(mabc.catom->position)));

    // Attempt direct delivery
    P2PNetworkInterface *nextHopItf = mabc.catom->getInterface(dstPos);
    if (nextHopItf and nextHopItf->isConnected()) {
        mabc.sendMessage(this->clone(), nextHopItf, MSG_DELAY_MC, 0);
        return;
    }

    // Otherwise, check if dstPos is part of scaffold and if that's not the case,
    //  find the nearest scaffold position for routing. Use the position with the lowest
    //  ZYX, as it is the most likely to have already been built
    bool dstInMesh = mabc.ruleMatcher->isInMeshOrSandbox(mabc.norm(dstPos));
    Cell3DPosition dstSFPos = dstInMesh ? dstPos :
        Cell3DPosition(numeric_limits<short>::max(), numeric_limits<short>::max(),
                       numeric_limits<short>::max());
    if (not dstInMesh) {
        for (const Cell3DPosition& nPos : mabc.lattice->getActiveNeighborCells(dstPos)) {
            if (mabc.ruleMatcher->isInMeshOrSandbox(mabc.norm(nPos))
                and Cell3DPosition::compare_ZYX(nPos, dstSFPos))
                dstSFPos = nPos;
        }

        cout << dstPos << " not in mesh -> " << dstSFPos << endl;

        VS_ASSERT(dstSFPos != Cell3DPosition(numeric_limits<short>::max(),
                                             numeric_limits<short>::max(),
                                             numeric_limits<short>::max()));
    }

    // Assumptions:
    // (1) Scaffold is convex
    //
    // Questions:
    // (a) Is it better to go up/down and then left/right, or the opposite? Does it matter?

    // 0. Support modules case
    //  Either route to TR, or to module on EPL below depending on dst
    if (mabc.ruleMatcher->isSupportModule(mabc.sbnorm(mabc.catom->position))) {
        cout << "0.0 - ";
        nextHopPos = mabc.catom->position - mabc.ruleMatcher->getBranchUnitOffset(
            mabc.ruleMatcher->getAlternateBranchIndex(mabc.branch));
    } else if ( (mabc.getTileRootPosition(mabc.catom->position)[2]
                 == mabc.getTileRootPosition(dstSFPos)[2])
                and ( (mabc.getTileRootPosition(mabc.catom->position)[2]
                       == mabc.getTileRootPosition(dstSFPos)[2])
                      or (mabc.catom->position[2] // Not on horizontal plane, go there
                          != mabc.getTileRootPosition(mabc.catom->position)[2]))
        ) {
        // 1. Si la destination est dans sa tile

        if (mabc.ruleMatcher->isTileRoot(mabc.sbnorm(mabc.catom->position))) {
            cout << "1.0 - ";

            // 1.0 Module is tile root, forward to correct branch
            nextHopPos = mabc.catom->position +
                mabc.ruleMatcher->getBranchUnitOffset(mabc.sbnorm(dstSFPos));
        } else if (mabc.areOnTheSameBranch(mabc.catom->position, dstSFPos)) {
            // 1.1 Si la destination est dans sa branche, on remonte ou descend la branche.
            if (Cell3DPosition::compare_ZYX(mabc.catom->position, dstSFPos)) {
                nextHopPos = mabc.catom->position +
                    mabc.ruleMatcher->getBranchUnitOffset(mabc.sbnorm(mabc.catom->position));
                cout << "1.1.< - ";
            } else {
                nextHopPos = mabc.catom->position -
                    mabc.ruleMatcher->getBranchUnitOffset(mabc.sbnorm(mabc.catom->position));
                cout << "1.1.> - ";
            }
        } else {
            // 1.2 Si la destination est dans la tile mais pas dans la branche, on renvoie au TR de la tile puis remonte la branche de destination.
            // Ou 2.0, la tile dst est sur le plan de notre tile, il faut déjà router au TR
            // Send backward
                nextHopPos = mabc.catom->position -
                    mabc.ruleMatcher->getBranchUnitOffset(mabc.sbnorm(mabc.catom->position));
                cout << "1.1.2/2.0 - ";
        }

    } else {
        cout << "dstSFPos: " << dstSFPos << endl;
        cout << "cTR: " << mabc.getTileRootPosition(mabc.catom->position) << endl;
        cout << "dstTR: " << mabc.getTileRootPosition(dstSFPos) << endl;

        // 2. Si la destination est dans le même plan de tile
        //   et que l'on est sur le même plan que les TR
        if (mabc.getTileRootPosition(mabc.catom->position)[2]
            == mabc.getTileRootPosition(dstSFPos)[2]) {
            // 2.1 Si la destination n’est pas dans la tile mais est sur une tile du même plan, on navigue horizontalement jusqu’au TR de la tile de destination puis remonte la branche de destination. (On définit aussi une priorité entre x et y en cas d’égalité, trivial).
            if (mabc.getTileRootPosition(mabc.catom->position)[0]
                < mabc.getTileRootPosition(dstSFPos)[0]) {
                // Send frontward X
                cout << "2.1.x.< - ";
                nextHopPos = mabc.catom->position + mabc.ruleMatcher->
                    getBranchUnitOffset(XBranch);
            } else if (mabc.getTileRootPosition(mabc.catom->position)[0]
                       > mabc.getTileRootPosition(dstSFPos)[0]) {
                // Send backward X
                cout << "2.1.x.> - ";
                nextHopPos = mabc.catom->position - mabc.ruleMatcher->
                    getBranchUnitOffset(XBranch);
            } else { // ==
                if (mabc.getTileRootPosition(mabc.catom->position)[1]
                    < mabc.getTileRootPosition(dstSFPos)[1]) {
                    // Send frontward Y
                    cout << "2.1.y.< - ";
                    nextHopPos = mabc.catom->position + mabc.ruleMatcher->
                        getBranchUnitOffset(YBranch);
                } else if (mabc.getTileRootPosition(mabc.catom->position)[1]
                           > mabc.getTileRootPosition(dstSFPos)[1]) {
                    // Send backward Y
                    cout << "2.1.y.< - ";
                    nextHopPos = mabc.catom->position - mabc.ruleMatcher->
                        getBranchUnitOffset(XBranch);
                } else {
                    VS_ASSERT(false);
                }
            }

        } else {
            // 4. Si la destination n’est pas dans la tile et pas dans le même plan :
            if (mabc.ruleMatcher->isTileRoot(mabc.sbnorm(mabc.catom->position))) {
                // 4.0 Coordinator,
                //  envoi sur l'une des branches verticales via une fonction de décision
                // Send upward/downward to best branch candidate
                bool upward = mabc.getTileRootPosition(mabc.catom->position)[2]
                    < mabc.getTileRootPosition(dstSFPos)[2];
                BranchIndex nextHopBi = mabc.findBestBranchIndexForMsgDst(dstSFPos, upward);
                if (upward) {
                    cout << "4.0.< - ";
                    nextHopPos = mabc.catom->position +
                        mabc.ruleMatcher->getBranchUnitOffset(nextHopBi);
                } else {
                    cout << "4.0.> - ";
                    nextHopPos = mabc.catom->position -
                        mabc.ruleMatcher->getBranchUnitOffset(nextHopBi);
                }
            } else if (mabc.ruleMatcher->getBranchIndexForNonRootPosition
                       (mabc.sbnorm(mabc.catom->position)) == XBranch
                       or mabc.ruleMatcher->getBranchIndexForNonRootPosition
                       (mabc.sbnorm(mabc.catom->position)) == YBranch) {
                // 4.1 Si on est sur une branche horizontale, on remonte jusqu’au TR de notre tile, puis monte ou descend l’une des branches verticales, jusqu’à arriver au plan de tile de destination, avant d’appliquer 3.
                // Send backwards
                cout << "4.1 - ";
                nextHopPos = mabc.catom->position -
                    mabc.ruleMatcher->getBranchUnitOffset(mabc.branch);
            } else {
                // 4.2 Sinon, on monte ou descend la branche courante suivant le plan de destination, jusqu’à arriver au plan cible, et on applique 3.
                if (mabc.getTileRootPosition(mabc.catom->position)[2]
                    < mabc.getTileRootPosition(dstSFPos)[2]) {
                    // Send updwards
                    cout << "4.2.< - ";
                    nextHopPos = mabc.catom->position +
                        mabc.ruleMatcher->getBranchUnitOffset(mabc.branch);
                } else if (mabc.getTileRootPosition(mabc.catom->position)[2]
                           > mabc.getTileRootPosition(dstSFPos)[2]) {

                    // Send downwards
                    cout << "4.2.> - ";
                    nextHopPos = mabc.catom->position -
                        mabc.ruleMatcher->getBranchUnitOffset(mabc.branch);
                } else {
                    cout << "4.2.ERR - ";
                }
            }
        }
    }

    cout << "RSM(" << srcPos << ", " << dstSFPos << "): "
         << mabc.catom->position
         << " -> " << nextHopPos;

    nextHopItf = mabc.catom->getInterface(nextHopPos);
    VS_ASSERT_MSG(nextHopItf,
                  "RoutableScaffoldMessage::route: invalid next hop position");

    if (nextHopItf->isConnected()) mabc.sendMessage(this->clone(), nextHopItf,
                                                    MSG_DELAY_MC, 0);
    else cout << " (DISCONNECTED)";

    cout << endl;
}

void RequestTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);
    // cout << "[t-" << getScheduler()->now() << "] received request target cell" << endl;

    if (mabc.catom->position != dstPos) {
        route(bc); return;
    }

    VS_ASSERT_MSG(mabc.role == Coordinator,
                  "Non coordinator should not have handled a RequestTargetCellMessage");

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
    mabc.sendMessage(new ProvideTargetCellMessage(dstPos, srcPos, tPos),
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
                    mabc.sendMessage(new ProvideTargetCellMessage(dstPos, wPos, tPos),
                                     tipItf, MSG_DELAY_MC, 0);

                    // Update looping condition and waiting state
                    mabc.moduleWaitingOnBranch[biw] = false;
                    moduleAwoken = true;
                    mabc.constructionQueue.pop_front();
                }
            }
        }
    } while (moduleAwoken);

    mabc.log_send_message();
}

void ProvideTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    if (mabc.catom->position != dstPos) {
        route(bc); return;
    }

    VS_ASSERT_MSG(mabc.role == FreeAgent,
                  "Non FA module should not have handled a ProvideTargetCellMessage");

    mabc.targetPosition = tPos;
    // cout << "Target position for #" << mabc.catom->blockId << " is " << tPos << endl;
    mabc.matchRulesAndProbeGreenLight();
    mabc.log_send_message();
}

void CoordinatorReadyMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    if (mabc.catom->position != dstPos) {
        route(bc); return;
    }

    // Module is free agent waiting for something to do
    VS_ASSERT(mabc.role == FreeAgent);

    // Resend RequestTargetCell
    VS_ASSERT(mabc.requestTargetCellFromTileRoot());
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
