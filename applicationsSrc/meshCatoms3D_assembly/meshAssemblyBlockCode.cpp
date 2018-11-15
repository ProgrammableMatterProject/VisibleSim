/**
 * @file   meshAssemblyBlockCode.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Mon Oct  1 10:42:29 2018
 * 
 * @brief  
 * 
 * 
 */

#include <iostream>
#include <set>

#include "catoms3DWorld.h"
#include "scheduler.h"
#include "events.h"
#include "trace.h"
#include "tDefs.h"

#include "teleportationEvents.h"
#include "rotation3DEvents.h"
#include "catoms3DMotionEngine.h"

#include "meshAssemblyBlockCode.hpp"

using namespace Catoms3D;
using namespace MeshCoating;

Time MeshAssemblyBlockCode::t0 = 0;
int MeshAssemblyBlockCode::nbCatomsInPlace = 0;
int MeshAssemblyBlockCode::nbMessages = 0;
bool MeshAssemblyBlockCode::sandboxInitialized = false;
uint MeshAssemblyBlockCode::X_MAX;
uint MeshAssemblyBlockCode::Y_MAX;
uint MeshAssemblyBlockCode::Z_MAX;
constexpr std::array<Cell3DPosition, 6> MeshAssemblyBlockCode::incidentTipRelativePos;
constexpr std::array<Cell3DPosition, 12> MeshAssemblyBlockCode::entryPointRelativePos;
constexpr Cell3DPosition MeshAssemblyBlockCode::meshSeedPosition;

MeshAssemblyBlockCode::MeshAssemblyBlockCode(Catoms3DBlock *host):
    Catoms3DBlockCode(host) {
    scheduler = getScheduler();
    world = BaseSimulator::getWorld();
    lattice = world->lattice;   
    catom = host;
    
    const Cell3DPosition& ub = lattice->getGridUpperBounds();
    // Round down mesh dimensions to previous multiple of B
    // TODO: Adapt to CSG
    X_MAX = ub[0] - (B - ub[0] % B);
    Y_MAX = ub[1] - (B - ub[1] % B);
    Z_MAX = ub[2] - (B - ub[2] % B);
    ruleMatcher = new MeshRuleMatcher(X_MAX, Y_MAX, Z_MAX, B);
}


MeshAssemblyBlockCode::~MeshAssemblyBlockCode() {
    if (ruleMatcher->isInMesh(norm(catom->position))) {
        OUTPUT << "bitrate:\t" << catom->blockId << "\t"
               << maxBitrate.first << "\t"
               << (maxBitrate.second.empty() ?
            ruleMatcher->roleToString(role) : maxBitrate.second) << endl;
    }
}

void MeshAssemblyBlockCode::onBlockSelected() {
    // if (catom->blockId == 1) {
    //     cout << "Modules in system: " << lattice->nbModules << endl;
    //     cout << "MAX: " << X_MAX << ", " << Y_MAX << ", " << Z_MAX << endl;
    //     for (int x = 0; x < (int)X_MAX + meshSeedPosition[0]; x++) {
    //         for (int y = 0; y < (int)Y_MAX + meshSeedPosition[1]; y++) {
    //             for (int z = 0; z < (int)Z_MAX + meshSeedPosition[2]; z++) {
    //                 const Cell3DPosition& pos = Cell3DPosition(x,y,z);
    //                 BuildingBlock *bb = lattice->getBlock(pos);
    //                 if (bb // and ruleMatcher->isInMesh(norm(pos)))
    //                     and ruleMatcher->isInPyramid(norm(pos))
    //                     and (ruleMatcher->isOnXPyramidBorder(norm(pos))
    //                          or ruleMatcher->isOnXOppPyramidBorder(norm(pos))
    //                          or ruleMatcher->isOnYPyramidBorder(norm(pos))
    //                          or ruleMatcher->isOnYOppPyramidBorder(norm(pos))))
    //                     bb->setColor(WHITE);
    //             }
    //         }
    //     }
    // }
    
    // Debug:
    // (1) Print details of branch growth plan and previous round
    if (role == Coordinator) {
        cout << "Growth Plan: [ ";
        for (int i = 0; i < 6; i++)
            cout << catomsReqByBranch[i] << ", ";
        cout << " ]" << endl;

        // cout << "Last Round: [ ";
        // for (int i = 0; i < 6; i++)
        //     cout << fedCatomOnLastRound[i] << ", ";
        // cout << " ]" << endl;

        cout << "Open Positions: [ ";
        for (int i = 0; i < 6; i++)
            cout << endl << "\t\t  "
                 <<(openPositions[i] ? openPositions[i]->config_print() : "NULL") << ", ";
        cout << " ]" << endl;

        cout << "branchTime: [ ";
        for (int i = 0; i < 6; i++)
            cout << branchTime[i] << ", ";
        cout << " ]" << endl;

        cout << "feedBranch: [ ";
        for (int i = 0; i < 6; i++)
            cout << feedBranch[i] << ", ";
        cout << " ]" << endl;

        cout << "targetLevel: [ ";
        for (int i = 0; i < 6; i++)
            cout << targetLevel[i] << ", ";
        cout << " ]" << endl;
        
        // cout << "Target for Entry Points: [ ";
        // for (int i = 0; i < 8; i++)
        //     cout << endl << "\t\t  " << targetForEntryPoint[i].config_print() << ", ";
        // cout << " ]" << endl;

        cout << "itCounter: " << itCounter << endl;

        // for (int i = 0; i < N_BRANCHES; i++) {
        //     cout << "shouldGrowPyramidBranch(: " << i << ") " <<
        //         ruleMatcher->shouldGrowPyramidBranch(norm(catom->position), (BranchIndex)i)
        //          << endl;
        // }

        // for (int i = 0; i < N_BRANCHES; i++) {
        //     cout << "tileRootAtEndOfBranch(: " << i << ") " <<
        //         denorm(ruleMatcher->getTileRootAtEndOfBranch(norm(catom->position), (BranchIndex)i))
        //          << endl;
        // }

    }
    
    // catom->setColor(debugColorIndex++);

    cout << "branch: " << branch << endl;
    cout << "coordinatorPos: " << coordinatorPos << endl;
    cout << "nearestCoordinatorPos: " << denorm(ruleMatcher->getNearestTileRootPosition(norm(catom->position))) << endl;
    cout << "role: " << role << endl;
    cout << "localNeighborhood: " << catom->getLocalNeighborhoodState() << endl;
    Cell3DPosition nextHop;
    matchLocalRules(catom->getLocalNeighborhoodState(), catom->position,
                    targetPosition, coordinatorPos, step, nextHop);            
    cout << "nextHop: " << getTileRelativePosition() << " -> " << nextHop << endl;
    cout << "isInMesh: " << ruleMatcher->isInMesh(norm(catom->position)) << endl;
    
    // cout << "Possible Rotations: " << endl;
    // const vector<std::pair<const Catoms3DMotionRulesLink*, Rotations3D>> allRotations =
    //     Catoms3DMotionEngine::getAllRotationsForModule(catom);

    // for (const auto& rotation : allRotations) {
    //         cout << *rotation.first << " --> " << rotation.second << endl;
    // }

    // cout << "isOnEntryPoint: " << isOnEntryPoint(catom->position) << endl;
    // cout << X_MAX << " " << Y_MAX << " " << Z_MAX << endl;
    // cout << "isOnOppXBorder: " << ruleMatcher->isOnXOppBorder(norm(catom->position)) << endl;
    // cout << "isOnOppYBorder: " << ruleMatcher->isOnYOppBorder(norm(catom->position)) << endl;
}

void MeshAssemblyBlockCode::startup() {
    stringstream info;
    info << "Starting ";
    startTime = scheduler->now();

    if (not sandboxInitialized)
        initializeSandbox();
    
    coordinatorPos =
            denorm(ruleMatcher->getNearestTileRootPosition(norm(catom->position)));
    
    // Do stuff
    if (catom->position == getEntryPointForMeshComponent(MeshComponent::R)
        and lattice->isFree(coordinatorPos)) {
        if (coordinatorPos == meshSeedPosition) {
            targetPosition = coordinatorPos;
            matchRulesAndRotate();
        } else {
            role = FreeAgent;
            return;
        }
    } else if (ruleMatcher->isVerticalBranchTip(norm(catom->position))) {            
        role = ActiveBeamTip; // nothing to be done, wait for tPos requests
        
        // Add z = B to ensure that level -1 catoms are handled properly
        short bi = ruleMatcher->determineBranchForPosition(
            norm(catom->position[2] < meshSeedPosition[2] ?
                 catom->position + Cell3DPosition(0,0,B) : catom->position));
        VS_ASSERT_MSG(bi >= 0 and bi < N_BRANCHES, "cannot determine branch.");
        branch = static_cast<BranchIndex>(bi);
    } else if (meshSeedPosition[2] - catom->position[2] > 1) {
        role = PassiveBeam; // nothing to be done here for visual decoration only
    } else {
            role = FreeAgent;
        if (not requestTargetCellFromTileRoot() )
            VS_ASSERT_MSG(false, "meshAssembly: spawned module cannot be without a delegate coordinator in its vicinity.");        
    }
}

const Cell3DPosition
MeshAssemblyBlockCode::norm(const Cell3DPosition& pos) {    
    return pos - meshSeedPosition;
}

const Cell3DPosition
MeshAssemblyBlockCode::relatify(const Cell3DPosition& pos) {    
    return pos - coordinatorPos;
}

const Cell3DPosition
MeshAssemblyBlockCode::denorm(const Cell3DPosition& pos) {    
    return pos + meshSeedPosition;
}

const Cell3DPosition
MeshAssemblyBlockCode::derelatify(const Cell3DPosition& pos) {    
    return pos + coordinatorPos;
}

void MeshAssemblyBlockCode::processReceivedMessage(MessagePtr msg,
                                                   P2PNetworkInterface *sender) {
    stringstream info;

    switch (msg->type) {
        // ALL MOVED TO HANDLEABLE MESSAGES
        default:
            cout << "Unknown Generic Message Type" << endl;
            assert(false);
            break;
    }

}

void MeshAssemblyBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;
	
    switch (pev->eventType) {
        case EVENT_RECEIVE_MESSAGE: {
            message =
                (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;

            if (message->isMessageHandleable()) {
                std::shared_ptr<HandleableMessage> hMsg =
                    (std::static_pointer_cast<HandleableMessage>(message));
                
                console << " received " << hMsg->getName() << " from "
                        << message->sourceInterface->hostBlock->blockId
                        << " at " << getScheduler()->now() << "\n";
                hMsg->handle(this);
            } else {
                P2PNetworkInterface * recv_interface = message->destinationInterface;
            
                // Handover to global message handler
                processReceivedMessage(message, recv_interface);
            }
        } break;

        case EVENT_ROTATION3D_END:
            console << "Rotation to " << catom->position << " over" << "\n";
        case EVENT_TELEPORTATION_END: {
            step++;
            if (catom->position == targetPosition and not isOnEntryPoint(catom->position)) {
                role = ruleMatcher->getRoleForPosition(norm(catom->position));
                catom->setColor(ruleMatcher->getColorForPosition(norm(catom->position)));

                // STAT EXPORT
                OUTPUT << "nbCatomsInPlace:\t" << (int)round(scheduler->now() / getRoundDuration()) << "\t" << ++nbCatomsInPlace << endl;

                if (ruleMatcher->isVerticalBranchTip(norm(catom->position))) {
                    coordinatorPos =
                        denorm(ruleMatcher->getNearestTileRootPosition(norm(catom->position)));
                    return;
                }
                
                if (ruleMatcher->isTileRoot(norm(catom->position))) 
                    initializeTileRoot();
                else if (ruleMatcher->isTileSupport(norm(catom->position)))
                    initializeSupportModule();
                else {
                    BranchIndex bi = 
                        ruleMatcher->getBranchIndexForNonRootPosition(norm(targetPosition));
                    const Cell3DPosition& nextPosAlongBranch =
                        catom->position + ruleMatcher->getBranchUnitOffset(bi);

                    // Coordinate to let the last arrived branch continue the construction
                    if (ruleMatcher->isTileRoot(norm(nextPosAlongBranch))
                        and incidentBranchesToRootAreComplete(nextPosAlongBranch)) {
                        lattice->highlightCell(nextPosAlongBranch, BLUE);
                        cout << "Some branches are missing around "
                             << nextPosAlongBranch << endl;

                        // Notify future tile root as position should be filled
                        P2PNetworkInterface* zBranchTipItf = NULL;
                        for (const auto& pos : lattice->
                                 getActiveNeighborCells(catom->position)) {
                            if (ruleMatcher->isOnZBranch(norm(pos))) {
                                zBranchTipItf = catom->getInterface(pos);
                                break;
                            }
                        }

                        VS_ASSERT(zBranchTipItf);
                        sendMessage(new TileInsertionReadyMessage(),
                                    zBranchTipItf, MSG_DELAY_MC, 0);
                        log_send_message();
                    } 
                }
            } else {
                if (catom->position == targetPosition and isOnEntryPoint(catom->position)) {
                    // Update tile belonging
                    coordinatorPos = denorm(ruleMatcher->
                                            getNearestTileRootPosition(norm(catom->position)));

                    // Reset step counter for matching a new ruleset
                    step = 1;
                    
                    // Check that that new tile root is in place and if absent,
                    //  wait for above layer XY modules to notify completion of previous tiles
                    if (lattice->isFree(coordinatorPos)) {
                        if (ruleMatcher->isOnXPyramidBorder(norm(coordinatorPos))
                            and ruleMatcher->isOnYPyramidBorder(norm(coordinatorPos)))
                            targetPosition = coordinatorPos;
                        else {
                            catom->setColor(WHITE);
                            lattice->highlightCell(coordinatorPos, WHITE);
                            return;
                        }
                    } else {
                        // Otherwise ask it for a new targetPosition
                        catom->setColor(BLACK);
                        if (not requestTargetCellFromTileRoot()) {
                            catom->setColor(RED);
                            VS_ASSERT_MSG(false, "arriving module cannot be without delegate coordinator in its vicinity.");
                        }
                        
                        return;
                    }
                }
                
                matchRulesAndRotate();
            }            
    } break;            
            
    case EVENT_TAP: {
    } break;

        case EVENT_INTERRUPTION: {            
            std::shared_ptr<InterruptionEvent> itev =
                std::static_pointer_cast<InterruptionEvent>(pev);
            
            switch(itev->mode) {

                case IT_MODE_TILEROOT_ACTIVATION: {
                    // static const int trInsertionTime = 18;
                    
                    // Only introduce catoms if on the lower tile level
                    if (catom->position[2] == meshSeedPosition[2]) {
                        if (itCounter == 0) {
                            handleMeshComponentInsertion(S_RZ);
                            handleMeshComponentInsertion(S_LZ);
                            
                        } else if (itCounter == 1 and ruleMatcher->
                                   shouldGrowBranch(norm(catom->position), YBranch)) {
                            handleMeshComponentInsertion(Y_1);
                            catomsReqByBranch[YBranch]--;

                        } else if (itCounter == 3 and ruleMatcher->
                                   shouldGrowBranch(norm(catom->position), XBranch)) {
                            handleMeshComponentInsertion(X_1);
                            catomsReqByBranch[XBranch]--;
                            
                        } else if (itCounter == 4) {
                            handleMeshComponentInsertion(S_Z);
                            handleMeshComponentInsertion(S_RevZ);
                        } else if (itCounter == 18 and
                                   ruleMatcher->shouldGrowRevZBranch(norm(catom->position))) {
                            handleMeshComponentInsertion(Z_R_EPL);
                        }

                        for (short bi = 0; bi < XBranch; bi++) {
                            if (ruleMatcher->shouldGrowBranch(norm(catom->position),
                                                              (BranchIndex)bi))
                                sendCatomsUpBranchIfRequired((BranchIndex)bi);
                            branchTime[bi]++;
                        }
                                                                        
                        if (not fedCatomsOnLastRound and itCounter > 5) {
                            // Spawning Rules
                            if (catomsReqByBranch[XBranch] > 0) {
                                handleMeshComponentInsertion(static_cast<MeshComponent>
                                                             (X_1 + B -
                                                              catomsReqByBranch[XBranch] - 1));
                                catomsReqByBranch[XBranch]--;
                            }

                            if (catomsReqByBranch[YBranch] > 0) {                            
                                handleMeshComponentInsertion(static_cast<MeshComponent>
                                                             (Y_1 + B -
                                                              catomsReqByBranch[YBranch] - 1));
                                catomsReqByBranch[YBranch]--;
                            }
                            
                            if (catomsReqByBranch[ZBranch] > 0 and itCounter > 7) {
                                handleMeshComponentInsertion(static_cast<MeshComponent>
                                                             (Z_1 + B -
                                                              catomsReqByBranch[ZBranch] - 1));
                                catomsReqByBranch[ZBranch]--;
                            }                        

                            if (catomsReqByBranch[RevZBranch] > 0 and itCounter > 7) {
                                handleMeshComponentInsertion(static_cast<MeshComponent>
                                                             (RevZ_1 + B -
                                                              catomsReqByBranch[RevZBranch] - 1));
                                catomsReqByBranch[RevZBranch]--;
                            }

                            if (catomsReqByBranch[LZBranch] > 0 and itCounter > 13) {
                                handleMeshComponentInsertion(static_cast<MeshComponent>
                                                             (LZ_1 + B -
                                                              catomsReqByBranch[LZBranch] - 1));
                                catomsReqByBranch[LZBranch]--;
                            }

                            if (catomsReqByBranch[RZBranch] > 0 and itCounter > 13) {
                                handleMeshComponentInsertion(static_cast<MeshComponent>
                                                             (RZ_1 + B -
                                                              catomsReqByBranch[RZBranch] - 1));
                                catomsReqByBranch[RZBranch]--;
                            }
                        } else {
                            fedCatomsOnLastRound = false;
                        }
                    }

                    itCounter++;
                    if (itCounter < 65535) { // FIXME:
                        getScheduler()->schedule(
                            new InterruptionEvent(getScheduler()->now() +
                                                  (getRoundDuration()),
                                                  catom, IT_MODE_TILEROOT_ACTIVATION));
                    }
                } break;
            }            
        }
    }
}

void MeshAssemblyBlockCode::discardNextTargetForComponent(MeshComponent comp) {   
    short idx = getEntryPointLocationForCell(getEntryPointForMeshComponent(comp)) - RevZ_EPL;
    if (not targetQueueForEPL[idx].empty())
        targetQueueForEPL[idx].pop();
}

const Cell3DPosition MeshAssemblyBlockCode::getNextTargetForEPL(MeshComponent epl) {
    short idx = epl - RevZ_EPL;
    if (not targetQueueForEPL[idx].empty()) {
        const Cell3DPosition& tPos = targetQueueForEPL[idx].front();
        targetQueueForEPL[idx].pop();
        return tPos;
    }
    
    switch (epl) {
        case RevZ_EPL: return Cell3DPosition(-4, -4, 5);
        // case RevZ_R_EPL: return getEntryPointPosition(R);
        // case RZ_L_EPL: return getEntryPointPosition(R);
        case RZ_EPL: return Cell3DPosition(-1, -4, 5);
        case RZ_R_EPL: return Cell3DPosition(-4, 0, 5);
        case Z_R_EPL: return Cell3DPosition(-4, -5, 5);
        case Z_EPL: return Cell3DPosition(-1, -1, 5);
        case Z_L_EPL: return Cell3DPosition(-5, -4, 5);
        case LZ_R_EPL: return Cell3DPosition(0, -4, 5);
        case LZ_EPL: return Cell3DPosition(-4, -1, 5);
            // case LZ_L_EPL: return getEntryPointPosition(R);
            // case RevZ_L_EPL: return getEntryPointPosition(R);
        default:
            cerr << "getNextTargetForEPL(" << epl << ")" << endl;
            VS_ASSERT_MSG(false, "getNextTargetForEPL: input is not an EPL");
    }

    return Cell3DPosition(); // unreachable
}

void MeshAssemblyBlockCode::updateOpenPositions() {
    for (int i = 0; i < N_BRANCHES; i++) {        
        // [1..B], the number of already placed catoms + 1.
        // B means that branch is finished or should not be grown
        int multiplier = B - catomsReqByBranch[i];
        
        if (catomsReqByBranch[i] > 0 and openPositions[i]) {
            *openPositions[i] = Cell3DPosition(catom->position + multiplier * ruleMatcher->
                                               getBranchUnitOffset((BranchIndex)i));
        } else if (catomsReqByBranch[i] > 0 and not openPositions[i]) {
            openPositions[i] = new Cell3DPosition(catom->position + multiplier * ruleMatcher->
                                                  getBranchUnitOffset((BranchIndex)i));
        } else openPositions[i] = NULL;
    }
}

short MeshAssemblyBlockCode::getEntryPointLocationForCell(const Cell3DPosition& pos) {
    for (int i = 0; i < 12; i++)                                               
        if (pos == catom->position + entryPointRelativePos[i]) return i + RevZ_EPL;
    
    return -1;
} 

const Cell3DPosition MeshAssemblyBlockCode::getEntryPointForMeshComponent(MeshComponent mc) {
    switch(mc) {
        case R: return getEntryPointPosition(Z_R_EPL);
        case S_Z: return getEntryPointPosition(LZ_EPL);
        case S_RevZ: return getEntryPointPosition(RZ_EPL);
        case S_LZ: return getEntryPointPosition(LZ_R_EPL);
        case S_RZ: return getEntryPointPosition(RZ_R_EPL);

        case X_1: return getEntryPointPosition(Z_R_EPL);
        case X_2: case X_3: case X_4: case X_5: return getEntryPointPosition(RZ_EPL);

        case Y_1: return getEntryPointPosition(Z_L_EPL);
        case Y_2: case Y_3: case Y_4: case Y_5: return getEntryPointPosition(LZ_EPL);

        case Z_1: case Z_2: case Z_3: case Z_4: case Z_5:
            return getEntryPointPosition(Z_EPL);

        case RevZ_1: case RevZ_2: case RevZ_3: case RevZ_4: case RevZ_5:
            return getEntryPointPosition(RevZ_EPL);

        case LZ_1: case LZ_2: case LZ_3: case LZ_4: case LZ_5:
            return getEntryPointPosition(LZ_EPL);

        case RZ_1: case RZ_2: case RZ_3: case RZ_4: case RZ_5:
            return getEntryPointPosition(RZ_EPL);

        // case EPLs
        case RevZ_EPL: return getEntryPointPosition(Z_EPL);
        case RevZ_R_EPL: return getEntryPointPosition(Z_EPL);
        case RZ_L_EPL: return getEntryPointPosition(LZ_EPL);
        case RZ_EPL: return getEntryPointPosition(LZ_EPL);
        case RZ_R_EPL: return getEntryPointPosition(LZ_EPL);
        case Z_R_EPL: return getEntryPointPosition(RevZ_EPL);
        case Z_EPL: return getEntryPointPosition(RevZ_EPL);
        case Z_L_EPL: return getEntryPointPosition(RevZ_EPL);
        case LZ_R_EPL: return getEntryPointPosition(RZ_EPL);
        case LZ_EPL: return getEntryPointPosition(RZ_EPL);
        case LZ_L_EPL: return getEntryPointPosition(RZ_EPL);
        case RevZ_L_EPL: return getEntryPointPosition(Z_EPL);
        default: throw NotImplementedException("Entry point for EPL mesh component");
    }

    return Cell3DPosition(); // unreachable
}

void MeshAssemblyBlockCode::handleMeshComponentInsertion(MeshComponent mc) {
    // Introduce new catoms
    // cout << "[t-" << scheduler->now() << "] catom introduced" << endl;
    world->addBlock(0, buildNewBlockCode,
                    getEntryPointForMeshComponent(mc), ORANGE);

    fedCatomsOnLastRound = true;
}

bool MeshAssemblyBlockCode::
incidentBranchesToRootAreComplete(const Cell3DPosition& pos) {
    VS_ASSERT(ruleMatcher->isInMesh(norm(pos))
              and ruleMatcher->isTileRoot(norm(pos)));

    for (int i = 0; i < N_BRANCHES; i++) {
        if (!isIncidentBranchTipInPlace(pos, static_cast<BranchIndex>(i))) return false;
    }

    return true;
}

bool MeshAssemblyBlockCode::
isIncidentBranchTipInPlace(const Cell3DPosition& trp, BranchIndex bi) {
    const Cell3DPosition& tipp = trp + incidentTipRelativePos[bi];
    return (not ruleMatcher->isInPyramid(ruleMatcher->
                                         getTileRootPositionForMeshPosition(norm(tipp))))
        or lattice->cellHasBlock(tipp);
}

void MeshAssemblyBlockCode::scheduleRotationTo(const Cell3DPosition& pos) {
    try {
        OUTPUT << "mvmt: " << round((scheduler->now()) / getRoundDuration()) << "\t" << endl;
        // cout << "[t-" << scheduler->now() << "] rotation scheduled" << endl;
        scheduler->schedule(new Rotation3DStartEvent(getScheduler()->now(),
                                                     catom, pos,
                                                     RotationLinkType::OctaFace, false));
#ifdef INTERACTIVE_MODE
        awaitKeyPressed();
#endif
    } catch (const NoAvailableRotationPivotException& e_piv) {
        cerr << e_piv.what();
        cerr << "target position: " << pos << endl;
        catom->setColor(RED);
        VS_ASSERT(false);
    } catch (std::exception const& e) {
        cerr << "exception: " << e.what() << endl;
        VS_ASSERT(false);
    }
}

void MeshAssemblyBlockCode::initializeTileRoot() {
    // Switch role
    role = Coordinator;
    coordinatorPos = catom->position;

    if (norm(catom->position) == Cell3DPosition(0,0,0)) t0 = scheduler->now();
    OUTPUT << "root: " << (int)(round((scheduler->now() - t0) / getRoundDuration())) << "\t" << norm(catom->position) << endl;   
    
    // Determine how many branches need to grow from here
    // and initialize growth data structures
    for (short bi = 0; bi < N_BRANCHES; bi++) {
        catomsReqByBranch[bi] = ruleMatcher->
            shouldGrowPyramidBranch(norm(catom->position), (BranchIndex)bi) ? B - 1 : -1;
    }
        
    // Compute the corresponding list of cells to be filled
    updateOpenPositions();

    // Tell neighbor tiles that it is ready to receive catoms
    if (not isAtGroundLevel())
    for (short bi = 0; bi < XBranch; bi++) {
        P2PNetworkInterface* nItf = catom->getInterface(
            catom->position - ruleMatcher->getBranchUnitOffset(bi));

        if (nItf and nItf->isConnected()) {
            sendMessage(new InitiateFeedingMechanismMessage(getFeedingRequirements(),
                                                            catom->position[2] / B),
                        nItf, MSG_DELAY_MC, 0);
            log_send_message();
        }
    }

    
    // Schedule next growth iteration (at t + MOVEMENT_DURATION (?) )
    getScheduler()->schedule(
        new InterruptionEvent(getScheduler()->now(),
                              catom, IT_MODE_TILEROOT_ACTIVATION));
}

void MeshAssemblyBlockCode::initializeSupportModule() {
    for (const Cell3DPosition& nPos : lattice->getActiveNeighborCells(catom->position)) {
        if (ruleMatcher->isVerticalBranchTip(norm(nPos))) {
            branchTipPos = nPos;
            return;
        }
    }

    VS_ASSERT_MSG(false, "cannot find branch tip among neighbor modules");
}

const Cell3DPosition
MeshAssemblyBlockCode::getEntryPointPosition(MeshComponent epl) const {    
    return getEntryPointRelativePos(epl) + coordinatorPos;
}

bool MeshAssemblyBlockCode::isOnEntryPoint(const Cell3DPosition& pos) const {
    const Cell3DPosition& nearestTR = denorm(
        ruleMatcher->getNearestTileRootPosition(norm(pos)));
    
    for (const Cell3DPosition& ep : entryPointRelativePos) {
        if (pos == ep + nearestTR) return true;
    }

    return false;
}

bool MeshAssemblyBlockCode::requestTargetCellFromTileRoot() {
    for (const Cell3DPosition& nPos : lattice->getActiveNeighborCells(catom->position)) {
        if (ruleMatcher->isVerticalBranchTip(norm(nPos))
            or ruleMatcher->isTileSupport(norm(nPos))) {
            // Module is delegate coordinator
            P2PNetworkInterface* nItf = catom->getInterface(nPos);
            VS_ASSERT(nItf);
            // cout << "[t-" << getScheduler()->now() << "] requesting target cell" << endl;
            sendMessage(new RequestTargetCellMessage(catom->position), nItf,
                        MSG_DELAY_MC, 0);
            log_send_message();
            return true;
        }
    }

    return false;
}


void MeshAssemblyBlockCode::initializeSandbox() {
    for (const auto& pos : ruleMatcher->getAllGroundTileRootPositionsForMesh()) {
        const Cell3DPosition& denormPos = denorm(pos);
        // cout << pos << " " << denormPos << endl;
        
        for (int i = 0; i < XBranch; i++) {
            world->addBlock(0, buildNewBlockCode,
                            denormPos + incidentTipRelativePos[i], PINK);
            world->addBlock(0, buildNewBlockCode, denormPos +
                            incidentTipRelativePos[i] + incidentTipRelativePos[i], GREY);
            world->addBlock(0, buildNewBlockCode,
                            denormPos + incidentTipRelativePos[i]
                            + incidentTipRelativePos[i] + incidentTipRelativePos[i],
                            GREY);
        }

        // Add waiting tile root module
        if (denormPos != meshSeedPosition)
            world->addBlock(0, buildNewBlockCode,
                            denormPos + getEntryPointRelativePos(Z_R_EPL), WHITE);
                    
    }

    sandboxInitialized = true;
    // cout << "round duration: " << getRoundDuration() << endl;
}


void MeshAssemblyBlockCode::matchRulesAndRotate() {
    Cell3DPosition nextPos;
    bool matched = matchLocalRules(catom->getLocalNeighborhoodState(),
                                   catom->position,
                                   targetPosition,
                                   coordinatorPos, step, nextPos);

    if (not matched) {
        catom->setColor(RED);
        cout << "#" << catom->blockId << endl;
        VS_ASSERT_MSG(matched, "DID NOT FIND RULE TO MATCH.");
    }

    scheduleRotationTo(nextPos);
}


void MeshAssemblyBlockCode::sendCatomsUpBranchIfRequired(BranchIndex bi) {
    if (not feedBranch[bi]) return;    

    bool alt = targetLevel[bi] % 2 == 0;
    BranchIndex bi_0 = not alt ?
        bi : ruleMatcher->getAlternateBranchIndex(bi);
    // cout << "targetLevel[" << bi << "]: " << targetLevel[bi] << endl;
    // cout << "alt: " << alt << endl;
    // cout << "bi_0: " << bi_0 << endl;
    
    switch(bi_0) {
        case ZBranch: // Targets RevZ EPLs #5
            switch (branchTime[bi]) {
                case 9: case 11: case 13: case 15: case 17:
                    if (feedBranchRequires[bi][RevZBranch])
                        handleMeshComponentInsertion(alt ? Z_EPL : RevZ_EPL); // RevZN
                    break;
                case 19:
                    if (feedBranchRequires[bi][6])
                        handleMeshComponentInsertion(alt ? Z_EPL : RevZ_EPL); // R
                    // Target tile must be done, stop feeding until asked again
                    feedBranch[bi] = false;
                    break; 
                default: return;
            } break;
            
        case RevZBranch: // Targets Z EPLs #8
            switch (branchTime[bi]) {
                case 1:
                    if (feedBranchRequires[bi][YBranch])
                        handleMeshComponentInsertion(alt ? RevZ_EPL : Z_L_EPL); // Y1
                    // else discardNextTargetForComponent(Z_L_EPL);
                    break;
                case 3:
                    if (feedBranchRequires[bi][XBranch])
                        handleMeshComponentInsertion(alt ? RevZ_EPL : Z_R_EPL); // X1
                    // else discardNextTargetForComponent(Z_R_EPL);
                    break;
                case 9: case 11: case 13: case 15: case 17:
                    if (feedBranchRequires[bi][ZBranch])
                        handleMeshComponentInsertion(alt ? RevZ_EPL : Z_EPL); // ZN
                    break;
                default: return;
            } break;

        case LZBranch:
            switch (branchTime[bi]) {
                case 0: handleMeshComponentInsertion(alt ? LZ_EPL : RZ_R_EPL);
                    break; // S_RZ FIXME:
                case 4: handleMeshComponentInsertion(alt ? LZ_EPL : RZ_EPL);
                    break; // S_RevZ FIXME:
                case 6: case 8: case 10: case 12:
                    if (feedBranchRequires[bi][XBranch])
                        handleMeshComponentInsertion(alt ? LZ_EPL : RZ_EPL); // XN
                    break;
                case 14: case 16: case 18: case 20: case 22:
                    if (feedBranchRequires[bi][RZBranch])
                        handleMeshComponentInsertion(alt ? LZ_EPL : RZ_EPL); // RZN
                    break;
                default: return;                                       
            } break;

        case RZBranch:
            switch (branchTime[bi]) {
                case 0: handleMeshComponentInsertion(alt ? RZ_EPL : LZ_R_EPL); // S_LZ FIXME:
                    break; 
                case 4: handleMeshComponentInsertion(alt ? RZ_EPL : LZ_EPL); // S_Z FIXME:
                    break; 
                case 6: case 8: case 10: case 12:
                    if (feedBranchRequires[bi][YBranch])
                        handleMeshComponentInsertion(alt ? RZ_EPL : LZ_EPL); // YN
                    break;
                case 14: case 16: case 18: case 20: case 22:
                    if (feedBranchRequires[bi][LZBranch])
                        handleMeshComponentInsertion(alt ? RZ_EPL : LZ_EPL); // LZN
                    break;
                default: return;
            } break;
            
        default: return;
    }
}

bool MeshAssemblyBlockCode::isAtGroundLevel() {
    return catom->position[2] == meshSeedPosition[2];
}

std::array<bool, 7> MeshAssemblyBlockCode::getFeedingRequirements() {
    std::array<bool, 7> requirements;
    for (short bi = 0; bi < N_BRANCHES; bi++) {        
        requirements[bi] = ruleMatcher->shouldGrowPyramidBranch(
            norm(catom->position), static_cast<BranchIndex>(bi));
    }

    requirements[6] = not (ruleMatcher->isOnXPyramidBorder(norm(catom->position))
                           or ruleMatcher->isOnYPyramidBorder(norm(catom->position)));
    
    return requirements;
}

void MeshAssemblyBlockCode::updateMsgRate() {
    Time t = (int)(round((scheduler->now() - t0) / getRoundDuration()));
            
    if (rate.first != t) {
        rate.first = t;
        rate.second = 1;
    } else {
        rate.second++;
    }

    if (rate.second > maxBitrate.first) {
        maxBitrate.first = rate.second;
        maxBitrate.second = ruleMatcher->roleToString(role);
    }
}

int MeshAssemblyBlockCode::sendMessage(HandleableMessage *msg,P2PNetworkInterface *dest,
                                       Time t0,Time dt) {
    OUTPUT << "nbMessages:\t" << round(scheduler->now() / getRoundDuration()) << "\t" << ++nbMessages << endl   ;
    updateMsgRate();
    return BlockCode::sendMessage(msg, dest, t0, dt);
}

int MeshAssemblyBlockCode::sendMessage(Message *msg,P2PNetworkInterface *dest,
                                       Time t0,Time dt) {
    OUTPUT << "nbMessages:\t" << round(scheduler->now() / getRoundDuration()) << "\t" << ++nbMessages << endl;    
    updateMsgRate();    
    return BlockCode::sendMessage(msg, dest, t0, dt);
}

void MeshAssemblyBlockCode::log_send_message() const {
    OUTPUT << "lfmsg: " << round((scheduler->now() - startTime) / getRoundDuration()) << "\t" << MeshRuleMatcher::roleToString(role) << endl;
}
