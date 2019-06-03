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
#include <limits>

#include "catoms3DWorld.h"
#include "scheduler.h"
#include "events.h"
#include "trace.h"
#include "tDefs.h"

#include "teleportationEvents.h"
#include "rotation3DEvents.h"
#include "catoms3DMotionEngine.h"
#include "color.h"

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
bool MeshAssemblyBlockCode::constructionOver = false;
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
        // OUTPUT << "bitrate:\t" << catom->blockId << "\t"
        //        << maxBitrate.first << "\t"
        //        << (maxBitrate.second.empty() ?
        //            ruleMatcher->roleToString(role) : maxBitrate.second) << endl;
    }
}

void MeshAssemblyBlockCode::onAssertTriggered() {
    onBlockSelected();
    catom->setColor(BLACK);
}

void MeshAssemblyBlockCode::onBlockSelected() {

    // int dimension = ruleMatcher->getCubeDimension();
    // int zmax = (dimension - 1) * B + meshSeedPosition[2];
    // int a = (dimension - 1) / 2;
    // const Cell3DPosition &last = dimension % 2 == 0 ?
    //     Cell3DPosition(a * B + B / 2 + meshSeedPosition[0],
    //                    a * B + B / 2,
    //                    zmax)
    //     : Cell3DPosition(a * B + meshSeedPosition[0] - 1,
    //                      a * B + meshSeedPosition[1] - 1,
    //                      zmax);

    // cout << "dimension: " << dimension << endl;
    // cout << "max: " << last << endl;
    // lattice->highlightCell(last, RED);

    // const Cell3DPosition& glb = world->lattice->getGridLowerBounds();
    // const Cell3DPosition& ulb = world->lattice->getGridUpperBounds();
    // Cell3DPosition pos;
    // for (short iz = glb[2]; iz < ulb[2]; iz++) {
    //     for (short iy = glb[1]; iy < ulb[1]; iy++) {
    //         for (short ix = glb[0]; ix < ulb[0]; ix++) {
    //             pos.set(ix, iy, iz);


                // if (ruleMatcher->isOnYOppBorder(norm(pos))
                //     and ruleMatcher->isOnXBorder(norm(pos))
                //     and pos[2] > meshSeedPosition[2]
                //     and (pos[2] / B) % 2 == 0)
                //     lattice->highlightCell(pos, BLACK);

                // if (ruleMatcher->isInGrid(norm(pos)))
                //     lattice->highlightCell(pos, WHITE);

                // if (ruleMatcher->isInSandbox(norm(pos)))
                //     lattice->highlightCell(pos, BLACK);

                // if (ruleMatcher->isInCube(norm(pos)))
                //     lattice->highlightCell(pos, PINK);

                // if (ruleMatcher->isOnOppXBranch(norm(pos)))
                //     lattice->highlightCell(pos, RED);
                // else if (ruleMatcher->isOnOppYBranch(norm(pos)))
                //     lattice->highlightCell(pos, BLUE);

                // if (ruleMatcher->isOnXOppCubeBorder(norm(pos)))
                //     lattice->highlightCell(pos, BLUE);
                // else if (ruleMatcher->isOnYOppCubeBorder(norm(pos)))
                //     lattice->highlightCell(pos, RED);
                // else if (ruleMatcher->isOnXCubeBorder(norm(pos)))
                //     lattice->highlightCell(pos, CYAN);
                // else if (ruleMatcher->isOnYCubeBorder(norm(pos)))
                //     lattice->highlightCell(pos, MAGENTA);

    //         }
    //     }
    // }


    // Debug:
    // (1) Print details of branch growth plan and previous round
    cout << endl << "--- PRINT MODULE " << *catom << "---" << endl;
    if (role == Coordinator) {
        // cout << "Growth Plan: [ ";
        // for (int i = 0; i < 6; i++)
        //     cout << catomsReqByBranch[i] << ", ";
        // cout << " ]" << endl;

        cout << "Construction Queue: [ " << endl;
        cout << "|   Component   |   EPL  |" << endl << endl;
        for (const auto& pair : constructionQueue) {
            cout << "\t{ " << ruleMatcher->component_to_string(pair.first) << ", "
                 << ruleMatcher->component_to_string(pair.second) << " }" << endl;
        }
        cout << "]" << endl;

        cout << "sandboxResourcesRequirement: " << endl;
        for (int bi = 0; bi < XBranch; bi++)
            cout << ruleMatcher->branch_to_string((BranchIndex)bi) << ": " <<
                sandboxResourcesRequirement.find(
                    ruleMatcher->getDefaultEPLComponentForBranch((BranchIndex)bi))->second
                 << endl;

        cout << "ResourcesForTileThrough: " << endl;
        for (int bi = 0; bi < XBranch; bi++)
            cout << ruleMatcher->branch_to_string((BranchIndex)bi) << ": " <<
                resourcesForTileThrough(catom->position, ruleMatcher->getDefaultEPLComponentForBranch((BranchIndex)bi))
                 << endl;


        cout << "NbIncidentVerticalCubeBranches: " << ruleMatcher->getNbIncidentVerticalCubeBranches(norm(catom->position)) << endl;
    }

    if (ruleMatcher->isTileRoot(norm(catom->position))) {
    //     for (int i = 0; i < N_INC_BRANCHES; i++)
    //         cout << "hasIncidentBranch(" << i << "): "
    //              << hasIncidentBranch((BranchIndex)i) << endl;
    //     cout << "numIncidentVerticalBranches: " << numIncidentVerticalBranches << endl;

        if (not constructionQueue.empty())
            cout << "nextPos: "
                 << ruleMatcher->component_to_string(constructionQueue.front().first)
                 << endl;
        else
            cout << "nextPos: " << "NULL" << endl;

    //     for (int i = 0; i < N_BRANCHES; i++) {
    //         BranchIndex bi = (BranchIndex)i;
    //         cout << "Res4branch: " << ruleMatcher->branch_to_string(bi) << ": "
    //              << ruleMatcher->resourcesForCubeBranch(norm(catom->position), bi) << endl;
    //     }
    }

    // if (ruleMatcher->isInMeshOrSandbox(norm(catom->position))) {
    //     if (not ruleMatcher->isTileRoot(norm(catom->position))) {
    //         cout << "branch: " << ruleMatcher->branch_to_string(branch)
    //              << " -> " << ruleMatcher->getBranchUnitOffset(branch) << endl;
    //     }

    //     cout << "coordinatorPos: " << coordinatorPos << endl;
    //     cout << "incidentBranchesToRootAreComplete: " << incidentBranchesToRootAreComplete(coordinatorPos) << endl;
    //     cout << "isSupportModule: " << ruleMatcher->isSupportModule(sbnorm(catom->position)) << endl;
    //     cout << "tileRootPosForMeshPos: " << getTileRootPosition(catom->position) << endl;
    // } else {
    //     cout << "incidentBranchesToRootAreComplete: " << incidentBranchesToRootAreComplete(denorm(ruleMatcher->getNearestTileRootPosition(norm(catom->position)))) << endl;
    // }

    // cout << "nearestCoordinatorPos: " << denorm(ruleMatcher->getNearestTileRootPosition(norm(catom->position))) << endl;
    // cout << "role: " << MeshRuleMatcher::roleToString(role) << endl;
    // cout << "localNeighborhood: " << getMeshLocalNeighborhoodState() << endl;
    // Cell3DPosition nextHop;
    // matchLocalRules(catom->getLocalNeighborhoodState(), catom->position,
    //                 targetPosition, coordinatorPos, step, lastVisitedEPL, nextHop, true);
    // cout << "nextHop: " << getTileRelativePosition() << " -> " << nextHop << endl;
    cout << "isInGrid: " << ruleMatcher->isInGrid(norm(catom->position)) << endl;
    cout << "isInMesh: " << ruleMatcher->isInMesh(norm(catom->position)) << endl;
    cout << "isInMeshOrSandbox: "<<ruleMatcher->isInMeshOrSandbox(norm(catom->position)) <<endl;
    // cout << "targetPosition: " << targetPosition;
    // if (targetPosition != Cell3DPosition(0,0,0)
    //     and ruleMatcher->isInMesh(norm(targetPosition))
    //     and MeshRuleMatcher::getComponentForPosition(targetPosition - coordinatorPos) != -1)
    //     cout << "[" << MeshRuleMatcher::component_to_string(
    //         static_cast<MeshComponent>(MeshRuleMatcher::getComponentForPosition(targetPosition - coordinatorPos))) << "]";
    // cout << endl;

    // cout << "matchingLocalRule: " << matchingLocalRule << endl;
    // cout << "greenLightIsOn: " << greenLightIsOn << endl;
    // cout << "pivotPosition: " << pivotPosition << endl;
    // cout << "RModuleRequestedMotion: " << RModuleRequestedMotion << endl;
    // cout << "--- END " << *catom << "---" << endl;
}

void MeshAssemblyBlockCode::startup() {
    stringstream info;
    info << "Starting ";
    initialized = true;
    startTime = scheduler->now();

    if (not sandboxInitialized)
        initializeSandbox();

    coordinatorPos =
        denorm(ruleMatcher->getNearestTileRootPosition(norm(catom->position)));

    // Need to initialize target light for sandbox modules at algorithm start
    if (ruleMatcher->isInSandbox(norm(catom->position))) {
        // EPLPivots will appear with a module on its EPL
        // if (ruleMatcher->isEPLPivotModule(norm(catom->position))) {
        //     SET_GREEN_LIGHT(false);
        // } else {
        // All other modules should be green (set by default)
            SET_GREEN_LIGHT(true);
        // }

    }

    // Do stuff
    if (catom->position == (getEntryPointForMeshComponent(MeshComponent::R)
                            // CONTINUOUS FEEDING
                            + Cell3DPosition(0, 1, 0))
        and coordinatorPos[2] == meshSeedPosition[2]
        and lattice->isFree(coordinatorPos)) {

        // Determine EPL and set as last visited
        lastVisitedEPL = getEntryPointLocationForCell(catom->position);
        VS_ASSERT(lastVisitedEPL != -1);

        // Catom is one of the future ground tile roots waiting on R_EPL
        role = FreeAgent;

        if (coordinatorPos == meshSeedPosition) {
            targetPosition = coordinatorPos;

            // Delay the algorithm start
            getScheduler()->schedule(
                new InterruptionEvent(getScheduler()->now() +
                                      (getRoundDuration()),
                                      catom, IT_MODE_ALGORITHM_START));
        }

        // others are waiting for horizontal branches leading to their tile to be completed
        return;

    } else if (ruleMatcher->isVerticalBranchTip(norm(catom->position))) {
        // Catom is one of the sandbox catoms here for decoration but also for actuating
        //  for incoming modules
        role = ActiveBeamTip; // nothing to be done, wait for tPos requests

        // Add z = B to ensure that level -1 catoms are handled properly
        short bi = ruleMatcher->determineBranchForPosition(norm(catom->position));
        VS_ASSERT_MSG(bi >= 0 and bi < N_INC_BRANCHES, "cannot determine branch.");
        branch = static_cast<BranchIndex>(bi);
    } else if (meshSeedPosition[2] - catom->position[2] > 1) {
        role = PassiveBeam; // nothing to be done here for visual decoration only

        // Add z = B to ensure that level -1 catoms are handled properly
        short bi = ruleMatcher->getBranchIndexForNonRootPosition(norm(catom->position)
                                                                 + Cell3DPosition(0, 0, B));
        VS_ASSERT_MSG(bi >= 0 and bi < N_INC_BRANCHES, "cannot determine branch.");
        branch = static_cast<BranchIndex>(bi);
    } else {
        // Catom is active module summoned from the sandbox and that will be used for scaffolding
        role = FreeAgent;

        // Determine EPL and set as last visited
        lastVisitedEPL = getEntryPointLocationForCell(catom->position);
        VS_ASSERT(lastVisitedEPL != -1);

        requestTargetCellFromTileRoot();
        // if (not requestTargetCellFromTileRoot() )
        // VS_ASSERT_MSG(false, "meshAssembly: spawned module cannot be without a delegate coordinator in its vicinity.");
    }
}

const Cell3DPosition
MeshAssemblyBlockCode::norm(const Cell3DPosition& pos) {
    return pos - meshSeedPosition;
}


const Cell3DPosition
MeshAssemblyBlockCode::sbnorm(const Cell3DPosition& pos) {
    Cell3DPosition meshPos = norm(pos) + Cell3DPosition(0,0,B);
    Cell3DPosition fixPos;
    fixPos.pt[0] = (meshPos[0] >= (int)X_MAX ? meshPos[0] - B : meshPos[0]);
    fixPos.pt[1] = (meshPos[1] >= (int)Y_MAX ? meshPos[1] - B : meshPos[1]);
    fixPos.pt[2] = (meshPos[2] >= (int)Z_MAX ? meshPos[2] - B : meshPos[2]);

    // cout << "sbnorm(" << pos << ") = " << fixPos << "\t";

    return fixPos;
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
MeshAssemblyBlockCode::sbdenorm(const Cell3DPosition& pos) {
    return denorm(pos) - (pos[2] < meshSeedPosition[2] ? Cell3DPosition(0,0,0) : Cell3DPosition(0,0,B));
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

        case EVENT_ADD_NEIGHBOR: {
            uint64_t face = Catoms3DWorld::getWorld()->lattice->
                getOppositeDirection((std::static_pointer_cast<AddNeighborEvent>(pev))
                                     ->face);
            const Cell3DPosition& pos = catom->getNeighborBlock(face)->position;

            getScheduler()->trace(" ADD_NEIGHBOR ", catom->blockId, MAGENTA);

            if (role != FreeAgent) {
                // MAYBE: CANNOT GUARANTEE THAT
                // // FIXME: Bugs in the motion coordination protocol
                // VS_ASSERT(greenLightIsOn
                //           // New neighbor is catom that has just been actuated by this m
                //           or actuationTargetPos == pos
                //           // During sandbox init
                //           or ruleMatcher->isEPLPivotModule(norm(catom->position))
                //           // Module just got into position
                //           or addNeighborToProcess > 0);

                if (addNeighborToProcess > 0) addNeighborToProcess--;

                if (not rotating) {
                    if (not ruleMatcher->isInMeshOrSandbox(norm(pos))) {
                        // Neighbor is not a module in terminal position
                        SET_GREEN_LIGHT(false);
                    }

                    // In case tile insertion ready but there was no module on EPL at the
                    //  time echeck that new module is on EPL,
                    //  send it a TileInsertionReadyMessage
                    if (tileInsertionPending
                        and pos == (catom->position + Cell3DPosition(0,0,1))) {
                        P2PNetworkInterface* itf = catom->getInterface(pos);
                        VS_ASSERT(itf and itf->isConnected());
                        sendMessage(new TileInsertionReadyMessage(), itf, MSG_DELAY_MC, 0);
                        tileInsertionPending = false;
                    }
                }

            } else if (role == FreeAgent) {
                if (matchingLocalRule) {
                    matchRulesAndProbeGreenLight();
                }
            }
            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            if (not rotating) {
                if (role != FreeAgent) {
                    uint64_t face = Catoms3DWorld::getWorld()->lattice->getOppositeDirection((std::static_pointer_cast<RemoveNeighborEvent>(pev))->face);

                    Cell3DPosition pos;
                    if (catom->getNeighborPos(face, pos)
                        // catom not actuating
                        and (catom->getState() == BuildingBlock::State::ALIVE)) {
                        SET_GREEN_LIGHT(true);
                    }
                } else if (matchingLocalRule) {
                    matchRulesAndProbeGreenLight();
                }
            }

            break;
        }
        case EVENT_PIVOT_ACTUATION_START: {
            std::shared_ptr<PivotActuationStartEvent> pase = std::static_pointer_cast
                <PivotActuationStartEvent>(pev);

            // A free agent module should never be used as pivot (for now)
            VS_ASSERT(role != FreeAgent);

            if (greenLightIsOn) {
                // cout << *catom << endl;
                // VS_ASSERT(false); // FIXME: should never happen, light should be red already
                SET_GREEN_LIGHT(false);
            }

            catom->getNeighborPos(pase->toConP, actuationTargetPos);

            // console << " started actuating for module #" << pase->mobile->blockId << "\n";
        } break;

        case EVENT_PIVOT_ACTUATION_END: {
            // std::shared_ptr<PivotActuationEndEvent> paee = std::static_pointer_cast
            //     <PivotActuationEndEvent>(pev);

            // console << " finished actuating for module #" << paee->mobile->blockId << "\n";
        } break;

        case EVENT_ROTATION3D_START:
            VS_ASSERT(catom->pivot);
            pivotPosition = catom->pivot->position;
            randomRotationTimeStart = scheduler->now();
            break;
        case EVENT_ROTATION3D_END: {
            // OUTPUT << "rrt: " << scheduler->now() - randomRotationTimeStart << endl;
            getScheduler()->trace(" ROTATION3D_END ", catom->blockId, MAGENTA);
            // console << "Rotation to " << catom->position << " over" << "\n";
            rotating = false;
            step++;
            if (catom->position == targetPosition and not isOnEntryPoint(catom->position)) {
                static int nbModulesInShape = 0;
                nbModulesInShape++;

                role = ruleMatcher->getRoleForPosition(norm(catom->position));
                catom->setColor(ruleMatcher->getColorForPosition(norm(catom->position)));

                const auto& nCells = lattice->getActiveNeighborCells(catom->position);

                // Reset neighbors to process count
                VS_ASSERT(addNeighborToProcess == 0);
                addNeighborToProcess = nCells.size();

                bool shouldTurnRed = false;
                for (const Cell3DPosition& nCell : nCells) {
                    // Check whether or not neighbor is in mesh or sandbox
                    //  if not, module must be a freeagent that was waiting
                    //  for this catom to get into place.
                    if (not ruleMatcher->isInMeshOrSandbox(norm(nCell))) {
                        // In that case catom should turn red immediatly
                        shouldTurnRed = true;
                        break;
                    }
                }

                moduleAwaitingGo = false;
                SET_GREEN_LIGHT(not shouldTurnRed);

                // Inform pivot that motion sequence is over and that it can turn green
                P2PNetworkInterface* pivotItf = catom->getInterface(pivotPosition);

                VS_ASSERT(pivotItf and pivotItf->isConnected());
                sendMessage(new FinalTargetReachedMessage(catom->position),
                            pivotItf, MSG_DELAY_MC, 0);

                // Check algorithm termination
                // Cube construction ending depends on parity of the dimension of the cube

                int dimension = ruleMatcher->getCubeDimension();
                int zmax = (dimension - 1) * B + meshSeedPosition[2];
                int a = (dimension - 1) / 2;
                const Cell3DPosition &last = dimension % 2 == 0 ?
                    Cell3DPosition(a * B + B / 2 + meshSeedPosition[0],
                                   a * B + B / 2,
                                   zmax)
                    : Cell3DPosition(a * B + meshSeedPosition[0] - 1,
                                     a * B + meshSeedPosition[1] - 1,
                                     zmax);

                if (// catom is top level module
                    catom->position == last) {
                    int ts = round(getScheduler()->now() / getRoundDuration());
                    cerr << ruleMatcher->getCubeDimension()
                         << "-CUBE CONSTRUCTION OVER AT TimeStep = "
                         << ts << " with " << lattice->nbModules << " modules in total"
                         << " including " << nbModulesInShape << " in the shape" << endl;
                    cout << "main: " << ruleMatcher->getCubeDimension() << " " << ts
                         << " " << lattice->nbModules << " "
                         << nbModulesInShape << endl;

                    constructionOver = true;
                }

                // STAT EXPORT
                // OUTPUT << "nbCatomsInPlace:\t" << (int)round(scheduler->now() / getRoundDuration()) << "\t" << ++nbCatomsInPlace << endl;

                if (ruleMatcher->isVerticalBranchTip(norm(catom->position))) {
                    coordinatorPos =
                        denorm(ruleMatcher->getNearestTileRootPosition(norm(catom->position)));
                }

                if (ruleMatcher->isTileRoot(norm(catom->position)))
                    initializeTileRoot();
                else if (ruleMatcher->isSupportModule(norm(catom->position))) {
                    initializeSupportModule();
                } else {
                    BranchIndex bi =
                        ruleMatcher->getBranchIndexForNonRootPosition(norm(targetPosition));
                    branch = bi;
                    VS_ASSERT_MSG(bi >= 0 and bi < N_BRANCHES, "cannot determine branch.");

                    stringstream info;
                    info << " assigned to branch " << ruleMatcher->branch_to_string(bi);
                    scheduler->trace(info.str(),catom->blockId, CYAN);

                    const Cell3DPosition& nextPosAlongBranch =
                        catom->position + ruleMatcher->getBranchUnitOffset(bi);

                    // Coordinate to let the last arrived branch continue the construction
                    if (ruleMatcher->isTileRoot(norm(nextPosAlongBranch))) {
                        if (incidentBranchesToRootAreComplete(nextPosAlongBranch)) {
                            // lattice->highlightCell(nextPosAlongBranch, BLUE);
                            // cout << "Some branches are missing around "
                            //      << nextPosAlongBranch << endl;

                            // Notify future tile root as position should be filled
                            P2PNetworkInterface* nextHopItf = NULL;


                            // Send message down branch, next module is likely
                            //  not on EPL yet, it will use TileInsertionPending = true
                            if (bi == RevZBranch
                                or (bi == RZBranch
                                    and ruleMatcher->isOnYOppBorder(norm(nextPosAlongBranch))
                                    and nextPosAlongBranch[2] > meshSeedPosition[2]
                                    and (nextPosAlongBranch[2] / B) % 2 == 1)
                                or (bi == LZBranch
                                    and ruleMatcher->isOnXOppBorder(norm(nextPosAlongBranch))
                                    and nextPosAlongBranch[2] > meshSeedPosition[2]
                                    and (nextPosAlongBranch[2] / B) % 2 == 1)
                                or (bi == ZBranch
                                    and ruleMatcher->isOnXOppBorder(norm(nextPosAlongBranch))
                                    and ruleMatcher->isOnYOppBorder(norm(nextPosAlongBranch))
                                    and nextPosAlongBranch[2] > meshSeedPosition[2]
                                    and (nextPosAlongBranch[2] / B) % 2 == 0)) {
                                nextHopItf =
                                    catom->getInterface(catom->position
                                                        -ruleMatcher->getBranchUnitOffset(bi));
                            } else {
                                for (const auto& pos :
                                         lattice->getActiveNeighborCells(catom->position)) {
                                    if (ruleMatcher->isOnZBranch(norm(pos))
                                        or (ruleMatcher->isOnYBorder(norm(nextPosAlongBranch))
                                            and (nextPosAlongBranch[2] / B) % 2 == 0
                                            and ruleMatcher->isOnLZBranch(norm(pos)))
                                        or (ruleMatcher->isOnXBorder(norm(nextPosAlongBranch))
                                            and (nextPosAlongBranch[2] / B) % 2 == 0
                                            and ruleMatcher->isOnRZBranch(norm(pos)))) {
                                        nextHopItf = catom->getInterface(pos);
                                        break;
                                    }
                                }
                            }

                            VS_ASSERT(nextHopItf);
                            sendMessage(new TileInsertionReadyMessage(),
                                        nextHopItf, MSG_DELAY_MC, 0);
                            log_send_message();
                        } else{
                            stringstream info;
                            info << " not ready to send coordinator ready"
                                 << " - NP: " << nextPosAlongBranch << " : "
                                 << ruleMatcher->isTileRoot(norm(nextPosAlongBranch));
                            if (ruleMatcher->isTileRoot(norm(nextPosAlongBranch)))
                                info << incidentBranchesToRootAreComplete(nextPosAlongBranch);
                            scheduler->trace(info.str(), catom->blockId, RED);
                        }
                    }
                }
            } else {
                if (catom->position == targetPosition and isOnEntryPoint(catom->position)) {
                    // Determine EPL and set as last visited
                    lastVisitedEPL = getEntryPointLocationForCell(catom->position);
                    VS_ASSERT(lastVisitedEPL != -1);

                    // Update tile belonging
                    coordinatorPos = denorm(ruleMatcher->
                                            getNearestTileRootPosition(norm(catom->position)));

                    // Reset step counter for matching a new ruleset
                    step = 1;

                    // Check that that new tile root is in place and if absent,
                    //  wait for above layer XY modules to notify completion of previous tiles
                    if (lattice->isFree(coordinatorPos)) { //FIXME: NON-LOCAL!
                        if (ruleMatcher->isOnXCubeBorder(norm(coordinatorPos))
                            and ruleMatcher->isOnYCubeBorder(norm(coordinatorPos))
                            and coordinatorPos[2] == meshSeedPosition[2]
                            and incidentBranchesToRootAreComplete(coordinatorPos)
                            and claimedTileRoots.find(coordinatorPos)==claimedTileRoots.end()) {
                            targetPosition = coordinatorPos;
                            stringstream info;
                            info << " claims coordinator position at " << targetPosition;
                            claimedTileRoots.insert(coordinatorPos);
                            scheduler->trace(info.str(), catom->blockId, GREY);
                        } else {
                            // catom->setColor(WHITE);
                            // lattice->highlightCell(coordinatorPos, WHITE);
                            return;
                        }
                    } else {
                        // Otherwise ask it for a new targetPosition
                        // catom->setColor(BLACK);
                        if (not requestTargetCellFromTileRoot()) {
                            catom->setColor(BLACK);
                            VS_ASSERT_MSG(false, "arriving module cannot be without delegate coordinator in its vicinity.");
                        }

                        return;
                    }
                }

                matchRulesAndProbeGreenLight();
            }
        } break;

        case EVENT_TAP: {
        } break;

        case EVENT_INTERRUPTION: {
            std::shared_ptr<InterruptionEvent> itev =
                std::static_pointer_cast<InterruptionEvent>(pev);

            switch(itev->mode) {

                case IT_MODE_TILEROOT_ACTIVATION: {
                    // Only introduce catoms if on the lower tile level
                    if (catom->position[2] == meshSeedPosition[2]) {
                        feedIncidentBranches();

                        if (not constructionOver)
                            getScheduler()->schedule(
                                new InterruptionEvent(getScheduler()->now() +
                                                      (getRoundDuration()),
                                                      catom, IT_MODE_TILEROOT_ACTIVATION));
                    }
                } break;

                case IT_MODE_ALGORITHM_START:
                    matchRulesAndProbeGreenLight(); // the seed starts the algorithm
                    break;

                case IT_MODE_FINDING_PIVOT:
                    // VS_ASSERT(++notFindingPivotCount < 10);
                    matchRulesAndProbeGreenLight(); // the seed starts the algorithm
                    catom->setColor(MAGENTA);
                    break;
            }
        }
    }
}

short MeshAssemblyBlockCode::getEntryPointLocationForCell(const Cell3DPosition& pos) {
    for (int i = 0; i < 12; i++)
        if (pos == denorm(ruleMatcher->getNearestTileRootPosition(norm(pos)))
            + entryPointRelativePos[i])
            return i + RevZ_EPL;

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
                    getEntryPointForMeshComponent(mc), YELLOW);
}

bool MeshAssemblyBlockCode::handleModuleInsertionToIncidentBranch(BranchIndex bid) {
    // Introduce new catoms
    // cout << "[t-" << scheduler->now() << "] catom introduced" << endl;
    const Cell3DPosition& entryPoint = getEntryPointForModuleOnIncidentBranch(bid);
    if (lattice->isFree(entryPoint)) {
        world->addBlock(0, buildNewBlockCode, entryPoint, YELLOW);
        catomsSpawnedToVBranch[bid]++;
        return true;
    } else {
        cout << entryPoint << endl;
        cout << lattice->isFree(entryPoint) << endl;
        cerr << "bid: " << bid << endl;
        VS_ASSERT(false);
    }

    return false;
}

const Cell3DPosition
MeshAssemblyBlockCode::getEntryPointForModuleOnIncidentBranch(BranchIndex bid) {
    MeshComponent epl;
    switch(bid) {
        case RevZBranch: epl = Z_EPL; break;
        case ZBranch: epl = RevZ_EPL; break;
            // if (catomsSpawnedToVBranch[ZBranch] == 0) return getEntryPointPosition(Z_L_EPL);
            // else if (catomsSpawnedToVBranch[ZBranch] == 1)return getEntryPointPosition(Z_R_EPL);
            // else return getEntryPointPosition(Z_EPL);
        case LZBranch: epl = RZ_EPL; break;
            // if (catomsSpawnedToVBranch[LZBranch] == 0) return getEntryPointPosition(LZ_R_EPL);
            // else return getEntryPointPosition(LZ_EPL);
        case RZBranch: epl = LZ_EPL; break;
            // if (catomsSpawnedToVBranch[RZBranch] == 0) return getEntryPointPosition(RZ_R_EPL                                                                                 );
            // else return getEntryPointPosition(RZ_EPL);
        default: throw NotImplementedException("getEntryPointForModuleOnIncidentBranch: invalid bid");
    }

    return getEntryPointPosition(epl);
}

bool MeshAssemblyBlockCode::
incidentBranchesToRootAreComplete(const Cell3DPosition& pos) {
    VS_ASSERT(ruleMatcher->isInMesh(norm(pos))
              and ruleMatcher->isTileRoot(norm(pos)));

    // cout << "ibtrac for " << pos << ": " << endl;

    for (int i = 0; i < N_INC_BRANCHES; i++) {
        if (!isIncidentBranchTipInPlace(pos, static_cast<BranchIndex>(i))) {
            // cout << ruleMatcher->branch_to_string(static_cast<BranchIndex>(i)) << "NOT in place!!!" << endl;
            return false;
        }
        // cout << ruleMatcher->branch_to_string(static_cast<BranchIndex>(i)) << " in place" << endl;
    }

    return true;
}

bool MeshAssemblyBlockCode::
isIncidentBranchTipInPlace(const Cell3DPosition& trp, BranchIndex bi) {
    const Cell3DPosition& tipp = trp + incidentTipRelativePos[bi];
    // cout << ruleMatcher->branch_to_string(bi) << " - tipp: " << tipp << endl;
    // cout << "gtrpfmp: " << denorm(ruleMatcher->getTileRootPositionForMeshPosition(norm(tipp))) << endl;
    return (not ruleMatcher->isInCube(ruleMatcher->
                                      getTileRootPositionForMeshPosition(norm(tipp))))
        or ((bi == XBranch or bi == YBranch)
            and ((bi == XBranch and ruleMatcher->isOnYBorder(norm(trp)))
                 or (bi == YBranch and ruleMatcher->isOnXBorder(norm(trp)))))
        or lattice->cellHasBlock(tipp);
}

void MeshAssemblyBlockCode::scheduleRotationTo(const Cell3DPosition& pos,
                                               Catoms3DBlock* pivot = NULL) {
    try {
        if (not pivot) pivot = customFindMotionPivot(catom, pos);

        // OUTPUT << "mvmt: " << round((scheduler->now()) / getRoundDuration()) << "\t" << endl;
        // cout << "[t-" << scheduler->now() << "] rotation scheduled" << endl;
        scheduler->schedule(new Rotation3DStartEvent(getScheduler()->now(),
                                                     catom, pivot, pos,
                                                     RotationLinkType::HexaFace, false));
#ifdef INTERACTIVE_MODE
        awaitKeyPressed();
#endif
    } catch (const NoAvailableRotationPivotException& e_piv) {
        cerr << e_piv.what();
        cerr << "target position: " << pos << endl;
        catom->setColor(MAGENTA);
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
    // OUTPUT << "root: " << (int)(round((scheduler->now() - t0) / getRoundDuration())) << "\t" << norm(catom->position) << endl;

    // Determine how many branches need to grow from here
    // and initialize growth data structures
    for (short bi = 0; bi < N_BRANCHES; bi++) {
        catomsReqByBranch[bi] = ruleMatcher->
            resourcesForCubeBranch(norm(catom->position), (BranchIndex)bi);
        if (catomsReqByBranch[bi] == 0) catomsReqByBranch[bi] = -1;
    }

    // Inspect each incoming vertical branch to see where catoms are ready to take part in
    //  the construction of the tile
    for (const Cell3DPosition& nPos : lattice->getActiveNeighborCells(catom->position)) {
        if (ruleMatcher->isVerticalBranchTip(norm(nPos))) {
            BranchIndex bi = ruleMatcher->getBranchIndexForNonRootPosition(sbnorm(nPos));
            P2PNetworkInterface* nItf = catom->getInterface(nPos);
            VS_ASSERT(nItf and nItf->isConnected());
            Cell3DPosition dstPos = catom->position + getEntryPointRelativePos(
                ruleMatcher->getTargetEPLComponentForBranch(bi));
            sendMessage(new CoordinatorReadyMessage(catom->position, dstPos),
                        nItf, MSG_DELAY_MC, 0);
        }
    }

    // Populate EPLPivots to be used by ground modules
    const Cell3DPosition EPLPivotPos[4] = {
        Cell3DPosition(0, 0, -2), Cell3DPosition(2, 2, -2),
        Cell3DPosition(2, 0, -2), Cell3DPosition(0, 2, -2)
    };

    for (short i = 0; i < 4; i++) {
        BuildingBlock* EPLPivot = lattice->getBlock(catom->position + EPLPivotPos[i]);

        if (EPLPivot) {
            EPLPivotBC[i] = static_cast<MeshAssemblyBlockCode*>(EPLPivot->blockCode);
            numIncidentVerticalBranches++;
        } else EPLPivotBC[i] =  NULL;
    }

    // Initialize construction queue from here
    constructionQueue = buildConstructionQueue(catom->position);

    // NOT AN ASSUMPTION ANYMORE
    // for (short bi = 0; bi < XBranch; bi++) VS_ASSERT(EPLPivotBC[bi]); //

    // Initialize Resources Allocation Requirements from sandbox
    sandboxResourcesRequirement.insert(
        make_pair(RevZ_EPL,resourcesForTileThrough(catom->position, RevZ_EPL)));
    sandboxResourcesRequirement.insert(
        make_pair(Z_EPL,resourcesForTileThrough(catom->position, Z_EPL)));
    sandboxResourcesRequirement.insert(
        make_pair(LZ_EPL,resourcesForTileThrough(catom->position, LZ_EPL)));
    sandboxResourcesRequirement.insert(
        make_pair(RZ_EPL,resourcesForTileThrough(catom->position, RZ_EPL)));

    // Schedule next growth iteration (at t + MOVEMENT_DURATION (?) )
    getScheduler()->schedule(
        new InterruptionEvent(getScheduler()->now(),
                              catom, IT_MODE_TILEROOT_ACTIVATION));
}

deque<pair<MeshComponent, MeshComponent>> MeshAssemblyBlockCode::
buildConstructionQueue(const Cell3DPosition& pos) const {
    if (ruleMatcher->getNbIncidentVerticalCubeBranches(norm(pos)) == 4) {
        return buildConstructionQueueWithFourIncidentBranches(pos);
    } else {
        return buildConstructionQueueWithFewerIncidentBranches(pos);
    }
}

deque<pair<MeshComponent, MeshComponent>> MeshAssemblyBlockCode::
buildConstructionQueueWithFourIncidentBranches(const Cell3DPosition& pos) const {
    std::array<int, 6> catomsReqs = {-1,-1,-1,-1,-1,-1};

    for (short bi = 0; bi < N_BRANCHES; bi++) {
        catomsReqs[bi] = ruleMatcher->
            resourcesForCubeBranch(norm(pos), (BranchIndex)bi);
        if (catomsReqs[bi] == 0) catomsReqs[bi] = -1;
    }

    deque<pair<MeshComponent, MeshComponent>> deque;

    std::function fn_isInCube = [this](const Cell3DPosition& p) {
                                    return ruleMatcher->isInCube(p) ;
                                };

    if (catomsReqs[OppYBranch] > 0) deque.push_back({ OPP_Y1, RevZ_EPL });
    if (catomsReqs[OppXBranch] > 0) deque.push_back({ OPP_X1, LZ_EPL });

    deque.push_back({ S_RZ, RZ_EPL});  // 0
    deque.push_back({ S_LZ, LZ_EPL }); // 0

    if (catomsReqs[YBranch] > 0) deque.push_back({ Y_1, Z_EPL }); // 1
    if (catomsReqs[XBranch] > 0) deque.push_back({ X_1, Z_EPL }); // 3

    deque.push_back({ S_Z, LZ_EPL }); // 4
    deque.push_back({ S_RevZ, RZ_EPL }); // 4

    if (catomsReqs[OppYBranch] > 1) deque.push_back({ OPP_Y2, RevZ_EPL });
    if (catomsReqs[OppYBranch] > 2) deque.push_back({ OPP_Y3, RevZ_EPL });
    if (catomsReqs[OppYBranch] > 3) deque.push_back({ OPP_Y4, RevZ_EPL });
    if (catomsReqs[OppYBranch] > 4) deque.push_back({ OPP_Y5, RevZ_EPL });

    if (catomsReqs[OppXBranch] > 1) deque.push_back({ OPP_X2, LZ_EPL });
    if (catomsReqs[OppXBranch] > 2) deque.push_back({ OPP_X3, LZ_EPL });
    if (catomsReqs[OppXBranch] > 3) deque.push_back({ OPP_X4, LZ_EPL });
    if (catomsReqs[OppXBranch] > 4) deque.push_back({ OPP_X5, LZ_EPL });

    if (catomsReqs[YBranch] > 1) deque.push_back({ Y_2, LZ_EPL }); // 5
    if (catomsReqs[XBranch] > 1) deque.push_back({ X_2, RZ_EPL }); // 5
    if (catomsReqs[YBranch] > 2) deque.push_back({ Y_3, LZ_EPL }); // 7
    if (catomsReqs[XBranch] > 2) deque.push_back({ X_3, RZ_EPL }); // 7
    if (catomsReqs[ZBranch] > 0) deque.push_back({ Z_1, Z_EPL }); // 8
    if (catomsReqs[RevZBranch] > 0) deque.push_back({ RevZ_1, RevZ_EPL }); // 8
    if (catomsReqs[YBranch] > 3) deque.push_back({ Y_4, LZ_EPL }); // 9
    if (catomsReqs[XBranch] > 3) deque.push_back({ X_4, RZ_EPL }); // 9
    if (catomsReqs[ZBranch] > 1) deque.push_back({ Z_2, Z_EPL }); // 10
    if (catomsReqs[RevZBranch] > 1) deque.push_back({ RevZ_2, RevZ_EPL }); // 10
    if (catomsReqs[YBranch] > 4) deque.push_back({ Y_5, LZ_EPL }); // 11
    if (catomsReqs[XBranch] > 4) deque.push_back({ X_5, RZ_EPL }); // 11
    if (catomsReqs[ZBranch] > 2) deque.push_back({ Z_3, Z_EPL }); // 12
    if (catomsReqs[RevZBranch] > 2) deque.push_back({ RevZ_3, RevZ_EPL }); // 12
    if (catomsReqs[ZBranch] > 3) deque.push_back({ Z_4, Z_EPL }); // 14
    if (catomsReqs[RevZBranch] > 3) deque.push_back({ RevZ_4, RevZ_EPL }); // 14
    if (catomsReqs[LZBranch] > 0) deque.push_back({ LZ_1, LZ_EPL }); // 14
    if (catomsReqs[RZBranch] > 0) deque.push_back({ RZ_1, RZ_EPL }); // 14
    if (catomsReqs[ZBranch] > 4) deque.push_back({ Z_5, Z_EPL }); // 16
    if (catomsReqs[RevZBranch] > 4) deque.push_back({ RevZ_5, RevZ_EPL }); // 16
    if (catomsReqs[LZBranch] > 1) deque.push_back({ LZ_2, LZ_EPL }); // 16
    if (catomsReqs[RZBranch] > 1) deque.push_back({ RZ_2, RZ_EPL }); // 16
    if (catomsReqs[LZBranch] > 2) deque.push_back({ LZ_3, LZ_EPL }); // 18
    if (catomsReqs[RZBranch] > 2) deque.push_back({ RZ_3, RZ_EPL }); // 18
    if (catomsReqs[LZBranch] > 3) deque.push_back({ LZ_4, LZ_EPL }); // 20
    if (catomsReqs[RZBranch] > 3) deque.push_back({ RZ_4, RZ_EPL }); // 20
    if (catomsReqs[LZBranch] > 4) deque.push_back({ LZ_5, LZ_EPL }); // 22
    if (catomsReqs[RZBranch] > 4) deque.push_back({ RZ_5, RZ_EPL }); // 22

    return deque;
}


deque<pair<MeshComponent, MeshComponent>> MeshAssemblyBlockCode::
buildConstructionQueueWithFewerIncidentBranches(const Cell3DPosition& pos) const {
    // NOTE: This is under the assumption that tiles with fewer than four incident branches
    //  do not need to grow Opp branches for now

    std::array<int, 6> catomsReqs = {-1,-1,-1,-1,-1,-1};

    for (short bi = 0; bi < N_BRANCHES; bi++) {
        catomsReqs[bi] = ruleMatcher->
            resourcesForCubeBranch(norm(pos), (BranchIndex)bi);
        if (catomsReqs[bi] == 0) catomsReqs[bi] = -1;
    }

    deque<pair<MeshComponent, MeshComponent>> deque;

    std::function fn_isInCube = [this](const Cell3DPosition& p) {
                                    return ruleMatcher->isInCube(p) ;
                                };

    int nbIVB = ruleMatcher->getNbIncidentVerticalCubeBranches(norm(pos));

    if (nbIVB == 1
        and ruleMatcher->hasIncidentBranch(norm(pos), RevZBranch, fn_isInCube)) {
        deque.push_back({ S_Z, Z_EPL });

        if (catomsReqs[YBranch] > 0) deque.push_back({ Y_1, Z_EPL });
        if (catomsReqs[XBranch] > 0) deque.push_back({ X_1, Z_EPL });

        if (catomsReqs[YBranch] > 1) deque.push_back({ Y_2, Z_EPL });
        if (catomsReqs[XBranch] > 1) deque.push_back({ X_2, Z_EPL });
        if (catomsReqs[YBranch] > 2) deque.push_back({ Y_3, Z_EPL });
        if (catomsReqs[XBranch] > 2) deque.push_back({ X_3, Z_EPL });
        if (catomsReqs[YBranch] > 3) deque.push_back({ Y_4, Z_EPL });
        if (catomsReqs[XBranch] > 3) deque.push_back({ X_4, Z_EPL });
        if (catomsReqs[YBranch] > 4) deque.push_back({ Y_5, Z_EPL });
        if (catomsReqs[XBranch] > 4) deque.push_back({ X_5, Z_EPL });

        if (catomsReqs[ZBranch] > 0) deque.push_back({ Z_1, Z_EPL });
        if (catomsReqs[ZBranch] > 1) deque.push_back({ Z_2, Z_EPL });
        if (catomsReqs[ZBranch] > 2) deque.push_back({ Z_3, Z_EPL });
        if (catomsReqs[ZBranch] > 3) deque.push_back({ Z_4, Z_EPL });
        if (catomsReqs[ZBranch] > 4) deque.push_back({ Z_5, Z_EPL });
    } else if (nbIVB == 1
               and ruleMatcher->hasIncidentBranch(norm(pos), ZBranch, fn_isInCube)) {
        deque.push_back({ S_RevZ, RevZ_EPL });

        if (catomsReqs[RevZBranch] > 0) deque.push_back({ RevZ_1,RevZ_EPL});
        if (catomsReqs[RevZBranch] > 1) deque.push_back({ RevZ_2,RevZ_EPL});
        if (catomsReqs[RevZBranch] > 2) deque.push_back({ RevZ_3,RevZ_EPL});
        if (catomsReqs[RevZBranch] > 3) deque.push_back({ RevZ_4,RevZ_EPL});
        if (catomsReqs[RevZBranch] > 4) deque.push_back({ RevZ_5,RevZ_EPL});
    } else if (ruleMatcher->hasIncidentBranch(norm(pos), RZBranch, fn_isInCube)
                and ((nbIVB == 1)
                     or (nbIVB == 2 and ruleMatcher->hasIncidentBranch(norm(pos), ZBranch,
                                                                       fn_isInCube)))){
        if (ruleMatcher->hasIncidentBranch(norm(pos), ZBranch, fn_isInCube))
            deque.push_back({ S_RevZ, LZ_EPL });
        deque.push_back({ S_LZ, LZ_EPL });

        if (catomsReqs[YBranch] > 0) deque.push_back({ Y_1, LZ_EPL });
        if (catomsReqs[RevZBranch] > 0) deque.push_back({ RevZ_1,RevZ_EPL});
        if (catomsReqs[YBranch] > 1) deque.push_back({ Y_2, LZ_EPL });
        if (catomsReqs[RevZBranch] > 1) deque.push_back({ RevZ_2,RevZ_EPL});
        if (catomsReqs[YBranch] > 2) deque.push_back({ Y_3, LZ_EPL });
        if (catomsReqs[RevZBranch] > 2) deque.push_back({ RevZ_3,RevZ_EPL});
        if (catomsReqs[YBranch] > 3) deque.push_back({ Y_4, LZ_EPL });
        if (catomsReqs[RevZBranch] > 3) deque.push_back({ RevZ_4,RevZ_EPL});
        if (catomsReqs[YBranch] > 4) deque.push_back({ Y_5, LZ_EPL });
        if (catomsReqs[RevZBranch] > 4) deque.push_back({ RevZ_5,RevZ_EPL});

        if (catomsReqs[LZBranch] > 0) deque.push_back({ LZ_1, LZ_EPL });
        if (catomsReqs[LZBranch] > 1) deque.push_back({ LZ_2, LZ_EPL });
        if (catomsReqs[LZBranch] > 2) deque.push_back({ LZ_3, LZ_EPL });
        if (catomsReqs[LZBranch] > 3) deque.push_back({ LZ_4, LZ_EPL });
        if (catomsReqs[LZBranch] > 4) deque.push_back({ LZ_5, LZ_EPL });
    } else if (ruleMatcher->hasIncidentBranch(norm(pos), LZBranch, fn_isInCube)
               and ((nbIVB == 1) or (nbIVB == 2 and ruleMatcher->
                                     hasIncidentBranch(norm(pos),ZBranch,fn_isInCube)))){
        if (ruleMatcher->hasIncidentBranch(norm(pos), ZBranch, fn_isInCube))
            deque.push_back({ S_RevZ, RZ_EPL });
        deque.push_back({ S_RZ, RZ_EPL });

        if (catomsReqs[XBranch] > 0) deque.push_back({ X_1, RZ_EPL });
        if (catomsReqs[RevZBranch] > 0) deque.push_back({ RevZ_1,RevZ_EPL});
        if (catomsReqs[XBranch] > 1) deque.push_back({ X_2, RZ_EPL });
        if (catomsReqs[RevZBranch] > 1) deque.push_back({ RevZ_2,RevZ_EPL});
        if (catomsReqs[XBranch] > 2) deque.push_back({ X_3, RZ_EPL });
        if (catomsReqs[RevZBranch] > 2) deque.push_back({ RevZ_3,RevZ_EPL});
        if (catomsReqs[XBranch] > 3) deque.push_back({ X_4, RZ_EPL });
        if (catomsReqs[RevZBranch] > 3) deque.push_back({ RevZ_4,RevZ_EPL});
        if (catomsReqs[XBranch] > 4) deque.push_back({ X_5, RZ_EPL });
        if (catomsReqs[RevZBranch] > 4) deque.push_back({ RevZ_5,RevZ_EPL});

        if (catomsReqs[RZBranch] > 0) deque.push_back({ RZ_1, RZ_EPL });
        if (catomsReqs[RZBranch] > 1) deque.push_back({ RZ_2, RZ_EPL });
        if (catomsReqs[RZBranch] > 2) deque.push_back({ RZ_3, RZ_EPL });
        if (catomsReqs[RZBranch] > 3) deque.push_back({ RZ_4, RZ_EPL });
        if (catomsReqs[RZBranch] > 4) deque.push_back({ RZ_5, RZ_EPL });
    } else if (nbIVB == 2
               and (ruleMatcher->hasIncidentBranch(norm(pos), RevZBranch, fn_isInCube)and
                    ruleMatcher->hasIncidentBranch(norm(pos), LZBranch, fn_isInCube))) {
        deque.push_back({ S_RZ, RZ_EPL});
        if (catomsReqs[XBranch] > 0) deque.push_back({ X_1, Z_EPL });
        deque.push_back({ S_Z, Z_EPL });
        if (catomsReqs[YBranch] > 0) deque.push_back({ Y_1, Z_EPL });

        if (catomsReqs[XBranch] > 1) deque.push_back({ X_2, RZ_EPL });
        if (catomsReqs[YBranch] > 1) deque.push_back({ Y_2, Z_EPL });
        if (catomsReqs[XBranch] > 2) deque.push_back({ X_3, RZ_EPL });
        if (catomsReqs[YBranch] > 2) deque.push_back({ Y_3, Z_EPL });
        if (catomsReqs[XBranch] > 3) deque.push_back({ X_4, RZ_EPL });
        if (catomsReqs[YBranch] > 3) deque.push_back({ Y_4, Z_EPL });
        if (catomsReqs[XBranch] > 4) deque.push_back({ X_5, RZ_EPL });
        if (catomsReqs[YBranch] > 4) deque.push_back({ Y_5, Z_EPL });

        if (catomsReqs[RZBranch] > 0) deque.push_back({ RZ_1, RZ_EPL });
        if (catomsReqs[ZBranch] > 0) deque.push_back({ Z_1, Z_EPL });
        if (catomsReqs[RZBranch] > 1) deque.push_back({ RZ_2, RZ_EPL });
        if (catomsReqs[ZBranch] > 1) deque.push_back({ Z_2, Z_EPL });
        if (catomsReqs[RZBranch] > 2) deque.push_back({ RZ_3, RZ_EPL });
        if (catomsReqs[ZBranch] > 2) deque.push_back({ Z_3, Z_EPL });
        if (catomsReqs[RZBranch] > 3) deque.push_back({ RZ_4, RZ_EPL });
        if (catomsReqs[ZBranch] > 3) deque.push_back({ Z_4, Z_EPL });
        if (catomsReqs[RZBranch] > 4) deque.push_back({ RZ_5, RZ_EPL });
        if (catomsReqs[ZBranch] > 4) deque.push_back({ Z_5, Z_EPL });
    } else if (nbIVB == 2
               and (ruleMatcher->hasIncidentBranch(norm(pos), RevZBranch, fn_isInCube)and
                    ruleMatcher->hasIncidentBranch(norm(pos), RZBranch, fn_isInCube))) {
        deque.push_back({ S_LZ, LZ_EPL});
        if (catomsReqs[YBranch] > 0) deque.push_back({ Y_1, Z_EPL });
        deque.push_back({ S_Z, Z_EPL });
        if (catomsReqs[XBranch] > 0) deque.push_back({ X_1, Z_EPL });

        if (catomsReqs[YBranch] > 1) deque.push_back({ Y_2, LZ_EPL });
        if (catomsReqs[XBranch] > 1) deque.push_back({ X_2, Z_EPL });
        if (catomsReqs[YBranch] > 2) deque.push_back({ Y_3, LZ_EPL });
        if (catomsReqs[XBranch] > 2) deque.push_back({ X_3, Z_EPL });
        if (catomsReqs[YBranch] > 3) deque.push_back({ Y_4, LZ_EPL });
        if (catomsReqs[XBranch] > 3) deque.push_back({ X_4, Z_EPL });
        if (catomsReqs[YBranch] > 4) deque.push_back({ Y_5, LZ_EPL });
        if (catomsReqs[XBranch] > 4) deque.push_back({ X_5, Z_EPL });

        if (catomsReqs[LZBranch] > 0) deque.push_back({ LZ_1, LZ_EPL });
        if (catomsReqs[ZBranch] > 0) deque.push_back({ Z_1, Z_EPL });
        if (catomsReqs[LZBranch] > 1) deque.push_back({ LZ_2, LZ_EPL });
        if (catomsReqs[ZBranch] > 1) deque.push_back({ Z_2, Z_EPL });
        if (catomsReqs[LZBranch] > 2) deque.push_back({ LZ_3, LZ_EPL });
        if (catomsReqs[ZBranch] > 2) deque.push_back({ Z_3, Z_EPL });
        if (catomsReqs[LZBranch] > 3) deque.push_back({ LZ_4, LZ_EPL });
        if (catomsReqs[ZBranch] > 3) deque.push_back({ Z_4, Z_EPL });
        if (catomsReqs[LZBranch] > 4) deque.push_back({ LZ_5, LZ_EPL });
        if (catomsReqs[ZBranch] > 4) deque.push_back({ Z_5, Z_EPL });
    }

    return deque;
}

void MeshAssemblyBlockCode::initializeSupportModule() {
    for (const Cell3DPosition& nPos : lattice->getActiveNeighborCells(catom->position)) {
        if (ruleMatcher->isVerticalBranchTip(norm(nPos))) {
            branchTipPos = nPos;
            short bi = ruleMatcher->determineBranchForPosition(norm(nPos));
            VS_ASSERT_MSG(bi >= 0 and bi < N_BRANCHES, "cannot determine branch.");
            branch = static_cast<BranchIndex>(bi);
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
    const Cell3DPosition& nearestTR =
        denorm(ruleMatcher->getNearestTileRootPosition(norm(pos)));

    for (const Cell3DPosition& ep : entryPointRelativePos) {
        if (pos == ep + nearestTR) return true;
    }

    return false;
}

bool MeshAssemblyBlockCode::requestTargetCellFromTileRoot() {
    for (const Cell3DPosition& nPos : lattice->getActiveNeighborCells(catom->position)) {
        if (ruleMatcher->isVerticalBranchTip(norm(nPos))
            or ruleMatcher->isNFromVerticalBranchTip(norm(nPos), 1)
            or (ruleMatcher->isSupportModule(norm(nPos)) and static_cast<MeshAssemblyBlockCode*>(lattice->getBlock(nPos)->blockCode)->role != FreeAgent)) {
            // Module is delegate coordinator
            P2PNetworkInterface* nItf = catom->getInterface(nPos);
            VS_ASSERT(nItf);
            // cout << "[t-" << getScheduler()->now() << "] requesting target cell" << endl;
            sendMessage(new RequestTargetCellMessage(catom->position, coordinatorPos,
                                                     catom->blockId), nItf,MSG_DELAY_MC, 0);
            log_send_message();
            return true;
        } // else Support module not yet in place, wait for it to come online and notify us
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

        // Add waiting tile EPL modules
        // for (int i = 0; i < XBranch; i++) {
        //         MeshComponent epl =ruleMatcher->getDefaultEPLComponentForBranch((BranchIndex)i);
        if (denormPos != meshSeedPosition) // or i != ZBranch) {
            world->addBlock(0, buildNewBlockCode,
                            denormPos + getEntryPointRelativePos(Z_EPL), YELLOW);
        // }
    // }

    }

    sandboxInitialized = true;
    // cout << "round duration: " << getRoundDuration() << endl;
}

std::bitset<12> MeshAssemblyBlockCode::getMeshLocalNeighborhoodState() {
    bitset<12> bitset = {0};
    const vector<Cell3DPosition> localNeighborhood =
        Catoms3DWorld::getWorld()->lattice->getNeighborhood(catom->position);

    for (const Cell3DPosition& nPos : localNeighborhood) {
        P2PNetworkInterface *itf = catom->getInterface(nPos);
        bitset.set(catom->getAbsoluteDirection(nPos),
                   itf->isConnected() and ruleMatcher->isInMeshOrSandbox(norm(nPos)));
    }

    return bitset;
}

void MeshAssemblyBlockCode::matchRulesAndRotate() {
    Cell3DPosition nextPos;
    bool matched = matchLocalRules(getMeshLocalNeighborhoodState(),
                                   catom->position,
                                   targetPosition,
                                   coordinatorPos,
                                   step, lastVisitedEPL, nextPos);

    if (matched) {
        scheduleRotationTo(nextPos);
    } else {
        // Try matching rules again once neighborhood updates
        catom->setColor(GOLD);
        cout << *catom << " is local rule matching" << endl;
        matchingLocalRule = true;
    }
}

void MeshAssemblyBlockCode::feedIncidentBranches() {
    for (int bi = 0; bi < XBranch; bi++) {
        const Cell3DPosition& supportPos = catom->position + ruleMatcher->
            getSupportPositionForPosition(sbnorm(EPLPivotBC[bi]->catom->position));
        Catoms3DBlock* support = static_cast<Catoms3DBlock*>(lattice->getBlock(supportPos));

        MeshComponent epl = ruleMatcher->getDefaultEPLComponentForBranch(
            ruleMatcher->getAlternateBranchIndex((BranchIndex)bi));

        // Only insert if green light on EPL pivot
        if (EPLPivotBC[bi]->greenLightIsOn
            // and if support is ready to receive modules as well
            and (not support or
                 static_cast<MeshAssemblyBlockCode*>(support->blockCode)->greenLightIsOn)
            // and if current resources allocation goal has not been met
            and sandboxResourcesRequirement.find(epl)->second > 0) {
            // sandboxResourcesRequirement.find(epl)->second--;
            handleModuleInsertionToIncidentBranch(static_cast<BranchIndex>(bi));
        }
    }
}

bool MeshAssemblyBlockCode::isAtGroundLevel() {
    return catom->position[2] == meshSeedPosition[2];
}

/************************************************************************
 ************************* MOTION COORDINATION **************************
 ***********************************************************************/

bool MeshAssemblyBlockCode::isAdjacentToPosition(const Cell3DPosition& pos) const {
    return lattice->cellsAreAdjacent(catom->position, pos);
}

Catoms3DBlock* MeshAssemblyBlockCode::
findTargetLightAmongNeighbors(const Cell3DPosition& targetPos,
                              const Cell3DPosition& srcPos) const {
    for (const auto& cell : lattice->getActiveNeighborCells(catom->position)) {
        if (lattice->cellsAreAdjacent(cell, targetPos)
            and ruleMatcher->isInMesh(norm(cell))
            and cell != srcPos
            and Cell3DPosition::compare_ZYX(catom->position, cell)) // TOCHECK
            return static_cast<Catoms3DBlock*>(lattice->getBlock(cell));
    }

    return NULL;
}

Catoms3DBlock* MeshAssemblyBlockCode::
findTargetLightAroundTarget(const Cell3DPosition& targetPos,
                            const Cell3DPosition& finalPos) const {
    cout << catom->position << " seeks light around " << targetPos
         << " to get to " << finalPos;

    bool reverse = Cell3DPosition::compare_ZYX(finalPos, catom->position);
    Cell3DPosition bestCandidate =  reverse ?
        Cell3DPosition(numeric_limits<short>::max(),
                       numeric_limits<short>::max(),
                       numeric_limits<short>::max())
        :
        Cell3DPosition(numeric_limits<short>::min(),
                       numeric_limits<short>::min(),
                       numeric_limits<short>::min());
    for (const auto& cell : lattice->getActiveNeighborCells(targetPos)) {
        if (ruleMatcher->isInMeshOrSandbox(norm(cell))
            and cell != catom->position
            and cell != finalPos
            and (reverse ? Cell3DPosition::compare_ZYX(cell, bestCandidate)
                 : Cell3DPosition::compare_ZYX(bestCandidate, cell) ))
            bestCandidate = cell;
    }

    cout << " and found " << bestCandidate << endl;

    if (bestCandidate[0] == numeric_limits<short>::max()
        or bestCandidate[0] == numeric_limits<short>::min()) return NULL;

    return static_cast<Catoms3DBlock*>(lattice->getBlock(bestCandidate));
}

void MeshAssemblyBlockCode::matchRulesAndProbeGreenLight() {
    Cell3DPosition nextPos;

    // TODO: FIXME: Special ALT case due to collision on Z_EPL/RZ_EPL with iBorders
    if (lastVisitedEPL == Z_EPL
        and targetPosition - coordinatorPos == Cell3DPosition(1,1,0) // S_Z
        and ruleMatcher->isOnXBorder(norm(coordinatorPos))
        and (coordinatorPos[2] / B) % 2 == 0
        and coordinatorPos[2] != meshSeedPosition[2]) {
        lastVisitedEPL = LR_EPL::LR_Z_EPL_ALT;
    } else if (lastVisitedEPL == RZ_EPL
               and targetPosition - coordinatorPos == Cell3DPosition(-1,-1,0) // S_RZ
               and ruleMatcher->isOnXOppBorder(norm(coordinatorPos))
               and (coordinatorPos[2] / B) % 2 == 0
               and coordinatorPos[2] != meshSeedPosition[2]) {
        lastVisitedEPL = LR_EPL::LR_RZ_EPL_ALT;
    }


    bool matched = matchLocalRules(getMeshLocalNeighborhoodState(),
                                   catom->position,
                                   targetPosition,
                                   coordinatorPos,
                                   step, lastVisitedEPL, nextPos);

    if (matched) {
        stepTargetPos = nextPos;
        Catoms3DBlock *pivot = customFindMotionPivot(catom, stepTargetPos);
        stepPivot = pivot;
        catom->setColor(YELLOW);

        // VS_ASSERT(pivot); // FIXME: TODO:
        if (not pivot) {
            notFindingPivot = true;
            getScheduler()->schedule(
                new InterruptionEvent(getScheduler()->now() +
                                      (getRoundDuration()),
                                      catom, IT_MODE_FINDING_PIVOT));
            stringstream info;
            info << " reattempt finding pivot for " << targetPosition;
            scheduler->trace(info.str(),catom->blockId,PINK);
            return;
        }

        notFindingPivot = false; // FIXME: TODO:
        matchingLocalRule = false;

        int finalComponent = ruleMatcher->getComponentForPosition(targetPosition -
                                                                  coordinatorPos);
        VS_ASSERT(finalComponent != -1);
        sendMessage(new ProbePivotLightStateMessage(catom->position, stepTargetPos,
                                                    (MeshComponent)finalComponent),
                    catom->getInterface(pivot->position), MSG_DELAY_MC, 0);

        // Catoms3DBlock* targetLight =
        //     findTargetLightAroundTarget(stepTargetPos,
        //                                 coordinatorPos + ruleMatcher->getPositionForComponent(static_cast<MeshComponent>(finalComponent)));
        // VS_ASSERT(targetLight);
        // sendMessage(new ProbePivotLightStateMessage(catom->position, targetLight->position,
        //                                             stepTargetPos,
        //                                             (MeshComponent)finalComponent),
        //             catom->getInterface(pivot->position), MSG_DELAY_MC, 0);
    } else {
        // Try matching rules again once neighborhood updates
        catom->setColor(BLUE);
        matchingLocalRule = true;
    }
}

Catoms3DBlock* MeshAssemblyBlockCode::customFindMotionPivot(const Catoms3DBlock* m,
                                                            const Cell3DPosition& tPos,
                                                            RotationLinkType faceReq) {
    const auto &allLinkPairs =
        Catoms3DMotionEngine::findPivotLinkPairsForTargetCell(m, tPos, faceReq);

    for (const auto& pair : allLinkPairs) {
        // Additional rule compared to Catoms3DMotionEngine::customFindMotionPivot:
        //  Make sure that pivot is not a FreeAgent (i.e., is part of scaffold)
        if (static_cast<MeshAssemblyBlockCode*>(pair.first->blockCode)->role == FreeAgent)
            continue;

        // cout << "{ " << *pair.first << ", " << *pair.second << " }" << endl;
        if (pair.second->getMRLT() == faceReq or faceReq == RotationLinkType::Any)
            return pair.first;
    }

    return NULL;
}

void MeshAssemblyBlockCode::setGreenLight(bool onoff, int _line_) {
    stringstream info;
    info << " light turned ";

    if (onoff) {
        info << "green: ";
        greenLightIsOn = true;
        catom->setColor(GREEN);

        // Resume flow if needed
        if (moduleAwaitingGo) {
            bool nextToModule = isAdjacentToPosition(awaitingModulePos);

            P2PNetworkInterface* itf = nextToModule ?
                catom->getInterface(awaitingModulePos) :
                // Move the message up the branch
                awaitingModuleProbeItf;

            VS_ASSERT(itf and itf->isConnected());
            sendMessage(new GreenLightIsOnMessage(catom->position, awaitingModulePos),
                        itf, MSG_DELAY_MC, 0);
            moduleAwaitingGo = false;
            awaitingModuleProbeItf = NULL;
        }
    } else {
        info << "red: ";
        greenLightIsOn = false;
        catom->setColor(RED);
    }

    info << _line_;
    getScheduler()->trace(info.str(),catom->blockId, onoff ? GREEN : RED);
}

/************************************************************************
 ******************************** STATS *********************************
 ***********************************************************************/

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
    // updateMsgRate();
    return BlockCode::sendMessage(msg, dest, t0, dt);
}

int MeshAssemblyBlockCode::sendMessage(Message *msg,P2PNetworkInterface *dest,
                                       Time t0,Time dt) {
    OUTPUT << "nbMessages:\t" << round(scheduler->now() / getRoundDuration()) << "\t" << ++nbMessages << endl;
    // updateMsgRate();
    return BlockCode::sendMessage(msg, dest, t0, dt);
}

void MeshAssemblyBlockCode::log_send_message() const {
    // OUTPUT << "lfmsg: " << round((scheduler->now() - startTime) / getRoundDuration()) << "\t" << MeshRuleMatcher::roleToString(role) << endl;
}

BranchIndex
MeshAssemblyBlockCode::findBestBranchIndexForMsgDst(const Cell3DPosition& dst,
                                                    bool upward) const {
    VS_ASSERT_MSG(ruleMatcher->isTileRoot(norm(catom->position)),
                  "findBestBranchIndexForMsgDst: Only coordinator caller is allowed");

    // if (not ruleMatcher->isTileRoot(sbnorm(dst))
    //     and not ruleMatcher->isSupportModule(sbnorm(dst))) {
    // }

    // cout << upward << endl;

    BranchIndex bestBi = ZBranch;
    int bestDist = numeric_limits<int>::max();
    for (int i = 0; i < XBranch; i++) {
        if (upward) {
            // Consider vertical branches of the tile
            if (catomsReqByBranch[i] != -1 and catomsReqByBranch[i] == B - 1
                and dst.dist_euclid(denorm(ruleMatcher->getTileRootAtEndOfBranch(catom->position,(BranchIndex)i))) < bestDist)
                bestBi = (BranchIndex)i;
        } else {
            // cout << "Aqui estamos" << endl;
            // consider parent vertical branches of the Tile
            // Ensure branch tip exists
            Cell3DPosition bTipPos = catom->position - ruleMatcher->getBranchUnitOffset(i);
            // cout << "i: " << ruleMatcher->branch_to_string((BranchIndex)i) << " -- bTipPos: " << bTipPos
            //      << " -- rPos: " << getTileRootPosition(bTipPos)
            //      << " -- dist: " << getTileRootPosition(dst).dist_euclid(getTileRootPosition(bTipPos)) << endl;

            int dist = getTileRootPosition(dst).dist_euclid(getTileRootPosition(bTipPos));
            if (catom->getNeighborBlock(bTipPos) and dist < bestDist) {
                bestBi = (BranchIndex)i;
                bestDist = dist;
            }
        }
    }

    return bestBi;
}

bool MeshAssemblyBlockCode::hasIncidentBranch(BranchIndex bi) const {
    VS_ASSERT_MSG(ruleMatcher->isTileRoot(norm(catom->position)),
                  "hasIncidentBranch: Only coordinator caller is allowed");

    return (not lattice->isFree(catom->position - ruleMatcher->getBranchUnitOffset(bi)));
}

Cell3DPosition MeshAssemblyBlockCode::getTileRootPosition(const Cell3DPosition& pos) const {
        return denorm(ruleMatcher->getTileRootPositionForMeshPosition(norm(pos)));
}

bool MeshAssemblyBlockCode::areOnTheSameBranch(const Cell3DPosition& pos1,
                                               const Cell3DPosition& pos2) const {
    return (getTileRootPosition(pos1) == getTileRootPosition(pos2))
        and (not ruleMatcher->isTileRoot(sbnorm(pos1))
             or ruleMatcher->isTileRoot(sbnorm(pos2)))
        and (ruleMatcher->getBranchIndexForNonRootPosition(sbnorm(pos1)) ==
             ruleMatcher->getBranchIndexForNonRootPosition(sbnorm(pos2)));
}

int MeshAssemblyBlockCode::resourcesForTileThrough(const Cell3DPosition& pos,
                                                   MeshComponent epl) const {
    // cout << "resourcesForTileThrough(" << pos
    //      << ", " << ruleMatcher->component_to_string(epl) << "): " << endl;
    if (not ruleMatcher->isInCube(norm(pos))) {
        // cout << "\tnot in shape" << endl;
        return 0;
    }

    vector<MeshComponent> relevantComponents;
    // Build a vector containg all MeshComponents that must be sourced from epl in the
    //  construction queue of the module
    for (const auto& pair : buildConstructionQueue(pos))
        if (pair.second == epl) relevantComponents.push_back(pair.first);

    short count = 0;

    const Cell3DPosition& trTip =
        ruleMatcher->getTileRootAtEndOfBranch(norm(pos),ruleMatcher->getBranchForEPL(epl));
    if ( (epl == RevZ_EPL and ruleMatcher->isInCube(trTip)
          and ruleMatcher->shouldGrowCubeBranch(norm(pos), RevZBranch)
          and ruleMatcher->getNbIncidentVerticalCubeBranches(trTip) == 4)


         or (epl == RevZ_EPL and ruleMatcher->isInCube(trTip)
             and ruleMatcher->shouldGrowCubeBranch(norm(pos), RevZBranch)
             and ruleMatcher->getNbIncidentVerticalCubeBranches(trTip) == 1)

         or (epl == LZ_EPL and ruleMatcher->isInCube(trTip)
             and ruleMatcher->shouldGrowCubeBranch(norm(pos), LZBranch)
             and ruleMatcher->getNbIncidentVerticalCubeBranches(trTip) == 1)

         or (epl == RZ_EPL and ruleMatcher->isInCube(trTip)
             and ruleMatcher->shouldGrowCubeBranch(norm(pos), RZBranch)
             and ruleMatcher->getNbIncidentVerticalCubeBranches(trTip) == 1)

         or (epl == Z_EPL and ruleMatcher->isInCube(trTip)
             and ruleMatcher->shouldGrowCubeBranch(norm(pos), ZBranch)
             and ruleMatcher->getNbIncidentVerticalCubeBranches(trTip) == 1)

         or (epl == RevZ_EPL and ruleMatcher->isInCube(trTip)
             and ruleMatcher->shouldGrowCubeBranch(norm(pos), RevZBranch)
             and ruleMatcher->shouldGrowCubeBranch(norm(pos), LZBranch)
             and ruleMatcher->getNbIncidentVerticalCubeBranches(trTip) == 2)

         or (epl == LZ_EPL and ruleMatcher->isInCube(trTip)
             and ruleMatcher->shouldGrowCubeBranch(norm(pos), ZBranch)
             and ruleMatcher->shouldGrowCubeBranch(norm(pos), LZBranch)
             and ruleMatcher->getNbIncidentVerticalCubeBranches(trTip) == 2)

         or (epl == RZ_EPL and ruleMatcher->isInCube(trTip)
             and ruleMatcher->shouldGrowCubeBranch(norm(pos), ZBranch)
             and ruleMatcher->shouldGrowCubeBranch(norm(pos), RZBranch)
             and ruleMatcher->getNbIncidentVerticalCubeBranches(trTip) == 2)

         ) {

        // cout << "\tshouldSendTR " << 1 << endl;
        count += 1; // TileRoot to be sent through RevZBranch
    }

    for (const MeshComponent& mc : relevantComponents) {
        const Cell3DPosition& cPos = norm(pos + ruleMatcher->getPositionForComponent(mc));
        if (ruleMatcher->isInCube(cPos) or ruleMatcher->isSupportModule(cPos))
            count++;
    }

    BranchIndex bi = ruleMatcher->getBranchForEPL(epl);
    MeshComponent eplAlt = ruleMatcher->getTargetEPLComponentForBranch(bi);

    // cout << "\tcount: " << count << endl;

    return count + resourcesForTileThrough(denorm(ruleMatcher->getTileRootAtEndOfBranch(
                                                      norm(pos), bi)), eplAlt);
}
