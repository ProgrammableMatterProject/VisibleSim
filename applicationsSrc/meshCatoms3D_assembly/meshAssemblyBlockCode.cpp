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
    // Debug:
    // (1) Print details of branch growth plan and previous round
    if (role == Coordinator) {
        cout << "Growth Plan: [ ";
        for (int i = 0; i < 6; i++)
            cout << catomsReqByBranch[i] << ", ";
        cout << " ]" << endl;
    }

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
        and coordinatorPos[2] == meshSeedPosition[2]
        and lattice->isFree(coordinatorPos)) {
        // Catom is one of the future ground tile roots waiting on RZ_EPL
        role = FreeAgent;

        if (coordinatorPos == meshSeedPosition) {
            targetPosition = coordinatorPos;
            // matchRulesAndRotate(); // the seed starts the algorithm
            matchRulesAndProbeGreenLight(); // the seed starts the algorithm
        }

        // others are waiting for horizontal branches leading to their tile to be completed
        return;

    } else if (ruleMatcher->isVerticalBranchTip(norm(catom->position))) {
        // Catom is one of the sandbox catoms here for decoration but also for actuating
        //  for incoming modules
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
        // Catom is active module summoned from the sandbox and that will be used for scaffolding
        role = FreeAgent;
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

        case EVENT_ADD_NEIGHBOR: {
            if (role != FreeAgent
                and awaitingModulePos != Cell3DPosition(-1, -1, -1)
                and not rotating) {
                uint64_t face = Catoms3DWorld::getWorld()->lattice->getOppositeDirection((std::static_pointer_cast<AddNeighborEvent>(pev))->face);
                const Cell3DPosition& pos = catom->getNeighborBlock(face)->position;

                if (not ruleMatcher->isInMesh(norm(pos))) {
                    // Neighbor is module moving on the line
                    greenLightIsOn = false;
                    catom->setColor(RED);
                } else {
                    // Module has taken position and is now pivot
                    setGreenLightAndResumeFlow();
                }
            }

            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            if (not rotating) {
                uint64_t face = Catoms3DWorld::getWorld()->lattice->getOppositeDirection((std::static_pointer_cast<RemoveNeighborEvent>(pev))->face);

                Cell3DPosition pos;
                if (catom->getNeighborPos(face, pos)
                    and (role == FreeAgent
                         and (catom->getState() == BuildingBlock::State::ALIVE
                              // Motion is not blocking for a module coming in
                              or (catom->getState() == BuildingBlock::State::ACTUATING
                                  and ruleMatcher->isInMesh(norm(actuationTargetPos)))) ) ) {
                    setGreenLightAndResumeFlow();
                }
            }

            break;
        }
        case EVENT_PIVOT_ACTUATION_START: {
            std::shared_ptr<PivotActuationStartEvent> pase = std::static_pointer_cast
                <PivotActuationStartEvent>(pev);

            if (greenLightIsOn) greenLightIsOn = false;

            catom->getNeighborPos(pase->toConP, actuationTargetPos);

            console << " started actuating for module #" << pase->mobile->blockId << "\n";
        } break;

        case EVENT_PIVOT_ACTUATION_END: {
            std::shared_ptr<PivotActuationEndEvent> paee = std::static_pointer_cast
                <PivotActuationEndEvent>(pev);

            console << " finished actuating for module #" << paee->mobile->blockId << "\n";
        } break;

        case EVENT_ROTATION3D_END:
            console << "Rotation to " << catom->position << " over" << "\n";

            rotating = false;
        case EVENT_TELEPORTATION_END: {
            step++;
            if (catom->position == targetPosition and not isOnEntryPoint(catom->position)) {
                role = ruleMatcher->getRoleForPosition(norm(catom->position));
                catom->setColor(ruleMatcher->getColorForPosition(norm(catom->position)));

                greenLightIsOn = true;
                moduleAwaitingGo = false;

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
                        // FIXME
                        // sendMessage(new TileInsertionReadyMessage(),
                        //             zBranchTipItf, MSG_DELAY_MC, 0);
                        // log_send_message();
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

                // matchRulesAndRotate();
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
                        feedBranches();

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
        const Cell3DPosition& tPos = coordinatorPos +
            ruleMatcher->getComponentPosition(targetQueueForEPL[idx].front());
        targetQueueForEPL[idx].pop();
        return tPos;
    }

    switch (epl) {
        case RevZ_EPL: return ruleMatcher->getComponentPosition(Z_EPL);
        case RevZ_R_EPL: return ruleMatcher->getComponentPosition(RevZ_R_EPL);
        case RZ_L_EPL: return ruleMatcher->getComponentPosition(RZ_L_EPL);
        case RZ_EPL: return ruleMatcher->getComponentPosition(LZ_EPL);
        case RZ_R_EPL: return ruleMatcher->getComponentPosition(RZ_R_EPL);
        case Z_R_EPL: return ruleMatcher->getComponentPosition(Z_R_EPL);
        case Z_EPL: return ruleMatcher->getComponentPosition(RevZ_EPL);
        case Z_L_EPL: return ruleMatcher->getComponentPosition(Z_L_EPL);
        case LZ_R_EPL: return ruleMatcher->getComponentPosition(RZ_EPL);
        case LZ_EPL: return ruleMatcher->getComponentPosition(RZ_EPL);
        case LZ_L_EPL: return ruleMatcher->getComponentPosition(LZ_R_EPL);
        case RevZ_L_EPL: return ruleMatcher->getComponentPosition(RevZ_L_EPL);
        default:
            cerr << "getNextTargetForEPL(" << epl << ")" << endl;
            VS_ASSERT_MSG(false, "getNextTargetForEPL: input is not an EPL");
    }

    return Cell3DPosition(); // unreachable
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
}

bool MeshAssemblyBlockCode::handleModuleInsertionToBranch(BranchIndex bid) {
    // Introduce new catoms
    // cout << "[t-" << scheduler->now() << "] catom introduced" << endl;
    const Cell3DPosition& entryPoint = getEntryPointForModuleOnBranch(bid);
    if (lattice->isFree(entryPoint)) {
        world->addBlock(0, buildNewBlockCode, entryPoint, ORANGE);
        catomsSpawnedToVBranch[bid]++;
        return true;
    }

    return false;
}

const Cell3DPosition MeshAssemblyBlockCode::getEntryPointForModuleOnBranch(BranchIndex bid) {
    MeshComponent epl;
    switch(bid) {
        case RevZBranch: epl = RevZ_EPL; break;
        case ZBranch: epl = Z_EPL; break;
            // if (catomsSpawnedToVBranch[ZBranch] == 0) return getEntryPointPosition(Z_L_EPL);
            // else if (catomsSpawnedToVBranch[ZBranch] == 1)return getEntryPointPosition(Z_R_EPL);
            // else return getEntryPointPosition(Z_EPL);
        case LZBranch: epl = LZ_EPL; break;
            // if (catomsSpawnedToVBranch[LZBranch] == 0) return getEntryPointPosition(LZ_R_EPL);
            // else return getEntryPointPosition(LZ_EPL);
        case RZBranch: epl = RZ_EPL; break;
            // if (catomsSpawnedToVBranch[RZBranch] == 0) return getEntryPointPosition(RZ_R_EPL                                                                                 );
            // else return getEntryPointPosition(RZ_EPL);
        default: throw NotImplementedException("getEntryPointForModuleOnBranch: invalid bid");
    }

    return getEntryPointPosition(epl);
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

    cout << "initializeTileRoot: " << endl;

    if (norm(catom->position) == Cell3DPosition(0,0,0)) t0 = scheduler->now();
    OUTPUT << "root: " << (int)(round((scheduler->now() - t0) / getRoundDuration())) << "\t" << norm(catom->position) << endl;

    // Determine how many branches need to grow from here
    // and initialize growth data structures
    for (short bi = 0; bi < N_BRANCHES; bi++) {
        catomsReqByBranch[bi] = ruleMatcher->
            shouldGrowPyramidBranch(norm(catom->position), (BranchIndex)bi) ? B - 1 : -1;
    }

    // TODO: Resume Catom flows on all eligible branches by turning the tip lights green
    // This means that upon receiving a probeLightRequest, branch tips and/or supports must
    //  first ask the TR (perhaps the reply can simply be a ProvideTargetCell in that case.
    // If the TR is not present, the catoms turn RED until incoming horizontal branches notify
    //  it of their completion (with an exception for front-left corners).
    // if (not isAtGroundLevel())
        // for (short bi = 0; bi < XBranch; bi++) {
        //     P2PNetworkInterface* nItf = catom->getInterface(
        //         catom->position - ruleMatcher->getBranchUnitOffset(bi));

        // }

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
            or ruleMatcher->isNFromVerticalBranchTip(norm(nPos), 1)
            or ruleMatcher->isTileSupport(norm(nPos))) {
            // Module is delegate coordinator
            P2PNetworkInterface* nItf = catom->getInterface(nPos);
            VS_ASSERT(nItf);
            // cout << "[t-" << getScheduler()->now() << "] requesting target cell" << endl;
            sendMessage(new RequestTargetCellMessage(catom->position), nItf,
                        MSG_DELAY_MC, 0);
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
        for (int i = 0; i < XBranch; i++) {
                MeshComponent epl =ruleMatcher->getDefaultEPLComponentForBranch((BranchIndex)i);
                if (denormPos != meshSeedPosition or i != ZBranch) {
                    world->addBlock(0, buildNewBlockCode,
                                    denormPos + getEntryPointRelativePos(epl), YELLOW);
                }
        }

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

void MeshAssemblyBlockCode::feedBranches() {
    for (int bi = 0; bi < XBranch; bi++) {
        handleModuleInsertionToBranch(static_cast<BranchIndex>(bi));
    }
}

bool MeshAssemblyBlockCode::isAtGroundLevel() {
    return catom->position[2] == meshSeedPosition[2];
}

/************************************************************************
 ************************* MOTION COORDINATION **************************
 ***********************************************************************/

void MeshAssemblyBlockCode::setGreenLightAndResumeFlow() {
    greenLightIsOn = true;
    catom->setColor(GREEN);

    if (moduleAwaitingGo) {
        bool nextToModule = isAdjacentToPosition(awaitingModulePos);
        P2PNetworkInterface* itf = nextToModule ?
            catom->getInterface(awaitingModulePos) :
            catom->getInterface(catom->position
                                + Cell3DPosition(-1, 0, 0));

        sendMessage(new GreenLightIsOnMessage(catom->position, awaitingModulePos),
                    itf, MSG_DELAY_MC, 0);
        moduleAwaitingGo = false;
    }
}

bool MeshAssemblyBlockCode::isAdjacentToPosition(const Cell3DPosition& pos) const {
    return lattice->cellsAreAdjacent(catom->position, pos);
}

Catoms3DBlock* MeshAssemblyBlockCode::
findTargetLightAmongNeighbors(const Cell3DPosition& targetPos) const {
    for (const auto& cell : lattice->getActiveNeighborCells(catom->position)) {
        if (lattice->cellsAreAdjacent(cell, targetPos)
            and ruleMatcher->isInMesh(norm(cell))
            and Cell3DPosition::compare_ZYX(catom->position, cell)) // TOCHECK
            return static_cast<Catoms3DBlock*>(lattice->getBlock(cell));
    }

    return NULL;
}

void MeshAssemblyBlockCode::matchRulesAndProbeGreenLight() {
    Cell3DPosition nextPos;
    bool matched = matchLocalRules(catom->getLocalNeighborhoodState(),
                                   catom->position,
                                   targetPosition,
                                   coordinatorPos, step, nextPos);

    if (not matched) {
        catom->setColor(RED);
        cout << "#" << catom->blockId<< endl;
        VS_ASSERT_MSG(matched, "DID NOT FIND RULE TO MATCH.");
    }

    stepTargetPos = nextPos;
    Catoms3DBlock *pivot = Catoms3DMotionEngine::findMotionPivot(catom, stepTargetPos);
    VS_ASSERT(pivot);

    sendMessage(new ProbePivotLightStateMessage(catom->position, stepTargetPos),
                catom->getInterface(pivot->position), MSG_DELAY_MC, 0);
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
    // OUTPUT << "nbMessages:\t" << round(scheduler->now() / getRoundDuration()) << "\t" << ++nbMessages << endl   ;
    // updateMsgRate();
    return BlockCode::sendMessage(msg, dest, t0, dt);
}

int MeshAssemblyBlockCode::sendMessage(Message *msg,P2PNetworkInterface *dest,
                                       Time t0,Time dt) {
    // OUTPUT << "nbMessages:\t" << round(scheduler->now() / getRoundDuration()) << "\t" << ++nbMessages << endl;
    // updateMsgRate();
    return BlockCode::sendMessage(msg, dest, t0, dt);
}

void MeshAssemblyBlockCode::log_send_message() const {
    OUTPUT << "lfmsg: " << round((scheduler->now() - startTime) / getRoundDuration()) << "\t" << MeshRuleMatcher::roleToString(role) << endl;
}
