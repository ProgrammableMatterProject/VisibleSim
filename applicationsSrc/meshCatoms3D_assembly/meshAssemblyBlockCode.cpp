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

uint MeshAssemblyBlockCode::X_MAX;
uint MeshAssemblyBlockCode::Y_MAX;
uint MeshAssemblyBlockCode::Z_MAX;
constexpr std::array<Cell3DPosition, 6> MeshAssemblyBlockCode::incidentTipRelativePos;
constexpr std::array<Cell3DPosition, 12> MeshAssemblyBlockCode::entryPointRelativePos;
constexpr Cell3DPosition MeshAssemblyBlockCode::meshSeedPosition;
bID id = 1;

MeshAssemblyBlockCode::MeshAssemblyBlockCode(Catoms3DBlock *host):
    Catoms3DBlockCode(host) {
    scheduler = getScheduler();
    world = BaseSimulator::getWorld();
    lattice = world->lattice;   
    catom = host;
    
    const Cell3DPosition& ub = lattice->getGridUpperBounds();
    // Round down mesh dimensions to multiple of B
    // TODO: Adapt to CSG
    X_MAX = ub[0] - (B - ub[0] % B); 
    Y_MAX = ub[1] - (B - ub[1] % B);
    Z_MAX = ub[2] - (B - ub[2] % B);

    ruleMatcher = new MeshRuleMatcher(X_MAX - meshSeedPosition[0],
                                      Y_MAX - meshSeedPosition[1],
                                      Z_MAX - meshSeedPosition[2], B);
}

MeshAssemblyBlockCode::~MeshAssemblyBlockCode() {
}

void MeshAssemblyBlockCode::onBlockSelected() {
    // Debug:
    // (1) Print details of branch growth plan and previous round
    if (role == Coordinator) {
        cout << "Growth Plan: [ ";
        for (int i = 0; i < 6; i++)
            cout << catomsReqByBranch[i] << ", ";
        cout << " ]" << endl;

        cout << "Last Round: [ ";
        for (int i = 0; i < 6; i++)
            cout << fedCatomOnLastRound[i] << ", ";
        cout << " ]" << endl;

        cout << "Open Positions: [ ";
        for (int i = 0; i < 6; i++)
            cout << endl << "\t\t  "
                 <<(openPositions[i] ? openPositions[i]->config_print() : "NULL") << ", ";
        cout << " ]" << endl;

    
        cout << "Targets for Entry Points: [ ";
        for (int i = 0; i < 8; i++)
            cout << endl << "\t\t  " << targetForEntryPoint[i].config_print() << ", ";
        cout << " ]" << endl;
    }
    
    // catom->setColor(debugColorIndex++);

    cout << "branch: " << branch << endl;
    cout << "coordinatorPos: " << coordinatorPos << endl;
    cout << "role: " << role << endl;
    cout << "localNeighborhood: " << catom->getLocalNeighborhoodState() << endl;
    Cell3DPosition nextHop;
    bool matched = matchLocalRules(catom->getLocalNeighborhoodState(),
                                   catom->position,
                                   targetPosition,
                                   coordinatorPos, step, nextHop);            
    cout << "nextHop: " << getTileRelativePosition() << " -> " << nextHop << endl;
    
    cout << "Possible Rotations: " << endl;
    const vector<std::pair<const Catoms3DMotionRulesLink*, Rotations3D>> allRotations =
        Catoms3DMotionEngine::getAllRotationsForModule(catom);

    for (const auto& rotation : allRotations) {
            cout << *rotation.first << " --> " << rotation.second << endl;
    }
}

void MeshAssemblyBlockCode::startup() {
    stringstream info;
    info << "Starting ";

    coordinatorPos =
            denorm(ruleMatcher->getNearestTileRootPosition(norm(catom->position)));
    
    // Do stuff
    if (catom->position == getEntryPointForMeshComponent(MeshComponent::R)
        and lattice->isFree(coordinatorPos)) {
        targetPosition = coordinatorPos;
        
        // Make incoming vertical branch tips appear already in place if at ground level
        if (coordinatorPos != meshSeedPosition) {
            for (int i = 0; i < XBranch; i++) {
                world->addBlock(++id, buildNewBlockCode,
                                coordinatorPos + incidentTipRelativePos[i], PINK);
                world->addBlock(++id, buildNewBlockCode, coordinatorPos +
                                incidentTipRelativePos[i] + incidentTipRelativePos[i], GREY);
                world->addBlock(++id, buildNewBlockCode,
                                coordinatorPos + incidentTipRelativePos[i]
                                + incidentTipRelativePos[i] + incidentTipRelativePos[i],
                                GREY);
            }
        }
        
        Cell3DPosition nextPos;
        bool matched = matchLocalRules(catom->getLocalNeighborhoodState(),
                                       catom->position,
                                       targetPosition,
                                       coordinatorPos, step, nextPos);
        VS_ASSERT_MSG(matched, "DID NOT FIND RULE TO MATCH.");
        
            VS_ASSERT_MSG(nextPos != catom->position, "DID NOT FIND RULE TO MATCH.");
        scheduleRotationTo(nextPos);
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

        for (const Cell3DPosition& nPos : lattice->getActiveNeighborCells(catom->position)) {
            if (ruleMatcher->isVerticalBranchTip(norm(nPos))
                or ruleMatcher->isTileSupport(norm(nPos))) {
                // Module is delegate coordinator
                P2PNetworkInterface* nItf = catom->getInterface(nPos);
                VS_ASSERT(nItf);
                sendMessage(new RequestTargetCellMessage(catom->position), nItf,
                            MSG_DELAY_MC, 0);
                return;
            }
        }
        
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
            if (catom->position == targetPosition) {
                role = ruleMatcher->getRoleForPosition(norm(catom->position));
                catom->setColor(ruleMatcher->getColorForPosition(norm(catom->position)));

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
                        and lattice->isFree(nextPosAlongBranch)
                        and catom->position[2] == meshSeedPosition[2]) {
                        
                        if (incidentBranchesToRootAreComplete(nextPosAlongBranch)) {
                            lattice->unhighlightCell(nextPosAlongBranch);
                            cout << "Ready to insert tile root at " << nextPosAlongBranch
                                 << endl;
#ifdef INTERACTIVE_MODE
                            awaitKeyPressed();
#endif
                            // Update coordinate system to the one of the next tile
                            coordinatorPos =
                                denorm(ruleMatcher->
                                       getNearestTileRootPosition(norm(catom->position)));
                            
                            world->addBlock(++id, buildNewBlockCode,
                                            getEntryPointForMeshComponent(R), CYAN);
                        } else {
                            lattice->highlightCell(nextPosAlongBranch, YELLOW);
                            cout << "Some branches are missing around " << nextPosAlongBranch
                                 << endl;
#ifdef INTERACTIVE_MODE
                            awaitKeyPressed();
#endif
                        }
                    }
                }
                
            } else {
                Cell3DPosition nextHop;
                bool matched = matchLocalRules(catom->getLocalNeighborhoodState(),
                                               catom->position,
                                               targetPosition,
                                               coordinatorPos, step, nextHop);
                if (not matched) {
                    catom->setColor(RED);
                    cout << "#" << catom->blockId << endl;
                    VS_ASSERT_MSG(matched, "DID NOT FIND RULE TO MATCH.");
                }

                scheduleRotationTo(nextHop);                
            }
                
        } break;            
            
        case EVENT_TAP: {
        } break;

        case EVENT_INTERRUPTION: {            
            std::shared_ptr<InterruptionEvent> itev =
                std::static_pointer_cast<InterruptionEvent>(pev);

            console << "IT Triggered, mode: " << itev->mode << "\n";
            
            switch(itev->mode) {

                case IT_MODE_TILEROOT_ACTIVATION: {
                    if (itCounter == 0) {
                        handleMeshComponentInsertion(S_RZ);
                        handleMeshComponentInsertion(S_LZ);        
                    } else if (itCounter == 1) {
                        handleMeshComponentInsertion(X_1);
                        catomsReqByBranch[XBranch]--;
                        
                        handleMeshComponentInsertion(Y_1);
                        catomsReqByBranch[YBranch]--;
                    } else if (itCounter == 2) {
                        handleMeshComponentInsertion(S_Z);
                        handleMeshComponentInsertion(S_RevZ);
                        fedCatomsOnLastRound = true;
                    } else if (not fedCatomsOnLastRound) {
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
                            
                        if (catomsReqByBranch[ZBranch] > 0 and itCounter > 5) {
                            handleMeshComponentInsertion(static_cast<MeshComponent>
                                                         (Z_1 + B -
                                                          catomsReqByBranch[ZBranch] - 1));
                            catomsReqByBranch[ZBranch]--;
                        }                        

                        if (catomsReqByBranch[RevZBranch] > 0 and itCounter > 5) {
                            handleMeshComponentInsertion(static_cast<MeshComponent>
                                                         (RevZ_1 + B -
                                                          catomsReqByBranch[RevZBranch] - 1));
                            catomsReqByBranch[RevZBranch]--;
                        }

                        if (catomsReqByBranch[LZBranch] > 0 and itCounter > 11) {
                            handleMeshComponentInsertion(static_cast<MeshComponent>
                                                         (LZ_1 + B -
                                                          catomsReqByBranch[LZBranch] - 1));
                            catomsReqByBranch[LZBranch]--;
                        }

                        if (catomsReqByBranch[RZBranch] > 0 and itCounter > 11) {
                            handleMeshComponentInsertion(static_cast<MeshComponent>
                                                         (RZ_1 + B -
                                                          catomsReqByBranch[RZBranch] - 1));
                            catomsReqByBranch[RZBranch]--;
                        }
                        
                        fedCatomsOnLastRound = true;
                    } else {
                        fedCatomsOnLastRound = false;
                    }

                    itCounter++;
                    getScheduler()->schedule(
                        new InterruptionEvent(getScheduler()->now() +
                                              (getRoundDuration() *
                                               (1 + (catom->position[0] * 0.05))),
                                              catom, IT_MODE_TILEROOT_ACTIVATION));
                } break;
            }            
        }
    }
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
        if (pos == catom->position + entryPointRelativePos[i]) return i;
    
    return -1;
} 

const Cell3DPosition MeshAssemblyBlockCode::getEntryPointForMeshComponent(MeshComponent mc) {
    switch(mc) {
        case R: return getEntryPointPosition(Z_Right_EPL);
        case S_Z: return getEntryPointPosition(LZ_EPL);
        case S_RevZ: return getEntryPointPosition(RZ_EPL);
        case S_LZ: return getEntryPointPosition(LZ_Right_EPL);
        case S_RZ: return getEntryPointPosition(RZ_Right_EPL);

        case X_1: return getEntryPointPosition(Z_Right_EPL);
        case X_2: case X_3: case X_4: case X_5: return getEntryPointPosition(RZ_EPL);

        case Y_1: return getEntryPointPosition(Z_Left_EPL);
        case Y_2: case Y_3: case Y_4: case Y_5: return getEntryPointPosition(LZ_EPL);

        case Z_1: case Z_2: case Z_3: case Z_4: case Z_5:
            return getEntryPointPosition(Z_EPL);

        case RevZ_1: case RevZ_2: case RevZ_3: case RevZ_4: case RevZ_5:
            return getEntryPointPosition(RevZ_EPL);

        case LZ_1: case LZ_2: case LZ_3: case LZ_4: case LZ_5:
            return getEntryPointPosition(LZ_EPL);

        case RZ_1: case RZ_2: case RZ_3: case RZ_4: case RZ_5:
            return getEntryPointPosition(RZ_EPL);
    }

    return Cell3DPosition(); // unreachable
}

void MeshAssemblyBlockCode::handleMeshComponentInsertion(MeshComponent mc) {
    // Introduce new catoms
    world->addBlock(++id, buildNewBlockCode,
                    getEntryPointForMeshComponent(mc), ORANGE);

    // Set target position for introduced catom
    targetForEntryPoint
        [getEntryPointLocationForCell(getEntryPointForMeshComponent(mc))] =
        catom->position + ruleMatcher->getPositionForMeshComponent(mc);
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
    return (not ruleMatcher->isInMesh(ruleMatcher->
                                      getTileRootPositionForMeshPosition(norm(tipp))))
        or lattice->cellHasBlock(tipp);
}

void MeshAssemblyBlockCode::scheduleRotationTo(const Cell3DPosition& pos) {
    try {
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
        
    // Determine how many branches need to grow from here
    // and initialize growth data structures
    catomsReqByBranch[ZBranch] = ruleMatcher->
        shouldGrowZBranch(norm(catom->position)) ? B - 1 : -1;
    catomsReqByBranch[RevZBranch] = ruleMatcher->
        shouldGrowRevZBranch(norm(catom->position)) ? B - 1 : -1;
    catomsReqByBranch[LZBranch] = ruleMatcher->
        shouldGrowLZBranch(norm(catom->position)) ? B - 1 : -1;       
    catomsReqByBranch[RZBranch] = ruleMatcher->
        shouldGrowRZBranch(norm(catom->position)) ? B - 1 : -1;
    catomsReqByBranch[XBranch] = ruleMatcher->
        shouldGrowXBranch(norm(catom->position)) ? B - 1 : -1;        
    catomsReqByBranch[YBranch] = ruleMatcher->
        shouldGrowYBranch(norm(catom->position)) ? B - 1 : -1;
        
    // Compute the corresponding list of cells to be filled
    updateOpenPositions();
        
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
MeshAssemblyBlockCode::getEntryPointPosition(EntryPointLocation epl) const {
    return entryPointRelativePos[epl] + coordinatorPos;
}
