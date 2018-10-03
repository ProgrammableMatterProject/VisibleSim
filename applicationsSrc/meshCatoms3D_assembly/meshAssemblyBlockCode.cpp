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

#include "meshAssemblyBlockCode.hpp"

// C3D Motion Engine
#include "messages.hpp"

using namespace Catoms3D;
using namespace MeshCoating;

uint MeshAssemblyBlockCode::X_MAX;
uint MeshAssemblyBlockCode::Y_MAX;
uint MeshAssemblyBlockCode::Z_MAX;
Cell3DPosition MeshAssemblyBlockCode::MeshSeedPosition = Cell3DPosition(1,1,1);
bID id = 1;

MeshAssemblyBlockCode::MeshAssemblyBlockCode(Catoms3DBlock *host):
    Catoms3DBlockCode(host) {
    scheduler = getScheduler();
    world = BaseSimulator::getWorld();
    lattice = world->lattice;   
    catom = host;
    
    const Cell3DPosition& ub = lattice->getGridUpperBounds();
    X_MAX = ub[0];
    Y_MAX = ub[1];
    Z_MAX = ub[2];

    ruleMatcher = new MeshRuleMatcher(X_MAX - MeshSeedPosition[0],
                                      Y_MAX - MeshSeedPosition[1],
                                      Z_MAX - MeshSeedPosition[2], B);
}

MeshAssemblyBlockCode::~MeshAssemblyBlockCode() {
}

void MeshAssemblyBlockCode::onBlockSelected() {
// Debug:
    // (1) Print details of branch growth plan and previous round            
    cout << "Growth Plan: [ ";
    for (int i = 0; i < 6; i++)
        cout << catomReqByBranch[i] << ", ";
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
}

bool MeshAssemblyBlockCode::moduleInSpanningTree(const Cell3DPosition& pos) {
    return target->isInTarget(pos) and lattice->isInGrid(pos);
}

// bool MeshAssemblyBlockCode::isMeshRoot(const Cell3DPosition& pos) {
//     return pos.pt[0] % B == 0 and pos.pt[1] % B == 0 and pos.pt[2] % B == 0;
// }

void MeshAssemblyBlockCode::startup() {
    stringstream info;
    info << "Starting ";

    // Do stuff
    if (ruleMatcher->isTileRoot(normalize_pos(catom->position))) {
        // Switch role
        role = Coordinator;
        
        // Determine how many branches need to grow from here
        // and initialize growth data structures
        catomReqByBranch[ZBranch] = ruleMatcher->
            shouldGrowZBranch(normalize_pos(catom->position)) ? B - 1 : 0;
        catomReqByBranch[RevZBranch] = ruleMatcher->
            shouldGrowRevZBranch(normalize_pos(catom->position)) ? B - 1 : 0;
        catomReqByBranch[Plus45DegZBranch] = ruleMatcher->
            shouldGrowPlus45DegZBranch(normalize_pos(catom->position)) ? B - 1 : 0;       
        catomReqByBranch[Minus45DegZBranch] = ruleMatcher->
            shouldGrowMinus45DegZBranch(normalize_pos(catom->position)) ? B - 1 : 0;
        catomReqByBranch[XBranch] = ruleMatcher->
            shouldGrowXBranch(normalize_pos(catom->position)) ? B - 1 : 0;        
        catomReqByBranch[YBranch] = ruleMatcher->
            shouldGrowYBranch(normalize_pos(catom->position)) ? B - 1 : 0;

        // Compute the corresponding list of cells to be filled
        updateOpenPositions();
        
        // Schedule next growth iteration (at t + MOVEMENT_DURATION (?) )
        getScheduler()->schedule(
            new InterruptionEvent(getScheduler()->now() + 40000, catom,
                                  IT_MODE_TILEROOT_ACTIVATION));
        console << "Scheduled Coordinator IT" << "\n";
    } else {
        role = FreeAgent;
        
        // Ask parent module where it should be headed
        for (const Cell3DPosition& nPos : lattice->getActiveNeighborCells(catom->position)) {
            if (ruleMatcher->isTileRoot(normalize_pos(nPos))) {
                P2PNetworkInterface* nItf = catom->getInterface(nPos);
                VS_ASSERT(nItf);
                
                sendMessage(new RequestTargetCellMessage(), nItf, MSG_DELAY_MC, 0);
                return; // Await answer
            }
        }

        VS_ASSERT_MSG(false, "meshAssembly: spawned module cannot be without a tile root in its vicinity.");
    }
}

const Cell3DPosition
MeshAssemblyBlockCode::normalize_pos(const Cell3DPosition& pos) {    
    return pos - MeshSeedPosition;
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
                
                console << "received " << hMsg->getName() << " from "
                        << message->sourceInterface->hostBlock->blockId
                        << " at " << getScheduler()->now() << "\n";
                hMsg->handle(this);
            } else {
                P2PNetworkInterface * recv_interface = message->destinationInterface;
            
                // Handover to global message handler
                processReceivedMessage(message, recv_interface);
            }
        } break;

        case EVENT_TELEPORTATION_END: {
            // engine->handleRotationEnd();            
            if (catom->position == targetPosition) {
                role = PassiveBeam;
                catom->setColor(BLUE);
            } else {
                Cell3DPosition nextHop;
                if (lattice->cellsAreAdjacent(catom->position, targetPosition))
                    nextHop = targetPosition;
                else
                    nextHop = catom->position + ruleMatcher->getBranchUnitOffset(
                        ruleMatcher->getBranchIndexForNonRootPosition(targetPosition));
        
                // throw NotImplementedException("Movement chaining");
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
                    int numInsertedCatoms = 0;
                    // Policy: Prioritize horizontal growth
                    if ((catomReqByBranch[XBranch] > 0 or catomReqByBranch[YBranch] > 0) and 
                        not (fedCatomOnLastRound[XBranch] or fedCatomOnLastRound[YBranch])) {
                        if (catomReqByBranch[XBranch] > 0) {
                            // FIXME: What if cell has module?
                            
                            world->addBlock(++id, buildNewBlockCode,
                                            catom->position + Cell3DPosition(1, 0, -1),
                                            ORANGE);
                            catomReqByBranch[XBranch]--;
                            fedCatomOnLastRound[XBranch] = true;
                            numInsertedCatoms++;
                        } else {
                            fedCatomOnLastRound[XBranch] = false;
                        }

                        if (catomReqByBranch[YBranch] > 0) {
                            // FIXME: What if cell has module?
                            
                            world->addBlock(++id, buildNewBlockCode,
                                            catom->position + Cell3DPosition(0, 1, -1),
                                            ORANGE);
                            catomReqByBranch[YBranch]--;
                            fedCatomOnLastRound[YBranch] = true;
                            numInsertedCatoms++;
                        } else {
                            fedCatomOnLastRound[XBranch] = false;
                        }
                    } else {
                        fedCatomOnLastRound[XBranch] = false;
                        fedCatomOnLastRound[YBranch] = false;                        
                    }
                    
                    cout << "[t-" << scheduler->now() << "] Round Summary: [ ";
                    for (int i = 0; i < 6; i++) cout << fedCatomOnLastRound[i] << ", ";
                    cout << " ]" << endl;
                    // cout << "(" << catom->blockId << ") Not spawning any catom this round" << endl;

                    getScheduler()->schedule(
                        new InterruptionEvent(getScheduler()->now() + 40001, catom,
                                              IT_MODE_TILEROOT_ACTIVATION));
                    console << "Scheduled Coordinator IT" << "\n";
                } break;
            }
        }
    }
}

void MeshAssemblyBlockCode::updateOpenPositions() {
    for (int i = 0; i < N_BRANCHES; i++) {        
        // [1..B], the number of already placed catoms + 1.
        // B means that branch is finished or should not be grown
        int multiplier = B - catomReqByBranch[i];
        
        if (catomReqByBranch[i] > 0 and openPositions[i])
            *openPositions[i] = Cell3DPosition(catom->position + multiplier * ruleMatcher->
                                               getBranchUnitOffset((BranchIndex)i));
        else if (catomReqByBranch[i] > 0 and not openPositions[i])
            openPositions[i] = new Cell3DPosition(catom->position + multiplier * ruleMatcher->
                                                  getBranchUnitOffset((BranchIndex)i));
        else openPositions[i] = NULL;
    }
}

bool MeshAssemblyBlockCode::
checkOrthogonalIncidentBranchCompletion(const Cell3DPosition& pos) {
    VS_ASSERT(ruleMatcher->isInMesh(pos));
    
    if (ruleMatcher->isOnXBranch(pos))
        return lattice->cellHasBlock(pos + Cell3DPosition(1,-1,0));
    else if (ruleMatcher->isOnYBranch(pos))
        return lattice->cellHasBlock(pos + Cell3DPosition(-1,1,0));
    else return true; // FIXME:
}
