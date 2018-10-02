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
using namespace MeshSpanningTree;

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

    ruleMatcher = new MeshSpanningTreeRuleMatcher(X_MAX, Y_MAX, Z_MAX, B);
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

    cout << "Last round: [ ";
    for (int i = 0; i < 6; i++)
        cout << fedCatomOnLastRound[i] << ", ";
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
        // Schedule next growth iteration (at t + MOVEMENT_DURATION (?) )
        getScheduler()->schedule(
            new InterruptionEvent(getScheduler()->now() + 200, catom,
                                  IT_MODE_TILEROOT_ACTIVATION));
        console << "Scheduled IT" << "\n";
    } else {
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

        case EVENT_ROTATION3D_END: {
            // engine->handleRotationEnd();
        } break;            
            
        case EVENT_TAP: {
        } break;

        case EVENT_INTERRUPTION: {
            std::shared_ptr<InterruptionEvent> itev =
                std::static_pointer_cast<InterruptionEvent>(pev);

            console << "IT Triggered, mode: " << itev->mode << "\n";
            
            switch(itev->mode) {
            //     case IT_MODE_TILE_INSERTION:
                    // if (checkOrthogonalIncidentBranchCompletion(catom->position)) {
                    //     world->addBlock(++id, buildNewBlockCode,
                    //                     posTileAwaitingPlacement, GREEN);
                    //     numberExpectedAcksFromSubTree++;
                    // } else {
                    //     getScheduler()->schedule(
                    //         new InterruptionEvent(getScheduler()->now() + 200, catom,
                    //                               IT_MODE_TILE_INSERTION));
                    // }
                    
                    // break;
                case IT_MODE_TILEROOT_ACTIVATION: {
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
                        } else {
                            fedCatomOnLastRound[XBranch] = false;
                        }
                    } else {
                        fedCatomOnLastRound[XBranch] = false;
                        fedCatomOnLastRound[YBranch] = false;
                        int numInsertedCatoms = 0;
                    }
                    
                } break;
            }
        }
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
