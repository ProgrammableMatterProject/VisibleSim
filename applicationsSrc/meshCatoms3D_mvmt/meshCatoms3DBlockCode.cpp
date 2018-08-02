/**
 * @file   meshCatoms3DBlockCode_mvmt.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 13:47:56 2018
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

#include "meshCatoms3DBlockCode.hpp"

// C3D Motion Engine
#include "messages.hpp"
#include "catoms3DMotionEngine.hpp"

using namespace Catoms3D;
using namespace MeshSpanningTree;

uint MeshCatoms3DBlockCode::X_MAX;
uint MeshCatoms3DBlockCode::Y_MAX;

MeshCatoms3DBlockCode::MeshCatoms3DBlockCode(Catoms3DBlock *host):
    Catoms3DBlockCode(host) {
    scheduler = getScheduler();
    world = BaseSimulator::getWorld();
    lattice = world->lattice;   
    catom = host;
    engine = new Catoms3DMotionEngine(*this, *host);
    
    const Cell3DPosition& ub = lattice->getGridUpperBounds();
    X_MAX = ub[0];
    Y_MAX = ub[1];

    ruleMatcher = new MeshSpanningTreeRuleMatcher(X_MAX, Y_MAX, B);
}

MeshCatoms3DBlockCode::~MeshCatoms3DBlockCode() {
    delete engine;
}


bool MeshCatoms3DBlockCode::moduleInSpanningTree(const Cell3DPosition& pos) {
    return target->isInTarget(pos) and lattice->isInGrid(pos);
}

bool MeshCatoms3DBlockCode::isMeshRoot(const Cell3DPosition& pos) {
    return pos.pt[0] % B == 0 and pos.pt[1] % B == 0 and pos.pt[2] % B == 0;
}

static std::set<Cell3DPosition> placedBorderCatoms;
void MeshCatoms3DBlockCode::startup() {
    stringstream info;
    info << "Starting ";

    static const bool ASSEMBLE_SCAFFOLD = true;
    if (ASSEMBLE_SCAFFOLD) {
        for (auto const& nPos : world->lattice->getNeighborhood(catom->position)) {            
            if (ruleMatcher->shouldSendToNeighbor(catom->position, nPos)
                and ruleMatcher->isInMesh(nPos)) {
                static bID id = 1;
                world->addBlock(++id, buildNewBlockCode, nPos, ORANGE);
                awaitKeyPressed();
            }
        }    
    }
    
    static const bool SIMULATE_SPANNINGTREE = true;
    if (SIMULATE_SPANNINGTREE and catom->blockId == 1) {
        catom->setColor(RED);

        for (const Cell3DPosition& pos : lattice->getActiveNeighborCells(catom->position)) {
            if (moduleInSpanningTree(pos)) {
                sendMessage(new DisassemblyTriggerMessage(*ruleMatcher, false),
                            catom->getInterface(pos), MSG_DELAY_MC, 0);
                expectedConfirms++;
            }
        }        
    }

    
}    

void MeshCatoms3DBlockCode::processReceivedMessage(MessagePtr msg,
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

void MeshCatoms3DBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;
	
    switch (pev->eventType) {
        case EVENT_RECEIVE_MESSAGE: {
            message =
                (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;

            if (message->isMessageHandleable()) {
                (std::static_pointer_cast<Catoms3DMotionEngineMessage>(message))->
                    handle(this);
            } else {
                P2PNetworkInterface * recv_interface = message->destinationInterface;
            
                // Handover to global message handler
                processReceivedMessage(message, recv_interface);
            }
        } break;

            
            
        case EVENT_TAP: {
            // ?
        } break;
    }
}
