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

#include "meshCatoms3DBlockCode_mvmt.hpp"

// C3D Motion Engine
#include "messages.hpp"
#include "catoms3DMotionEngine.hpp"

using namespace Catoms3D;

MeshCatoms3DBlockCode::MeshCatoms3DBlockCode(Catoms3DBlock *host):
    Catoms3DBlockCode(host) {
    scheduler = getScheduler();
    world = BaseSimulator::getWorld();
    lattice = world->lattice;
    catom = host;
    engine = new Catoms3DMotionEngine(*this, *host);
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

    if (target == NULL) {
        target = (Target::loadNextTarget());
    }

    static const double BORDER_WIDTH = 1.0;
    if ((!target->isInTarget(catom->position)
        // or static_cast<TargetCSG*>(target)->isInTargetBorder(catom->position, BORDER_WIDTH)
            )
        // ensure newly placed border catom is not removed on init
        and placedBorderCatoms.find(catom->position) == placedBorderCatoms.end()) {
        catom->setColor(WHITE);
        catom->setVisible(false);
        world->deleteBlock(catom);
    } else if (!static_cast<TargetCSG*>(target)->
               isInTargetBorder(catom->position, BORDER_WIDTH)) {
    }

    static const bool ENABLE_COATING = false;
    static const int SANDBOX_DEPTH = B;
    if  (ENABLE_COATING) {
        for (auto const& cell : world->lattice->getNeighborhood(catom->position)) {
            
            if (static_cast<TargetCSG*>(target)->isInTargetBorder(cell, BORDER_WIDTH)
                and cell.pt[2] > SANDBOX_DEPTH
                and world->lattice->isFree(cell)) {
                static bID id = 1;
                world->addBlock(++id, buildNewBlockCode, cell, ORANGE);
                placedBorderCatoms.insert(cell);
            }
        }
    }

    static const bool COLOR_ROOTS = true;
    if (COLOR_ROOTS and target->isInTarget(catom->position)) {
        if (isMeshRoot(catom->position))
            catom->setColor(GREEN);
        else
            catom->setColor(target->getTargetColor(catom->position));
    }

    static const bool SIMULATE_SPANNINGTREE = true;
    if (SIMULATE_SPANNINGTREE and catom->blockId == 1) {
        catom->setColor(RED);

        for (const Cell3DPosition& pos : lattice->getActiveNeighborCells(catom->position)) {
            if (moduleInSpanningTree(pos)) {
                sendMessage("Spanning Tree A",
                            new SpanningTreeAMessage(),
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
