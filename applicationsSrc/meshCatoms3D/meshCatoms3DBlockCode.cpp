/*
 * meshCatoms3DBlockCode.cpp
 *
 *  Created on: 26/06/18
 *      Author: pthalamy
 */

#include <iostream>
#include <set>

#include "catoms3DWorld.h"
#include "scheduler.h"
#include "events.h"
#include "trace.h"
#include "tDefs.h"

#include "meshCatoms3DBlockCode.hpp"

using namespace Catoms3D;

MeshCatoms3DBlockCode::MeshCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    scheduler = getScheduler();
    world = BaseSimulator::getWorld();
    catom = (Catoms3DBlock*)hostBlock;
}

MeshCatoms3DBlockCode::~MeshCatoms3DBlockCode() {}

static unsigned short meshScale = 5;
static bID id = 1;
bool MeshCatoms3DBlockCode::isInMesh(const Cell3DPosition &pos) {
    return isOnXBranch(pos)
        or isOnYBranch(pos)
        or isOnZBranch(pos)
        or isOnRevZBranch(pos)
        or isOnPlus45DegZBranch(pos)
        or isOnMinus45DegZBranch(pos);
}

static std::set<Cell3DPosition> placedBorderCatoms;
void MeshCatoms3DBlockCode::startup() {
    stringstream info;

    Cell3DPosition check = Cell3DPosition(2,-1,4);
    static bool done = false;
    if (not done) {
        cout << "isInTarget:" << target->isInTarget(check) << endl;
        cout << "isInTargetBorder:" << static_cast<TargetCSG*>(target)->isInTargetBorder(check, 1.0) << endl;
                
        done = true;
    }
    
    info << "Starting ";
    if (target == NULL) {
        target = (Target::loadNextTarget());
    }

    // static std::atomic_bool initialized;
    // if (not initialized) {
    //     for (int x = 0 - abs(lattice->gridSize[0] / 2) + 1;
    //          x < lattice->gridSize[0] - abs(lattice->gridSize[0] / 2); x++) {
    //         for (int y = 0 - abs(lattice->gridSize[1] / 2) + 1;
    //              y < lattice->gridSize[1] - abs(lattice->gridSize[1] / 2); y++) {
    //             for (int z = 0; z < lattice->gridSize[2] - 1; z++) {
    //                 Cell3DPosition test = Cell3DPosition(x, y, z);       
                    
    //                 if (target->isInTarget(test) and isInMesh(test)
    //                     and world->lattice->isFree(test)) {
    //                     world->addBlock(++id, buildNewBlockCode, test,
    //                                     target->getTargetColor(test));
    //                 }                
    //             }
    //         }
    //     }
    // }

    static const double BORDER_WIDTH = 1.0;
    if ((!target->isInTarget(catom->position)
        or static_cast<TargetCSG*>(target)->isInTargetBorder(catom->position, BORDER_WIDTH))
        // ensure newly placed border catom is not removed on init
        and placedBorderCatoms.find(catom->position) == placedBorderCatoms.end()) {
        catom->setColor(WHITE);
        catom->setVisible(false);
        world->deleteBlock(catom);
    } else if (!static_cast<TargetCSG*>(target)->isInTargetBorder(catom->position, BORDER_WIDTH)) {
        catom->setColor(target->getTargetColor(catom->position));
    }
   
    static const bool ENABLE_COATING = true;
    if  (ENABLE_COATING) {
        for (auto const& cell : world->lattice->getNeighborhood(catom->position)) {
            
            if (static_cast<TargetCSG*>(target)->isInTargetBorder(cell, BORDER_WIDTH)
                and world->lattice->isFree(cell)) {                
                world->addBlock(++id, buildNewBlockCode, cell, ORANGE);
                placedBorderCatoms.insert(cell);
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
                (std::static_pointer_cast<HandleableMessage>(message))->handle(this);
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

bool MeshCatoms3DBlockCode::isOnXBranch(const Cell3DPosition &pos) {
    return (pos.pt[1] % meshScale == ((pos.pt[2] / meshScale) * (meshScale / 2) + (not IS_EVEN(meshScale) and IS_EVEN(pos.pt[2]) and pos.pt[2] != 0 ? 1 : 0)) % meshScale and pos.pt[2] % meshScale == 0);
}

bool MeshCatoms3DBlockCode::isOnYBranch(const Cell3DPosition &pos) {
    return (pos.pt[0] % meshScale == ((pos.pt[2] / meshScale) * (meshScale / 2) + (not IS_EVEN(meshScale) and IS_EVEN(pos.pt[2]) and pos.pt[2] != 0 ? 1 : 0)) % meshScale and pos.pt[2] % meshScale == 0);
}
    
bool MeshCatoms3DBlockCode::isOnZBranch(const Cell3DPosition &pos) {
    return ( pos.pt[0] % meshScale == pos.pt[1] % meshScale and
             (pos.pt[2] % meshScale == pos.pt[0] % meshScale + pos.pt[1] % meshScale
              or pos.pt[2] % meshScale == pos.pt[0] % meshScale + pos.pt[1] % meshScale + 1));
}
    
bool MeshCatoms3DBlockCode::isOnRevZBranch(const Cell3DPosition &pos) {
    return (pos.pt[0] % meshScale == pos.pt[1] % meshScale
            and pos.pt[2] % meshScale == (meshScale - pos.pt[0] % meshScale));
}
    
bool MeshCatoms3DBlockCode::isOnMinus45DegZBranch(const Cell3DPosition &pos) {
    return (pos.pt[0] % meshScale + pos.pt[2] % meshScale == meshScale
            and pos.pt[1] % meshScale == 0);
}
    
bool MeshCatoms3DBlockCode::isOnPlus45DegZBranch(const Cell3DPosition &pos) {
    return (pos.pt[1] % meshScale + pos.pt[2] % meshScale == meshScale
            and pos.pt[0] % meshScale == 0);
}
