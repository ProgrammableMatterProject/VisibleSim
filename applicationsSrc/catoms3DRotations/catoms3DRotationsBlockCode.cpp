/**
 * @file   catoms3DRotationsBlockCode.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Oct 10 14:10:37 2018
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
#include "rotation3DEvents.h"
#include "catoms3DBlock.h"

#include "catoms3DRotationsBlockCode.hpp"


using namespace Catoms3D;

Catoms3DRotationsBlockCode::Catoms3DRotationsBlockCode(Catoms3DBlock *host):
    Catoms3DBlockCode(host) {
    scheduler = getScheduler();
    world = BaseSimulator::getWorld();
    lattice = world->lattice;   
    catom = host;
}

Catoms3DRotationsBlockCode::~Catoms3DRotationsBlockCode() {
}

void Catoms3DRotationsBlockCode::onBlockSelected() {
    // Debug:
    // for (const Cell3DPosition& pos : lattice->getNeighborhood(catom->position)) {
    //     short conId = catom->getConnectorId(pos);
    //     cout << pos << " <---> " << conId << endl;
    // }
    // cout << " --- " << endl;
}

void Catoms3DRotationsBlockCode::startup() {
    stringstream info;
    info << "Starting ";

    if (catom->blockId == 1) {
        // Module #1 expects a neighbor on its connector 0
        Catoms3DBlock* pivot = catom->getNeighborBlock(0);

        VS_ASSERT_MSG(pivot, "no module on connector 0 of catom #1!");

        try {
            scheduler->schedule(
                new Rotation3DStartEvent(getScheduler()->now(), catom, pivot,
                                         4, RotationLinkType::Any));
        } catch (NoRotationPathForFaceException& e) {
            cerr << "Rotation failed: " << e.what() << endl;
        }
    }
}

void Catoms3DRotationsBlockCode::processReceivedMessage(MessagePtr msg,
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

void Catoms3DRotationsBlockCode::processLocalEvent(EventPtr pev) {
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

        case EVENT_ROTATION3D_END: {
            // Do something when rotation has ended...
            // e.g.:
            catom->setColor(YELLOW);
        } break;            
            
        case EVENT_TAP: {
        } break;

        case EVENT_INTERRUPTION: {            
            std::shared_ptr<InterruptionEvent> itev =
                std::static_pointer_cast<InterruptionEvent>(pev);

            console << "IT Triggered, mode: " << itev->mode << "\n";
        }
    }
}
