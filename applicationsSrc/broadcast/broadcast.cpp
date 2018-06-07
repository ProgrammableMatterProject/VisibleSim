#include <iostream>
#include "broadcast.h"
#include "catoms3DWorld.h"

using namespace std;
using namespace Catoms3D;

Broadcast::Broadcast(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;
}

void Broadcast::startup() {
    broadcast = false;
    if (catom->blockId == 1) {
        sendMessageToAllNeighbors();
    }

    std::this_thread::sleep_for(std::chrono::milliseconds(CONSTRUCT_WAIT_TIME));
}

void Broadcast::processLocalEvent(EventPtr pev) {
	MessagePtr message;

    if (pev->eventType == EVENT_NI_RECEIVE) {
        message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
        if (message->id == BROADCAST_MESSAGE_ID) {
            if (brodcast == false) {
                broadcast = true;
                sendMessageToAllNeighbors();
            }
        }
    }
}

BlockCode* Broadcast::buildNewBlockCode(BuildingBlock *host) {
    return (new Broadcast((Catoms3DBlock*)host));
}
