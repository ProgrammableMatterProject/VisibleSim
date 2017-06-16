#include "syncPrevious.h"

#define MSG_TIME 1000

void SyncPrevious::sync() {
    shared_ptr<SyncPrevious_message> message(new SyncPrevious_message(1, catom->position.addX(1).addY(1), catom->position));
    handleMessage(message);
}

void SyncPrevious::response(Cell3DPosition origin) {
    shared_ptr<SyncPrevious_response_message> message(new SyncPrevious_response_message(1, origin));
    handleMessageResponse(message);
}

void SyncPrevious::handleMessage(shared_ptr<Message> message) {
    shared_ptr<SyncPrevious_message> syncMsg = static_pointer_cast<SyncPrevious_message>(message);
    for (int i = 0; i < 4; i++) {
        int idx = (((syncMsg->idx+i-1)%4)+4)%4;
        Cell3DPosition pos = catom->position.addX(ccw_order[idx].first)
                                            .addY(ccw_order[idx].second);
        P2PNetworkInterface *p2p = catom->getInterface(pos);
        if (p2p->isConnected()) {
            SyncPrevious_message *msg = new SyncPrevious_message(idx, syncMsg->goal, syncMsg->origin);
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, p2p));
            break;
        }
    }
}

void SyncPrevious::handleMessageResponse(shared_ptr<Message> message) {
    shared_ptr<SyncPrevious_response_message> syncMsg = static_pointer_cast<SyncPrevious_response_message>(message);
    for (int i = 0; i < 4; i++) {
        int idx = (((syncMsg->idx+i-1)%4)+4)%4;
        Cell3DPosition pos = catom->position.addX(cw_order[idx].first)
                                            .addY(cw_order[idx].second);
        P2PNetworkInterface *p2p = catom->getInterface(pos);
        if (p2p->isConnected()) {
            SyncPrevious_response_message *msg = new SyncPrevious_response_message(idx, syncMsg->origin);
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, p2p));
            break;
        }
    }
}
