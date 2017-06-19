#include "syncNext.h"

#define MSG_TIME 1000

void SyncNext::sync() {
    shared_ptr<SyncNext_message> message(new SyncNext_message(1, catom->position.addX(-1).addY(-1), catom->position));
    handleMessage(message);
}

void SyncNext::response(Cell3DPosition origin) {
    shared_ptr<SyncNext_response_message> message(new SyncNext_response_message(1, origin));
    handleMessageResponse(message);
}

void SyncNext::handleMessage(shared_ptr<Message> message) {
    catom->setColor(ORANGE);
    shared_ptr<SyncNext_message> syncMsg = static_pointer_cast<SyncNext_message>(message);
    for (int i = 0; i < 4; i++) {
        int idx = (((syncMsg->idx+i-1)%4)+4)%4;
        Cell3DPosition pos = catom->position.addX(ccw_order[idx].first)
                                            .addY(ccw_order[idx].second);
        P2PNetworkInterface *p2p = catom->getInterface(pos);
        if (p2p->isConnected()) {
            SyncNext_message *msg = new SyncNext_message(idx, syncMsg->goal, syncMsg->origin);
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, p2p));
            break;
        }
    }
}

void SyncNext::handleMessageResponse(shared_ptr<Message> message) {
    catom->setColor(GREEN);
    shared_ptr<SyncNext_response_message> syncMsg = static_pointer_cast<SyncNext_response_message>(message);
    for (int i = 0; i < 4; i++) {
        int idx = (((syncMsg->idx+i-1)%4)+4)%4;
        Cell3DPosition pos = catom->position.addX(cw_order[idx].first)
                                            .addY(cw_order[idx].second);
        P2PNetworkInterface *p2p = catom->getInterface(pos);
        if (p2p->isConnected()) {
            SyncNext_response_message *msg = new SyncNext_response_message(idx, syncMsg->origin);
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, p2p));
            break;
        }
    }
}
