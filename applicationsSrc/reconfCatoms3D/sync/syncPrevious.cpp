#include "syncPrevious.h"

#define MSG_TIME 0

void SyncPrevious::sync() {
    shared_ptr<SyncPrevious_message> message(new SyncPrevious_message(3, catom->position.offsetX(1).offsetY(1), catom->position));
    handleMessage(message);
}

void SyncPrevious::response(Cell3DPosition origin) {
    shared_ptr<SyncPrevious_response_message> message(new SyncPrevious_response_message(3, origin));
    handleMessageResponse(message);
}

bool SyncPrevious::needSyncToLeft() {
    if (catom->getInterface(catom->position.offsetY(-1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.offsetX(-1)) &&
            BlockCode::target->isInTarget(catom->position.offsetX(-1).offsetY(-1))) {
        BoundingBox bb;
        static_cast<TargetCSG*>(BlockCode::target)->boundingBox(bb);

        for (int i = 2; static_cast<TargetCSG*>(BlockCode::target)->gridToCSGPosition(catom->position.offsetX(-i))[0] < bb.P1[0]; i++) {
            if (!BlockCode::target->isInTarget(catom->position.offsetX(-i)) &&
                BlockCode::target->isInTarget(catom->position.offsetX(-i).offsetY(-1)))
                continue;
            if (BlockCode::target->isInTarget(catom->position.offsetX(-i)) &&
                BlockCode::target->isInTarget(catom->position.offsetX(-i).offsetY(-1)) )
                return isInternalBorder(3);
            return false;
        }
    }
    return false;
}

bool SyncPrevious::needSyncToRight() {
    if (catom->getInterface(catom->position.offsetX(1))->isConnected())
        return false;

    if (catom->getInterface(catom->position.offsetX(1))->connectedInterface == NULL &&
            !BlockCode::target->isInTarget(catom->position.offsetY(1)) &&
            BlockCode::target->isInTarget(catom->position.offsetX(1)) &&
            BlockCode::target->isInTarget(catom->position.offsetX(1).offsetY(1)) )
        return isInternalBorder(3);
    return false;
}
void SyncPrevious::handleMessage(shared_ptr<Message> message) {
    //catom->setColor(BLUE);
    shared_ptr<SyncPrevious_message> syncMsg = static_pointer_cast<SyncPrevious_message>(message);
    for (int i = 0; i < 4; i++) {
        int idx = (((syncMsg->idx+i-1)%4)+4)%4;
        Cell3DPosition pos = catom->position.offsetX(ccw_order[idx].first)
                                            .offsetY(ccw_order[idx].second);
        P2PNetworkInterface *p2p = catom->getInterface(pos);
        if (BlockCode::target->isInTarget(pos) && !p2p->isConnected()) {
            SyncPrevious_message *msg = new SyncPrevious_message(idx, syncMsg->goal, syncMsg->origin);
            MessageQueue mQueue(pos, msg);
            reconf->messageQueue.push_back(mQueue);
            break;
        }
        if (p2p->isConnected()) {
            SyncPrevious_message *msg = new SyncPrevious_message(idx, syncMsg->goal, syncMsg->origin);
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, p2p));
            nMessagesSync++;
            break;
        }
    }
}

void SyncPrevious::handleMessageResponse(shared_ptr<Message> message) {
    //catom->setColor(GREEN);
    shared_ptr<SyncPrevious_response_message> syncMsg = static_pointer_cast<SyncPrevious_response_message>(message);
    for (int i = 0; i < 4; i++) {
        int idx = (((syncMsg->idx+i-1)%4)+4)%4;
        Cell3DPosition pos = catom->position.offsetX(cw_order[idx].first)
                                            .offsetY(cw_order[idx].second);
        P2PNetworkInterface *p2p = catom->getInterface(pos);
        if (p2p->isConnected()) {
            SyncPrevious_response_message *msg = new SyncPrevious_response_message(idx, syncMsg->origin);
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, p2p));
            nMessagesSyncResponse++;
            break;
        }
    }
}
