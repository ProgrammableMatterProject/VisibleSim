#include "syncNext.h"

#define MSG_TIME 0

void SyncNext::sync() {
    shared_ptr<SyncNext_message> message(new SyncNext_message(1, catom->position.offsetX(-1).offsetY(-1), catom->position));
    handleMessage(message);
}

void SyncNext::response(Cell3DPosition origin) {
    shared_ptr<SyncNext_response_message> message(new SyncNext_response_message(1, origin));
    handleMessageResponse(message);
}

bool SyncNext::needSyncToLeft()
{
    if (catom->getInterface(catom->position.offsetX(-1))->isConnected())
        return false;

    if (catom->getInterface(catom->position.offsetX(-1))->connectedInterface == NULL &&
        !BlockCode::target->isInTarget(catom->position.offsetY(-1)) &&
        BlockCode::target->isInTarget(catom->position.offsetX(-1)) &&
        BlockCode::target->isInTarget(catom->position.offsetX(-1).offsetY(-1)) )
    {
        return isInternalBorder(1);
    }
    return false;
}

bool SyncNext::needSyncToRight()
{
    if (catom->getInterface(catom->position.offsetY(1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.offsetX(1)) &&
        BlockCode::target->isInTarget(catom->position.offsetX(1).offsetY(1)))
    {

        BoundingBox bb;
        static_cast<TargetCSG*>(BlockCode::target)->boundingBox(bb);
        for (int i = 2; static_cast<TargetCSG*>(BlockCode::target)->gridToCSGPosition(catom->position.offsetX(i))[0] < bb.P1[0]; i++) {
            cout << "a" << endl;
            if (!BlockCode::target->isInTarget(catom->position.offsetX(i)) &&
                BlockCode::target->isInTarget(catom->position.offsetX(i).offsetY(1)))
                continue;
            if (BlockCode::target->isInTarget(catom->position.offsetX(i)) &&
                BlockCode::target->isInTarget(catom->position.offsetX(i).offsetY(1)) )
                return isInternalBorder(1);
            return false;
        }
    }
    return false;
}

void SyncNext::handleMessage(shared_ptr<Message> message) {
    //catom->setColor(BLUE);
    shared_ptr<SyncNext_message> syncMsg = static_pointer_cast<SyncNext_message>(message);
    for (int i = 0; i < 4; i++) {
        int idx = (((syncMsg->idx+i-1)%4)+4)%4;
        Cell3DPosition pos = catom->position.offsetX(ccw_order[idx].first)
                                            .offsetY(ccw_order[idx].second);
        P2PNetworkInterface *p2p = catom->getInterface(pos);

        if (BlockCode::target->isInTarget(pos) && !p2p->isConnected()) {
            SyncNext_message *msg = new SyncNext_message(idx, syncMsg->goal, syncMsg->origin);
            MessageQueue mQueue(pos, msg);
            reconf->messageQueue.push_back(mQueue);
            break;
        }

        if (p2p->isConnected()) {
            SyncNext_message *msg = new SyncNext_message(idx, syncMsg->goal, syncMsg->origin);
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, p2p));
            nMessagesSync++;
            break;
        }
    }
}

void SyncNext::handleMessageResponse(shared_ptr<Message> message) {
    //catom->setColor(GREEN);
    shared_ptr<SyncNext_response_message> syncMsg = static_pointer_cast<SyncNext_response_message>(message);
    for (int i = 0; i < 4; i++) {
        int idx = (((syncMsg->idx+i-1)%4)+4)%4;
        Cell3DPosition pos = catom->position.offsetX(cw_order[idx].first)
                                            .offsetY(cw_order[idx].second);
        P2PNetworkInterface *p2p = catom->getInterface(pos);
        if (p2p->isConnected()) {
            SyncNext_response_message *msg = new SyncNext_response_message(idx, syncMsg->origin);
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, p2p));
            nMessagesSyncResponse++;
            break;
        }
    }
}
