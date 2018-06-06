#include "neighborMessages.h"
#include "../reconfCatoms3DBlockCode.h"

#define MSG_TIME 0//rand()%10

int NeighborMessages::nMessagesGetInfo = 0;
int NeighborMessages::nMessagesBorderMessage = 0;

NeighborMessages::NeighborMessages(Catoms3D::Catoms3DBlock *c, Reconf *r, Neighborhood *n, SyncNext *sn, SyncPrevious *sp, SyncPlane *sPlane)
{
    catom = c;
    reconf = r;
    neighborhood = n;
    syncNext = sn;
    syncPrevious = sp;
    syncPlane = sPlane;
}

void NeighborMessages::init()
{
    reconf->init = true;
    if (syncNext->needSyncToRight() && !syncNext->isInternalBorder(1)) {
        reconf->setSeedNext();
    }

    if (syncPrevious->needSyncToRight() && !syncNext->isInternalBorder(2)) {
        reconf->setSeedPrevious();
    }

    neighborhood->addNeighbors();

    if (reconf->areNeighborsPlaced() && reconf->nChildren == 0) {
        sendMessagePlaneFinished();
    }
}

void NeighborMessages::checkLineParent() {
    if (neighborhood->isFirstCatomOfLine())
        reconf->setLineParent();
}

void NeighborMessages::handleParentSeedMsg(MessagePtr message)
{
    New_catom_response_message *msgResponse = new New_catom_response_message;
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
}

void NeighborMessages::handleNewCatomMsg(MessagePtr message)
{
    New_catom_response_message *msgResponse = new New_catom_response_message;

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
    nMessagesGetInfo++;

    sendMessagesOnQueue(message->sourceInterface->hostBlock->position);
}

void NeighborMessages::handleNewCatomParentMsg(MessagePtr message)
{
    New_catom_parent_response_message *msgResponse = new New_catom_parent_response_message;
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
    nMessagesGetInfo++;

    sendMessagesOnQueue(message->sourceInterface->hostBlock->position);
}

void NeighborMessages::handleNewCatomParentResponseMsg(MessagePtr message)
{
    New_catom_parent_response_ptr recv_message = static_pointer_cast<New_catom_parent_response_message>(message);
    reconf->interfaceParent = recv_message->sourceInterface;
}

void NeighborMessages::handleNewCatomResponseMsg(MessagePtr message)
{
    New_catom_response_ptr recv_message = static_pointer_cast<New_catom_response_message>(message);
    reconf->interfaceParent = recv_message->sourceInterface;
}

void NeighborMessages::sendMessageToGetLineInfo()
{
    for (int i = 0; i < 2; i++) {
        Cell3DPosition neighborPosition = (i == 0) ? catom->position.addX(-1) : catom->position.addX(1);
        New_catom_message *msg = new New_catom_message;
        if (i == 0)
            msg->lineParentDirection = TO_LEFT;
        else
            msg->lineParentDirection = TO_RIGHT;
        if (catom->getInterface(neighborPosition)->isConnected()) {
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
            nMessagesGetInfo++;
        }
    }
}

void NeighborMessages::sendMessageToGetParentInfo()
{
    New_catom_parent_message *msg = new New_catom_parent_message;
    Cell3DPosition neighborPosition;

    neighborPosition = catom->position.addY(1);
    if (catom->getInterface(neighborPosition)->isConnected()) {
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
        nMessagesGetInfo++;
    }

    neighborPosition = catom->position.addY(-1);
    if (catom->getInterface(neighborPosition)->isConnected()) {
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
        nMessagesGetInfo++;
    }
}

New_catom_response_message::New_catom_response_message()
{
    id = NEW_CATOM_RESPONSE_MSG_ID;
}

void NeighborMessages::sendMessagePlaneFinished() {
    Plane_finished_message *msg = new Plane_finished_message();
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, reconf->interfaceParent->connectedInterface));
}

void NeighborMessages::sendMessagePlaneFinishedAck()
{
    if (reconf->isPlaneCompleted)
        return;
    reconf->isPlaneCompleted = true;
    vector<pair<int,int>> coordinates = {{1,0},{-1,0},{0,-1},{0,1}};
    for (int i = 0; i < 4; i++) {
        int x = coordinates[i].first;
        int y = coordinates[i].second;
        Plane_finished_ack_message *msg = new Plane_finished_ack_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addX(x).addY(y))));
    }
}

void NeighborMessages::sendMessagesOnQueue(Cell3DPosition pos)
{
    vector<MessageQueue>::iterator it;
    for (it = reconf->messageQueue.begin(); it != reconf->messageQueue.end(); ) {
        if (pos == it->destination) {
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, it->message, catom->getInterface(pos)));
            it = reconf->messageQueue.erase(it);
            Sync::nMessagesSync++;
        }
        else
            ++it;
    }
}

void NeighborMessages::sendMessageCanStartNextPlane(Cell3DPosition origin)
{
    //catom->setColor(RED);
    Can_start_next_plane_message *msg = new Can_start_next_plane_message();
    msg->origin = origin;
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, reconf->interfaceParent->connectedInterface));
}

void NeighborMessages::sendMessageConfirmationCanStartNextPlane(Cell3DPosition pos)
{
    //catom->setColor(LIGHTBLUE);
    //if (pos == catom->position)
    //{
        //neighborhood->addNeighborToNextPlane();
    //}
    //else {
        //broadcastConfirmationCanStartNextPlane(pos);
    //}
}

void NeighborMessages::broadcastConfirmationCanStartNextPlane(Cell3DPosition destination)
{
    if (reconf->lastMessage == destination)
        return;
    reconf->lastMessage = destination;
    vector<pair<int,int>> coordinates = {{1,0},{-1,0},{0,-1},{0,1}};
    for (int i = 0; i < 4; i++) {
        int x = coordinates[i].first;
        int y = coordinates[i].second;
        Confirmation_can_start_next_plane_message *msg = new Confirmation_can_start_next_plane_message();
        msg->destination = destination;
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addX(x).addY(y))));
    }
    if (reconf->isSeedNext())
    {
        Confirmation_can_start_next_plane_message *msg = new Confirmation_can_start_next_plane_message();
        msg->destination = destination;
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addZ(1))));
    }
}

void Plane_finished_message::handle(BlockCode *blockCode) {
    ReconfCatoms3DBlockCode* b = static_cast<ReconfCatoms3DBlockCode*>(blockCode);
    b->reconf->childConfirm++;

    //b->catom->setColor(GREEN);

    if (b->reconf->childConfirm == b->reconf->nChildren) {
        if (b->reconf->isPlaneParent) {
            b->neighborMessages->sendMessagePlaneFinishedAck();
            if (b->syncPlane->isSeed()) {
                b->neighborhood->addNeighborToNextPlane();
            }
        }
        else {
            b->neighborMessages->sendMessagePlaneFinished();
        }
    }
    //std::this_thread::sleep_for(std::chrono::milliseconds(10));
}

void Plane_finished_ack_message::handle(BlockCode *blockCode) {
    ReconfCatoms3DBlockCode* block = static_cast<ReconfCatoms3DBlockCode*>(blockCode);
    block->neighborMessages->sendMessagePlaneFinishedAck();
    if (block->syncPlane->isSeed()) {
        block->neighborhood->addNeighborToNextPlane();
    }
    //block->catom->setColor(GREY);
    //std::this_thread::sleep_for(std::chrono::milliseconds(10));
}
