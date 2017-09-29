#include "neighborMessages.h"

#define MSG_TIME rand()%10000
//#define MSG_TIME 0000

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
    if (syncNext->needSyncToRight() && !syncNext->isInternalBorder(1)) {
        reconf->setSeedNext();
    }

    if (syncPrevious->needSyncToRight() && !syncNext->isInternalBorder(2)) {
        reconf->setSeedPrevious();
    }

    neighborhood->checkSyncAndTryAddNeighbors();
    //trySendMessagePlaneFinished();
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
    New_catom_ptr recv_message = static_pointer_cast<New_catom_message>(message);
    New_catom_response_message *msgResponse = new New_catom_response_message;

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
    nMessagesGetInfo++;

    neighborhood->sendResponseMessageToAddLeft();
    neighborhood->sendResponseMessageToAddRight();

    sendMessagesOnQueue(recv_message->sourceInterface->hostBlock->position);
}

void NeighborMessages::handleNewCatomParentMsg(MessagePtr message)
{
    New_catom_parent_response_message *msgResponse = new New_catom_parent_response_message;
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
    nMessagesGetInfo++;

    neighborhood->sendResponseMessageToAddLeft();
    neighborhood->sendResponseMessageToAddRight();

    sendMessagesOnQueue(message->sourceInterface->hostBlock->position);
}

void NeighborMessages::handleNewCatomParentResponseMsg(MessagePtr message)
{
    New_catom_parent_response_ptr recv_message = static_pointer_cast<New_catom_parent_response_message>(message);
}

void NeighborMessages::handleNewCatomResponseMsg(MessagePtr message)
{
    New_catom_response_ptr recv_message = static_pointer_cast<New_catom_response_message>(message);
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

void NeighborMessages::trySendMessagePlaneFinished()
{
    if (reconf->checkPlaneCompleted()) {
       sendMessagePlaneFinished();
    }
}

void NeighborMessages::sendMessagePlaneFinished()
{
    if (reconf->syncPlaneNodeParent != NULL)
        reconf->syncPlaneNodeParent->setCompleted();
    if (reconf->planeFinished)
        return;
    reconf->planeFinished = true;
    Plane_finished_message *msg = new Plane_finished_message();
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addX(1))));
    msg = new Plane_finished_message();
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addX(-1))));
    msg = new Plane_finished_message();
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addY(1))));
    msg = new Plane_finished_message();
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addY(-1))));
}

void NeighborMessages::sendMessagePlaneFinishedAck()
{
    if (reconf->planeFinishedAck)
        return;
    reconf->planeFinishedAck = true;
    Plane_finished_ack_message *msg = new Plane_finished_ack_message();
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addX(1))));
    msg = new Plane_finished_ack_message();
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addX(-1))));
    msg = new Plane_finished_ack_message();
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addY(1))));
    msg = new Plane_finished_ack_message();
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addY(-1))));
}

void NeighborMessages::sendMessagesOnQueue(Cell3DPosition pos)
{
    vector<MessageQueue>::iterator it;
    for (it = reconf->messageQueue.begin(); it != reconf->messageQueue.end(); ) {
        if (pos == it->destination) {
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, it->message, catom->getInterface(pos)));
            it = reconf->messageQueue.erase(it);
        }
        else
            ++it;
    }
}
