#include "neighborMessages.h"

#define MSG_TIME 0

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
    checkLineParent();
    checkAndSendLeftBorderMessage();
    checkAndSendRightBorderMessage();

    neighborhood->tryAddNeighbors();
    neighborhood->tryAddNextLineNeighbor();
    neighborhood->tryAddPreviousLineNeighbor();
    trySendMessagePlaneFinished();
}

void NeighborMessages::checkLineParent() {
    if (neighborhood->isFirstCatomOfLine())
        reconf->setLineParent();
}

void NeighborMessages::checkAndSendLeftBorderMessage()
{
    if (neighborhood->isOnLeftBorder())
        sendMessageLeftSideCompleted(reconf->getNumberSeedsLeft(), reconf->isSeedNext());
}

void NeighborMessages::checkAndSendRightBorderMessage()
{
    if (neighborhood->isOnRightBorder())
        sendMessageRightSideCompleted(reconf->getNumberSeedsRight(), reconf->isSeedNext());
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

    msgResponse->lineParentDirection = recv_message->lineParentDirection;

    msgResponse->leftCompleted = reconf->isLeftCompleted();
    msgResponse->numberSeedsLeft = reconf->getNumberSeedsLeft() + reconf->isSeedNext();

    msgResponse->rightCompleted = reconf->isRightCompleted();
    msgResponse->numberSeedsRight = reconf->getNumberSeedsRight() + reconf->isSeedNext();

    msgResponse->requestQueue = reconf->requestQueue;
    msgResponse->createdFromPrevious = reconf->createdFromPrevious;

    msgResponse->syncPlaneNodeParent = reconf->syncPlaneNodeParent;

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
}

void NeighborMessages::handleNewCatomParentMsg(MessagePtr message)
{
    New_catom_parent_response_message *msgResponse = new New_catom_parent_response_message;
    while (!reconf->requestQueue.empty()) {
        msgResponse->requestQueue.push(reconf->requestQueue.front());
        reconf->requestQueue.pop();
    }
    if (message->sourceInterface->hostBlock->position[1] > catom->position[1])
        msgResponse->createdFromPrevious = true;
    else
        msgResponse->createdFromPrevious = false;

    msgResponse->syncPlaneNodeParent = reconf->syncPlaneNodeParent;
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
}

void NeighborMessages::handleNewCatomParentResponseMsg(MessagePtr message)
{
    New_catom_parent_response_ptr recv_message = static_pointer_cast<New_catom_parent_response_message>(message);
    reconf->requestQueue = recv_message->requestQueue;
    reconf->createdFromPrevious = recv_message->createdFromPrevious;
    reconf->syncPlaneNodeParent = recv_message->syncPlaneNodeParent;
    requestQueueHandler();
}

void NeighborMessages::requestQueueHandler()
{
    for (int i = 0; i < (int)reconf->requestQueue.size(); i++) {
        shared_ptr<Sync_message> requestMsg = static_pointer_cast<Sync_message>(reconf->requestQueue.front());
        reconf->requestQueue.pop();
        if (requestMsg->goal == catom->position) {
            if (catom->getInterface(catom->position.addY(-1))->isConnected() ||
                catom->getInterface(catom->position.addX(-1))->isConnected())
                syncNext->response(requestMsg->origin);
            else
                syncPrevious->response(requestMsg->origin);
        }
        else
            reconf->requestQueue.push(requestMsg);
    }
}

void NeighborMessages::handleNewCatomResponseMsg(MessagePtr message)
{
    New_catom_response_ptr recv_message = static_pointer_cast<New_catom_response_message>(message);
    int numberSeedsLeft = max(recv_message->numberSeedsLeft, reconf->getNumberSeedsLeft());
    int numberSeedsRight = max(recv_message->numberSeedsRight, reconf->getNumberSeedsRight());

    reconf->setNumberSeedsLeft(numberSeedsLeft);
    reconf->setNumberSeedsRight(numberSeedsRight);

    if (recv_message->rightCompleted)
        reconf->setRightCompleted();
    if (recv_message->leftCompleted)
        reconf->setLeftCompleted();

    reconf->lineParentDirection = recv_message->lineParentDirection;
    reconf->createdFromPrevious = recv_message->createdFromPrevious;
    reconf->requestQueue = recv_message->requestQueue;
    reconf->syncPlaneNodeParent = recv_message->syncPlaneNodeParent;
    requestQueueHandler();
    //if (!reconf->requestQueue.empty()) {
        //catom->setColor(LIGHTGREEN);
    //}
}

void NeighborMessages::handleLeftSideCompletedMsg(MessagePtr message)
{
    Left_side_completed_ptr recv_message = static_pointer_cast<Left_side_completed_message>(message);

    reconf->setLeftCompleted();
    reconf->setNumberSeedsLeft(recv_message->numberSeedsLeft);

    neighborhood->tryAddNextLineNeighbor();
    neighborhood->tryAddPreviousLineNeighbor();

    sendMessageLeftSideCompleted(reconf->getNumberSeedsLeft(), reconf->isSeedNext());

    trySendMessagePlaneFinished();
}

void NeighborMessages::handleRightSideCompletedMsg(MessagePtr message)
{
    Right_side_completed_ptr recv_message = static_pointer_cast<Right_side_completed_message>(message);

    reconf->setRightCompleted();
    reconf->setNumberSeedsRight(recv_message->numberSeedsRight);

    neighborhood->tryAddNextLineNeighbor();
    neighborhood->tryAddPreviousLineNeighbor();

    sendMessageRightSideCompleted(reconf->getNumberSeedsRight(), reconf->isSeedNext());

    trySendMessagePlaneFinished();
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
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
    }
}

void NeighborMessages::sendMessageToGetParentInfo()
{
    New_catom_parent_message *msg = new New_catom_parent_message;
    Cell3DPosition neighborPosition;

    neighborPosition = catom->position.addY(1);
    if (catom->getInterface(neighborPosition)->isConnected()) {
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
    }

    neighborPosition = catom->position.addY(-1);
    if (catom->getInterface(neighborPosition)->isConnected()) {
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
    }
}

void NeighborMessages::sendMessageLeftSideCompleted(int numberSeedsLeft, bool isSeed)
{
    int newNumberSeedsLeft = numberSeedsLeft + isSeed;
    Cell3DPosition positionLeft = catom->position.addX(1);

    Left_side_completed_message *msg = new Left_side_completed_message(newNumberSeedsLeft);
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(positionLeft)));
}

void NeighborMessages::sendMessageRightSideCompleted(int numberSeedsRight, bool isSeed)
{
    int newNumberSeedsRight = numberSeedsRight + isSeed;
    Cell3DPosition positionRight = catom->position.addX(-1);
    Right_side_completed_message *msg = new Right_side_completed_message(newNumberSeedsRight);
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(positionRight)));
}

New_catom_response_message::New_catom_response_message()
{
    id = NEW_CATOM_RESPONSE_MSG_ID;
    leftCompleted = rightCompleted = false;
    numberSeedsLeft = numberSeedsRight = 0;
}

void NeighborMessages::trySendMessagePlaneFinished()
{
    if (reconf->isLineCompleted() && reconf->checkPlaneCompleted()) {
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
