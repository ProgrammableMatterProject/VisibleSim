#include "neighborMessages.h"

NeighborMessages::NeighborMessages(Catoms3D::Catoms3DBlock *c, Reconf *r, Neighborhood *n, Sync *s, SyncCCW *sccw)
{
    catom = c;
    reconf = r;
    neighborhood = n;
    sync = s;
    syncCCW = sccw;
}

void NeighborMessages::init()
{
    checkLineParent();
    checkAndSendLeftBorderMessage();
    checkAndSendRightBorderMessage();

    neighborhood->tryAddNeighbors();
    neighborhood->tryAddNextLineNeighbor();
    neighborhood->tryAddPreviousLineNeighbor();
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
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msgResponse, message->destinationInterface));
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

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msgResponse, message->destinationInterface));
}

void NeighborMessages::handleNewCatomParentMsg(MessagePtr message)
{
    New_catom_parent_response_message *msgResponse = new New_catom_parent_response_message;
    while (!reconf->requestQueue.empty()) {
        shared_ptr<SyncCCW_message> requestMsg = static_pointer_cast<SyncCCW_message>(reconf->requestQueue.front());
        reconf->requestQueue.pop();
    }

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msgResponse, message->destinationInterface));
}

void NeighborMessages::handleNewCatomParentResponseMsg(MessagePtr message)
{
    New_catom_parent_response_ptr recv_message = static_pointer_cast<New_catom_parent_response_message>(message);
    reconf->requestQueue = recv_message->requestQueue;
    requestQueueHandler();
}

void NeighborMessages::requestQueueHandler()
{
    shared_ptr<SyncCCW_message> requestMsg = static_pointer_cast<SyncCCW_message>(reconf->requestQueue.front());
    for (int i = 0; i < (int)reconf->requestQueue.size(); i++) {
        reconf->requestQueue.pop();
        if (requestMsg->goal == catom->position)
            syncCCW->response();
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
    reconf->requestQueue = recv_message->requestQueue;
    requestQueueHandler();
    if (!reconf->requestQueue.empty()) {
        catom->setColor(LIGHTGREEN);
    }
}

void NeighborMessages::handleLeftSideCompletedMsg(MessagePtr message)
{
    Left_side_completed_ptr recv_message = static_pointer_cast<Left_side_completed_message>(message);

    reconf->setLeftCompleted();
    reconf->setNumberSeedsLeft(recv_message->numberSeedsLeft);

    neighborhood->tryAddNextLineNeighbor();
    neighborhood->tryAddPreviousLineNeighbor();

    sendMessageLeftSideCompleted(reconf->getNumberSeedsLeft(), reconf->isSeedNext());
}

void NeighborMessages::handleRightSideCompletedMsg(MessagePtr message)
{
    Right_side_completed_ptr recv_message = static_pointer_cast<Right_side_completed_message>(message);

    reconf->setRightCompleted();
    reconf->setNumberSeedsRight(recv_message->numberSeedsRight);

    neighborhood->tryAddNextLineNeighbor();
    neighborhood->tryAddPreviousLineNeighbor();

    sendMessageRightSideCompleted(reconf->getNumberSeedsRight(), reconf->isSeedNext());
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
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(neighborPosition)));
    }
}

void NeighborMessages::sendMessageToGetParentInfo()
{
    New_catom_parent_message *msg = new New_catom_parent_message;
    Cell3DPosition neighborPosition;

    neighborPosition = catom->position.addY(1);
    if (catom->getInterface(neighborPosition)->isConnected())
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(neighborPosition)));

    neighborPosition = catom->position.addY(-1);
    if (catom->getInterface(neighborPosition)->isConnected())
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(neighborPosition)));
}

void NeighborMessages::sendMessageLeftSideCompleted(int numberSeedsLeft, bool isSeed)
{
    int newNumberSeedsLeft = numberSeedsLeft + isSeed;
    Cell3DPosition positionLeft = catom->position.addX(1);

    Left_side_completed_message *msg = new Left_side_completed_message(newNumberSeedsLeft);
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(positionLeft)));
}

void NeighborMessages::sendMessageRightSideCompleted(int numberSeedsRight, bool isSeed)
{
    int newNumberSeedsRight = numberSeedsRight + isSeed;
    Cell3DPosition positionRight = catom->position.addX(-1);
    Right_side_completed_message *msg = new Right_side_completed_message(newNumberSeedsRight);
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(positionRight)));
}

New_catom_response_message::New_catom_response_message()
{
    id = NEW_CATOM_RESPONSE_MSG_ID;
    leftCompleted = rightCompleted = false;
    numberSeedsLeft = numberSeedsRight = 0;

}
