#include "neighborhood.h"
#include "neighborRestriction.h"
#include "catoms3DWorld.h"

#define WAIT_TIME 5

using namespace Catoms3D;

Neighborhood::Neighborhood(Catoms3D::Catoms3DBlock *c, Reconf *r, Sync *s, SyncCCW *sccw, BlockCodeBuilder bcb)
{
    catom = c;
    reconf = r;
    sync = s;
    syncCCW = sccw;
    blockCodeBuilder = bcb;
    leftCompleted = rightCompleted = false;
}

void Neighborhood::setLeftCompleted()
{
    leftCompleted = true;
    if (leftCompleted && rightCompleted)
        reconf->setLineCompleted();
}

void Neighborhood::setRightCompleted()
{
    rightCompleted = true;
    if (leftCompleted && rightCompleted)
        reconf->setLineCompleted();
}

void Neighborhood::addNeighborToLeft()
{
    addNeighbor(catom->position.addX(-1));
}

void Neighborhood::addNeighborToRight()
{
    addNeighbor(catom->position.addX(1));
}

bool Neighborhood::isOnLeftBorder()
{
    if (!BlockCode::target->isInTarget(catom->position.addX(-1))) {
        setLeftCompleted();
        return true;
    }
    return false;
}

bool Neighborhood::isOnRightBorder()
{
    if (!BlockCode::target->isInTarget(catom->position.addX(1))) {
        setRightCompleted();
        return true;
    }
    return false;
}

void Neighborhood::addNextLineNeighbor()
{
    if  (reconf->isSeedNext()) {
        if (isLeftCompleted() && isRightCompleted()) {
            addNeighbor(catom->position.addY(1));
        }
    }
}

void Neighborhood::checkLineCompleted()
{
    if (isOnLeftBorder())  {
        if (rightCompleted)
            addNextLineNeighbor();
        sendMessageLeftSideCompleted(reconf->getNumberSeedsLeft(), reconf->isSeedNext());
    }
    if (isOnRightBorder()) {
        if (leftCompleted)
            addNextLineNeighbor();
        sendMessageRightSideCompleted(reconf->getNumberSeedsRight(), reconf->isSeedNext());
    }
}

bool Neighborhood::isFirstCatomOfLine()
{
    if (catom->getInterface(catom->position.addX(-1))->connectedInterface == NULL &&
            catom->getInterface(catom->position.addX(1))->connectedInterface == NULL)
        return true;
    return false;
}

void Neighborhood::init()
{
    if (isFirstCatomOfLine())
        reconf->setLineParent();
    reconf->isSeedNext();
    reconf->isSeedPrevious();
    checkLineCompleted();
}

void Neighborhood::addNeighbors()
{
    if (reconf->needSyncToLeft()) {
        syncCCW->sync();
    }
    else if (reconf->needSyncToRight()) {
        if (syncCCW->canContinueLeftSeed()) {
            reconf->setSeedNext();
            addNextLineNeighbor();
        }
    }
    else {
        addNeighborToLeft();
        addNeighborToRight();
    }
}

void Neighborhood::addNeighborsWithoutSync()
{
    addNeighborToLeft();
    addNeighborToRight();
}

void Neighborhood::handleParentSeedMsg(MessagePtr message)
{
    New_catom_response_message *msgResponse = new New_catom_response_message;
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msgResponse, message->destinationInterface));
}

void Neighborhood::handleNewCatomMsg(MessagePtr message)
{
    New_catom_ptr recv_message = static_pointer_cast<New_catom_message>(message);
    New_catom_response_message *msgResponse = new New_catom_response_message;
    if (recv_message->lineParentDirection == TO_LEFT) {
        msgResponse->leftCompleted = isLeftCompleted();
        msgResponse->numberSeedsLeft = reconf->getNumberSeedsLeft() + reconf->isSeedNext();
    }
    else {
        msgResponse->rightCompleted = isRightCompleted();
        msgResponse->numberSeedsRight = reconf->getNumberSeedsRight() + reconf->isSeedNext();
    }
    msgResponse->lineParentDirection = recv_message->lineParentDirection;
    msgResponse->requestQueue = reconf->requestQueue;

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msgResponse, message->destinationInterface));
}

void Neighborhood::handleNewCatomParentMsg(MessagePtr message)
{
    New_catom_parent_response_message *msgResponse = new New_catom_parent_response_message;
    while (!reconf->requestQueue.empty()) {
        shared_ptr<SyncCCW_message> requestMsg = static_pointer_cast<SyncCCW_message>(reconf->requestQueue.front());
        reconf->requestQueue.pop();
        if (requestMsg->goal[1] != catom->position[1]) {
            msgResponse->requestQueue.push(requestMsg);
        }
    }

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msgResponse, message->destinationInterface));
}

void Neighborhood::handleNewCatomParentResponseMsg(MessagePtr message)
{
    New_catom_parent_response_ptr recv_message = static_pointer_cast<New_catom_parent_response_message>(message);
    reconf->requestQueue = recv_message->requestQueue;
    requestQueueHandler();
    if (!reconf->requestQueue.empty()) {
        catom->setColor(BLACK);
    }
}

void Neighborhood::requestQueueHandler()
{
    for (int i = 0; i < (int)reconf->requestQueue.size(); i++) {
        shared_ptr<SyncCCW_message> requestMsg = static_pointer_cast<SyncCCW_message>(reconf->requestQueue.front());
        reconf->requestQueue.pop();
        if (requestMsg->goal == catom->position) {
            syncCCW->response();
        }
        else {
            reconf->requestQueue.push(requestMsg);
        }
    }
}

void Neighborhood::handleNewCatomResponseMsg(MessagePtr message)
{
    New_catom_response_ptr recv_message = static_pointer_cast<New_catom_response_message>(message);
    if (recv_message->lineParentDirection == TO_LEFT) {
        int numberSeedsLeft = max(recv_message->numberSeedsLeft, reconf->getNumberSeedsLeft());
        reconf->setNumberSeedsLeft(numberSeedsLeft);
        if (recv_message->leftCompleted)
            setLeftCompleted();
    }
    else {
        int numberSeedsRight = max(recv_message->numberSeedsRight, reconf->getNumberSeedsRight());
        reconf->setNumberSeedsRight(numberSeedsRight);
        if (recv_message->rightCompleted)
            setRightCompleted();
    }
    reconf->lineParentDirection = recv_message->lineParentDirection;
    reconf->requestQueue = recv_message->requestQueue;
    requestQueueHandler();
    if (!reconf->requestQueue.empty()) {
        catom->setColor(LIGHTGREEN);
    }
    init();
}

void Neighborhood::handleLeftSideCompletedMsg(MessagePtr message)
{
    Left_side_completed_ptr recv_message = static_pointer_cast<Left_side_completed_message>(message);

    setLeftCompleted();
    addNextLineNeighbor();
    reconf->setNumberSeedsLeft(recv_message->numberSeedsLeft);
    sendMessageLeftSideCompleted(reconf->getNumberSeedsLeft(), reconf->isSeedNext());
}

void Neighborhood::handleRightSideCompletedMsg(MessagePtr message)
{
    Right_side_completed_ptr recv_message = static_pointer_cast<Right_side_completed_message>(message);

    setRightCompleted();
    addNextLineNeighbor();
    reconf->setNumberSeedsRight(recv_message->numberSeedsRight);
    sendMessageRightSideCompleted(reconf->getNumberSeedsRight(), reconf->isSeedNext());

}

void Neighborhood::sendMessageToGetLineInfo()
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

void Neighborhood::sendMessageToGetParentInfo()
{
    Cell3DPosition neighborPosition = catom->position.addY(-1);
    New_catom_parent_message *msg = new New_catom_parent_message;
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(neighborPosition)));
}

void Neighborhood::sendMessageLeftSideCompleted(int numberSeedsLeft, bool isSeed)
{
    int newNumberSeedsLeft = numberSeedsLeft + isSeed;
    Cell3DPosition positionLeft = catom->position.addX(1);

    Left_side_completed_message *msg = new Left_side_completed_message(newNumberSeedsLeft);
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(positionLeft)));
}

void Neighborhood::sendMessageRightSideCompleted(int numberSeedsRight, bool isSeed)
{
    int newNumberSeedsRight = numberSeedsRight + isSeed;
    Cell3DPosition positionRight = catom->position.addX(-1);
    Right_side_completed_message *msg = new Right_side_completed_message(newNumberSeedsRight);
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(positionRight)));
}

void Neighborhood::addAllNeighbors()
{
    for (int i = 0; i < 12; i++) {
        Cell3DPosition neighborGridPos = catom->position;
        int *neighborPosPointer = (catom->position[2]%2) ? NeighborRestriction::neighborDirectionsOdd[i] : NeighborRestriction::neighborDirectionsEven[i];

        neighborGridPos.pt[0] += neighborPosPointer[0];
        neighborGridPos.pt[1] += neighborPosPointer[1];
        neighborGridPos.pt[2] += neighborPosPointer[2];
        if (neighborGridPos[2] != catom->position[2])
            continue;
        addNeighbor(neighborGridPos);
    }
}

void Neighborhood::addNeighbor(Cell3DPosition pos)
{
    Catoms3DWorld *world = Catoms3DWorld::getWorld();
    NeighborRestriction neighbors;
    if (world->lattice->isFree(pos) && BlockCode::target->isInTarget(pos)) {
        if (neighbors.isPositionBlockable(pos))
            world->addBlock(0, blockCodeBuilder, pos, PINK, 0, false);
        else if (neighbors.isPositionBlocked(pos))
            world->addBlock(0, blockCodeBuilder, pos, RED, 0, false);
        else {

            world->addBlock(0, blockCodeBuilder, pos, WHITE, 0, false);
        }
        world->linkBlock(pos);
    }
}

New_catom_response_message::New_catom_response_message()
{
    id = NEW_CATOM_RESPONSE_MSG_ID;
    leftCompleted = rightCompleted = false;
    numberSeedsLeft = numberSeedsRight = 0;
}
