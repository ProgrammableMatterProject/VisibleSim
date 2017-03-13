#include "neighbor.h"
#include "neighborRestriction.h"
#include "catoms3DWorld.h"
#include "../CSG/csgUtils.h"

#define WAIT_TIME 5

using namespace Catoms3D;

Neighbor::Neighbor(Catoms3D::Catoms3DBlock *c, Reconf *r, BlockCodeBuilder bcb) : catom(c), reconf(r), blockCodeBuilder(bcb)
{
    leftCompleted = rightCompleted = false;
}

void Neighbor::addAllNeighbors()
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

void Neighbor::addNeighbor(Cell3DPosition pos)
{
    Catoms3DWorld *world = Catoms3DWorld::getWorld();
    NeighborRestriction neighbors;
    if (world->lattice->isFree(pos) && CsgUtils::isInside(pos)) {
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

void Neighbor::setLeftCompleted()
{
    leftCompleted = true;
    if (leftCompleted && rightCompleted)
        reconf->setLineCompleted();
}

void Neighbor::setRightCompleted()
{
    rightCompleted = true;
    if (leftCompleted && rightCompleted)
        reconf->setLineCompleted();
}

void Neighbor::addNeighborToLeft()
{
    if (reconf->needSync())
        catom->setColor(BLUE);
    addNeighbor(catom->position.addX(-1));
}

void Neighbor::addNeighborToRight()
{
    if (reconf->needSync())
        catom->setColor(BLUE);
    addNeighbor(catom->position.addX(1));
}

void Neighbor::sendMessageToGetLineInfo()
{
    for (int i = 0; i < 2; i++) {
        Cell3DPosition neighborPosition = (i == 0) ? catom->position.addX(-1) : catom->position.addX(1);
        if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
            New_catom_message *msg = new New_catom_message;
            if (i == 0)  
                msg->lineParentDirection = TO_LEFT;
            else
                msg->lineParentDirection = TO_RIGHT; 
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(neighborPosition)));
        }
    }
}

void Neighbor::sendMessageLeftSideCompleted(int numberSeedsLeft, bool isSeed)
{
    int newNumberSeedsLeft = numberSeedsLeft + isSeed;
    Cell3DPosition positionLeft = catom->position.addX(1);

    Left_side_completed_message *msg = new Left_side_completed_message(newNumberSeedsLeft);
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(positionLeft)));
}

void Neighbor::sendMessageRightSideCompleted(int numberSeedsRight, bool isSeed)
{
    int newNumberSeedsRight = numberSeedsRight + isSeed;
    Cell3DPosition positionRight = catom->position.addX(-1);
    Right_side_completed_message *msg = new Right_side_completed_message(newNumberSeedsRight);
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(positionRight)));
}

void Neighbor::tryAddNextLineNeighbor()
{
    if  (reconf->isSeed()) {
        if (isLeftCompleted() && isRightCompleted()) {
            addNeighbor(catom->position.addY(1));
        }
    }
}

void Neighbor::checkLineCompleted()
{
    if (isOnLeftBorder())
        sendMessageLeftSideCompleted(reconf->getNumberSeedsLeft(), reconf->isSeed());
    if (isOnRightBorder())
        sendMessageRightSideCompleted(reconf->getNumberSeedsRight(), reconf->isSeed());
}

bool Neighbor::isOnLeftBorder()
{
    if (!CsgUtils::isInside(catom->position.addX(-1))) {
        setLeftCompleted();
        return true;
    }
    return false;
}

bool Neighbor::isOnRightBorder()
{
    if (!CsgUtils::isInside(catom->position.addX(1))) {
        setRightCompleted();
        return true;
    }
    return false;
}

bool Neighbor::isFirstCatomOfLine()
{
    if (catom->getInterface(catom->position.addX(-1))->connectedInterface == NULL &&
            catom->getInterface(catom->position.addX(1))->connectedInterface == NULL)
        return true;
    return false;
}

void Neighbor::init()
{
    if (isFirstCatomOfLine())
        reconf->setLineParent();
    reconf->isSeedCheck();
    checkLineCompleted();
    tryAddNextLineNeighbor();
}

void Neighbor::addNeighbors()
{
    addNeighborToLeft();
    addNeighborToRight();
}

void Neighbor::handleNewCatomMsg(MessagePtr message)
{
    New_catom_ptr recv_message = static_pointer_cast<New_catom_message>(message);
    New_catom_response_message *msgResponse = new New_catom_response_message;
    if (recv_message->lineParentDirection == TO_LEFT) {
        msgResponse->leftCompleted = isLeftCompleted();
        msgResponse->numberSeedsLeft = reconf->getNumberSeedsLeft() + reconf->isSeed();
    }
    else {
        msgResponse->rightCompleted = isRightCompleted();
        msgResponse->numberSeedsRight = reconf->getNumberSeedsRight() + reconf->isSeed();
    }
    msgResponse->lineParentDirection = recv_message->lineParentDirection;

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msgResponse, message->destinationInterface));
}

void Neighbor::handleNewCatomResponseMsg(MessagePtr message)
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

    init();

    std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
}


void Neighbor::handleLeftSideCompletedMsg(MessagePtr message)
{
    Left_side_completed_ptr recv_message = static_pointer_cast<Left_side_completed_message>(message);

    setLeftCompleted();
    tryAddNextLineNeighbor();
    reconf->setNumberSeedsLeft(recv_message->numberSeedsLeft);
    sendMessageLeftSideCompleted(reconf->getNumberSeedsLeft(), reconf->isSeed());
    
    std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
}

void Neighbor::handleRightSideCompletedMsg(MessagePtr message)
{
    Right_side_completed_ptr recv_message = static_pointer_cast<Right_side_completed_message>(message);

    setRightCompleted();
    tryAddNextLineNeighbor();
    reconf->setNumberSeedsRight(recv_message->numberSeedsRight);
    sendMessageRightSideCompleted(reconf->getNumberSeedsRight(), reconf->isSeed());

    std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
}

New_catom_response_message::New_catom_response_message()
{
    id = NEW_CATOM_RESPONSE_MSG_ID;
    leftCompleted = rightCompleted = false;
    numberSeedsLeft = numberSeedsRight = 0;
}

