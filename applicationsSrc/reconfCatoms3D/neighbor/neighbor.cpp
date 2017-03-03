#include "neighbor.h"
#include "neighborRestriction.h"
#include "catoms3DWorld.h"
#include "../CSG/csgUtils.h"

using namespace Catoms3D;

Neighbor::Neighbor(Catoms3D::Catoms3DBlock *c, BlockCodeBuilder bcb) : catom(c), blockCodeBuilder(bcb)
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

void Neighbor::addNeighborToLeft(Reconf *reconf)
{
    if (reconf->needSync())
        catom->setColor(BLUE);
    addNeighbor(catom->position.addX(-1));
}

void Neighbor::addNeighborToRight(Reconf *reconf)
{
    if (reconf->needSync())
        catom->setColor(BLUE);
    addNeighbor(catom->position.addX(1));
}

void Neighbor::sendMessageToGetNeighborInformation()
{
    for (int i = 0; i < 2; i++) {
        Cell3DPosition neighborPosition = (i == 0) ? catom->position.addX(-1) : catom->position.addX(1);
        if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
            New_catom_message *msg = new New_catom_message;
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(neighborPosition)));
        }
    }
}

void Neighbor::sendMessageLeftSideCompleted(int numberSeedsLeft, bool isSeed)
{
    int newNumberSeedsLeft = numberSeedsLeft + isSeed;
    Cell3DPosition positionLeft = catom->position.addX(1);

    Message *msg = new Left_side_completed_message(newNumberSeedsLeft);
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(positionLeft)));
}

void Neighbor::sendMessageRightSideCompleted(int numberSeedsRight, bool isSeed)
{
    int newNumberSeedsRight = numberSeedsRight + isSeed;
    Cell3DPosition positionRight = catom->position.addX(-1);
    Message *msg = new Right_side_completed_message(newNumberSeedsRight);
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(positionRight)));
}

void Neighbor::tryAddNextLineNeighbor(Reconf *reconf)
{
    if  (reconf->isSeed()) {
        addNeighbor(catom->position.addY(1));
    }
}

void Neighbor::checkLineCompleted(Reconf *reconf)
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

void Neighbor::handleNewCatomMsg(MessagePtr msg, int numberSeedsLeft, int numberSeedsRight)
{
    New_catom_response_message *msgResponse = new New_catom_response_message;
    msgResponse->leftCompleted = isLeftCompleted();
    msgResponse->rightCompleted = isRightCompleted();
    msgResponse->numberSeedsLeft = numberSeedsLeft;
    msgResponse->numberSeedsRight = numberSeedsRight;

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msgResponse, msg->destinationInterface));
}

void Neighbor::handleNewCatomResponseMsg(MessagePtr message, Reconf *reconf)
{
    shared_ptr<New_catom_response_message> recv_message = static_pointer_cast<New_catom_response_message>(message);
    int numberSeedsLeft = max(recv_message->numberSeedsLeft, reconf->getNumberSeedsLeft());
    int numberSeedsRight = max(recv_message->numberSeedsRight, reconf->getNumberSeedsRight());

    if (isLeftCompleted() && isRightCompleted()) {
        tryAddNextLineNeighbor(reconf);
    }
    if (recv_message->leftCompleted)
        setLeftCompleted();
    if (recv_message->rightCompleted)
        setRightCompleted();
    checkLineCompleted(reconf);
    addNeighborToLeft(reconf);
    addNeighborToRight(reconf);
    reconf->setNumberSeedsLeft(numberSeedsLeft);
    reconf->setNumberSeedsRight(numberSeedsRight);
}

void Neighbor::handleLeftSideCompletedMsg(MessagePtr message, Reconf *reconf)
{
    shared_ptr<Left_side_completed_message> recv_message = static_pointer_cast<Left_side_completed_message>(message);

    setLeftCompleted();
    if (isRightCompleted()) 
        tryAddNextLineNeighbor(reconf);
    reconf->setNumberSeedsLeft(recv_message->numberSeedsLeft);
    sendMessageLeftSideCompleted(reconf->getNumberSeedsLeft(), reconf->iAmSeed());

    std::this_thread::sleep_for(std::chrono::milliseconds(100));
}

void Neighbor::handleRightSideCompletedMsg(MessagePtr message, Reconf *reconf)
{
    shared_ptr<Right_side_completed_message> recv_message = static_pointer_cast<Right_side_completed_message>(message);

    setRightCompleted();
    if (isLeftCompleted())
        tryAddNextLineNeighbor(reconf);
    reconf->setNumberSeedsRight(recv_message->numberSeedsRight);
    sendMessageRightSideCompleted(reconf->getNumberSeedsRight(), reconf->iAmSeed());

    std::this_thread::sleep_for(std::chrono::milliseconds(100));
}

