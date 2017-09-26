#include "neighborhood.h"
#include "neighborRestriction.h"
#include "catoms3DWorld.h"

#define MSG_TIME rand()%10000

int Neighborhood::numberBlockedModules = 0;

Neighborhood::Neighborhood(Catoms3D::Catoms3DBlock *c, Reconf *r, SyncNext *sn, SyncPrevious *sp, BlockCodeBuilder bcb)
{
    catom = c;
    reconf = r;
    syncNext = sn;
    syncPrevious = sp;
    blockCodeBuilder = bcb;
}

void Neighborhood::addNeighborToLeft()
{
    addNeighbor(catom->position.addX(-1));
}

void Neighborhood::addNeighborToRight()
{
    addNeighbor(catom->position.addX(1));
}

void Neighborhood::addNeighborToNextPlane()
{
    //if (BlockCode::target->isInTarget(catom->position.addZ(1)))
        //addNeighbor(catom->position.addZ(1));
}

void Neighborhood::addNeighborToPreviousPlane()
{
    //if (BlockCode::target->isInTarget(catom->position.addZ(-1)))
        //addNeighbor(catom->position.addZ(-1));
}

bool Neighborhood::isOnLeftBorder()
{
    if (!BlockCode::target->isInTarget(catom->position.addX(-1))) {
        reconf->setLeftCompleted();
        return true;
    }
    return false;
}

bool Neighborhood::isOnRightBorder()
{
    if (!BlockCode::target->isInTarget(catom->position.addX(1))) {
        reconf->setRightCompleted();
        return true;
    }
    return false;
}

bool Neighborhood::isFirstCatomOfLine()
{
    if (catom->getInterface(catom->position.addX(-1))->connectedInterface == NULL &&
            catom->getInterface(catom->position.addX(1))->connectedInterface == NULL)
        return true;
    return false;
}

bool Neighborhood::isFirstCatomOfPlane()
{
    if (isFirstCatomOfLine() &&
            catom->getInterface(catom->position.addY(-1))->connectedInterface == NULL &&
            catom->getInterface(catom->position.addY(1))->connectedInterface == NULL)
        return true;
    return false;
}

void Neighborhood::tryAddNextLineNeighbor()
{
    if (!reconf->isSeedNext() && reconf->needSyncToRightNext()) {
        if (!syncNext->isInternalBorder(0)) {
            reconf->setSeedNext();
        }
    }

    if (reconf->isSeedNext()) {
        //if (reconf->isLineCompleted() || !reconf->createdFromPrevious) {
            addNeighbor(catom->position.addY(1));
        //}
    }
}

void Neighborhood::tryAddPreviousLineNeighbor()
{
    if (!reconf->isSeedPrevious() && reconf->needSyncToRightPrevious()) {
        if (!syncNext->isInternalBorder(2)) {
            reconf->setSeedPrevious();
        }
    }

    if  (reconf->isSeedPrevious()) {
        if (reconf->isLineCompleted()) { //|| reconf->createdFromPrevious) {
            addNeighbor(catom->position.addY(-1));
        }
    }
}

void Neighborhood::tryAddNeighbors()
{
    if (reconf->needSyncToLeftNext() &&
        syncNext->isInternalBorder(1)) {
        syncNext->sync();
        catom->setColor(RED);
    }
    else if (reconf->needSyncToLeftPrevious()) {
        if (syncPrevious->isInternalBorder(3)) {
            syncPrevious->sync();
        }
    }
    else if (reconf->needSyncToRightNext()) {
        syncNext->response(catom->position.addX(1).addY(1));
        catom->setColor(RED);
    }
    else if (reconf->needSyncToRightPrevious()) {
        //addNeighborToRight();
    }
    else {
        canFill();
    }
}

void Neighborhood::tryAddNeighborToLeft()
{
    if (reconf->needSyncToLeftNext() &&
        syncNext->isInternalBorder(1)) {
        syncNext->sync();
        catom->setColor(RED);
    }
    else {
        addNeighborToLeft();
    }
}
void Neighborhood::tryAddNeighborToRight()
{
    if (!reconf->needSyncToRightNext())
        addNeighborToRight();
}

void Neighborhood::addNeighborsWithoutSync()
{
    addNeighborToLeft();
    addNeighborToRight();
}

void Neighborhood::addAllNeighbors()
{
    for (int i = 0; i < 12; i++) {
        Cell3DPosition neighborGridPos = catom->position;
        int *neighborPosPointer = (catom->position[2]%2) ? NeighborRestriction::neighborDirectionsOdd[i] : NeighborRestriction::neighborDirectionsEven[i];

        neighborGridPos.pt[0] += neighborPosPointer[0];
        neighborGridPos.pt[1] += neighborPosPointer[1];
        neighborGridPos.pt[2] += neighborPosPointer[2];
        //if (neighborGridPos[2] != catom->position[2])
            //continue;
        addNeighbor(neighborGridPos);
    }
}

bool Neighborhood::addFirstNeighbor()
{
    for (int i = 0; i < 12; i++) {
        Cell3DPosition neighborGridPos = catom->position;
        int *neighborPosPointer = (catom->position[2]%2) ? NeighborRestriction::neighborDirectionsOdd[i] : NeighborRestriction::neighborDirectionsEven[i];

        neighborGridPos.pt[0] += neighborPosPointer[0];
        neighborGridPos.pt[1] += neighborPosPointer[1];
        neighborGridPos.pt[2] += neighborPosPointer[2];
        //if (neighborGridPos[2] != catom->position[2])
            //continue;
        if (addNeighbor(neighborGridPos))
            return true;
    }
    return false;
}

bool Neighborhood::addNeighbor(Cell3DPosition pos)
{
    Catoms3D::Catoms3DWorld *world = Catoms3D::Catoms3DWorld::getWorld();
    NeighborRestriction neighbors;
    if (world->lattice->isFree(pos) && BlockCode::target->isInTarget(pos)) {
        if (neighbors.isPositionBlockable(pos))
            world->addBlock(0, blockCodeBuilder, pos, LIGHTGREY, 0, false);
        else if (neighbors.isPositionBlocked(pos)) {
            world->addBlock(0, blockCodeBuilder, pos, RED, 0, false);
            numberBlockedModules++;
            cout << "number of blocked modules = " << numberBlockedModules << endl;
            cout << "---- ERROR ----\nPosition " << pos << " blocked" << endl;

            //std::this_thread::sleep_for(std::chrono::milliseconds(100000));
        }
        else {
            world->addBlock(0, blockCodeBuilder, pos, LIGHTGREY, 0, false);
            //std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        world->linkBlock(pos);
        return true;
    }
    return false;
}

void Neighborhood::canFill()
{
    // fill to next line
    if (reconf->isSeedNext())
        addEventAddNextLineNeighbor();

    // fill to left
    if (!BlockCode::target->isInTarget(catom->position.addY(-1).addX(-1)))
        tryAddNeighborToLeft();
    else
        sendMessageToAddLeft();

    // fill to right
    if (!BlockCode::target->isInTarget(catom->position.addY(-1).addX(1)))
        tryAddNeighborToRight();
    else
        sendMessageToAddRight();
}

void Neighborhood::addEventAddNextLineNeighbor() {
    AddNextLine_event *evt = new AddNextLine_event(getScheduler()->now()+MSG_TIME, catom);
    getScheduler()->schedule(evt);
    //getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position)));
}

void Neighborhood::sendMessageToAddLeft() {
    if (catom->getInterface(catom->position.addY(-1))->isConnected()
            && BlockCode::target->isInTarget(catom->position.addY(-1).addX(-1))) {

        CanFillLeft_message *msg = new CanFillLeft_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addY(-1))));

    }
}

void Neighborhood::sendMessageToAddRight() {
    if (catom->getInterface(catom->position.addY(-1))->isConnected()
            && BlockCode::target->isInTarget(catom->position.addY(-1).addX(1))) {
        CanFillRight_message *msg2 = new CanFillRight_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg2, catom->getInterface(catom->position.addY(-1))));
    }
}

void Neighborhood::sendResponseMessageToAddLeft() {
    if (BlockCode::target->isInTarget(catom->position.addX(-1)) &&
            catom->getInterface(catom->position.addX(-1))->connectedInterface != NULL) {
        CanFillLeftResponse_message *msg = new CanFillLeftResponse_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addY(1))));
    }
}

void Neighborhood::sendResponseMessageToAddRight() {
    if (BlockCode::target->isInTarget(catom->position.addX(1)) &&
            catom->getInterface(catom->position.addX(1))->connectedInterface != NULL) {
        CanFillRightResponse_message *msg = new CanFillRightResponse_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addY(1))));
    }
}
