#include "neighborhood.h"
#include "neighborRestriction.h"
#include "catoms3DWorld.h"

#define MSG_TIME rand()%10
#define MSG_TIME_ADD 100+rand()%10

int Neighborhood::numberBlockedModules = 0;
int Neighborhood::numberMessagesToAddBlock = 0;

Neighborhood::Neighborhood(Catoms3D::Catoms3DBlock *c, Reconf *r, BlockCodeBuilder bcb)
{
    catom = c;
    reconf = r;
    blockCodeBuilder = bcb;
}

void Neighborhood::addNeighborToLeft()
{
    Cell3DPosition pos = catom->position.addX(-1);
    if (BlockCode::target->isInTarget(pos) && !catom->getInterface(pos)->isConnected()) {
        if (!Scheduler::getScheduler()->hasEvent(ADDLEFTBLOCK_EVENT_ID, catom->blockId)) {
            getScheduler()->schedule(new AddLeftBlock_event(getScheduler()->now()+MSG_TIME_ADD, catom));
            reconf->nChildren++;
        }
    }
}

void Neighborhood::addNeighborToRight()
{
    Cell3DPosition pos = catom->position.addX(1);
    if (BlockCode::target->isInTarget(pos) && !catom->getInterface(pos)->isConnected()) {
        if (!Scheduler::getScheduler()->hasEvent(ADDRIGHTBLOCK_EVENT_ID, catom->blockId)) {
            getScheduler()->schedule(new AddRightBlock_event(getScheduler()->now()+MSG_TIME_ADD, catom));
            reconf->nChildren++;
        }
    }
}

void Neighborhood::addNextLineNeighbor()
{
    if (!catom->getInterface(catom->position.addY(1))->isConnected()) {
        reconf->nChildren++;
        addNeighbor(catom->position.addY(1));
    }
}

void Neighborhood::addPreviousLineNeighbor()
{
    if (!catom->getInterface(catom->position.addY(-1))->isConnected()) {
        reconf->nChildren++;
        addNeighbor(catom->position.addY(-1));
    }
}

void Neighborhood::addNeighborToNextPlane()
{
    addNeighbor(catom->position.addZ(1));
}

void Neighborhood::addNeighborToPreviousPlane()
{
    addNeighbor(catom->position.addZ(-1));
}

bool Neighborhood::isFirstCatomOfLine()
{
    if (!catom->getInterface(catom->position.addX(-1))->isConnected() &&
            !catom->getInterface(catom->position.addX(1))->isConnected())
        return true;
    return false;
}

bool Neighborhood::isFirstCatomOfPlane()
{
    if (isFirstCatomOfLine() &&
            !catom->getInterface(catom->position.addY(-1))->isConnected() &&
            !catom->getInterface(catom->position.addY(1))->isConnected())
        return true;
    return false;
}


void Neighborhood::addNeighbors()
{
    addNext();
    addPrevious();
    addLeft();
    addRight();
    addNextPlane();
}

void Neighborhood::addNextPlane() {
    //if (catom->blockId == 158)
        //cout << reconf->isPlaneSeed() << ' '  << reconf->canAddNextPlaneSeed() << endl;
    if (reconf->isPlaneSeed() && reconf->canAddNextPlaneSeed()) {
        addNeighborToNextPlane();
    }
}

void Neighborhood::addLeft() {
    if(BlockCode::target->isInTarget(catom->position.addX(-1)) &&
       !catom->getInterface(catom->position.addX(-1))->isConnected()) {
        if (!BlockCode::target->isInTarget(catom->position.addY(-1).addX(-1)) || !catom->getInterface(catom->position.addY(-1))->isConnected()) {
            if (reconf->floor == 0 || (reconf->confirmWestLeft && reconf->confirmWestRight) || reconf->parentPlaneFinished)
                addNeighborToLeft();
        }
        else {
            //TODO send just once
            sendMessageToAddLeft();
        }
    }
}

void Neighborhood::addRight() {
    if(BlockCode::target->isInTarget(catom->position.addX(1)) &&
       !catom->getInterface(catom->position.addX(1))->isConnected()) {
        if (!BlockCode::target->isInTarget(catom->position.addY(1).addX(1)) || !catom->getInterface(catom->position.addY(1))->isConnected()) {
            if (reconf->floor == 0 || reconf->arePreviousPlaneNeighborsComplete() || reconf->parentPlaneFinished)
                addNeighborToRight();
        }
        else {
            //TODO send just once
            sendMessageToAddRight();
        }
    }
}

void Neighborhood::addNext() {
        //if (catom->blockId == 39) {
            //cout << "----" << endl;
            //cout << reconf->isSeedNext() << endl;
            //cout << reconf->confirmNorthLeft << endl;
            //cout << reconf->confirmNorthRight << endl;
            //cout << reconf->floor << endl;
        //}
    if (reconf->isSeedNext() && ((reconf->confirmNorthLeft && reconf->confirmNorthRight) || reconf->floor == 0 || reconf->parentPlaneFinished)) {
        addEventAddNextLineNeighbor();
    }
}
void Neighborhood::addPrevious() {
    if (reconf->isSeedPrevious() && ((reconf->confirmSouthLeft && reconf->confirmSouthRight) || reconf->floor == 0 || reconf->parentPlaneFinished)) {
        addEventAddPreviousLineNeighbor();
    }
}

void Neighborhood::addEventAddNextLineNeighbor() {
    if (!Scheduler::getScheduler()->hasEvent(ADDNEXTLINE_EVENT_ID, catom->blockId)
            && !catom->getInterface(catom->position.addY(1))->isConnected()) {
        AddNextLine_event *evt = new AddNextLine_event(getScheduler()->now()+MSG_TIME_ADD, catom);
        getScheduler()->schedule(evt);
    }
}

void Neighborhood::addEventAddPreviousLineNeighbor() {
    if (!Scheduler::getScheduler()->hasEvent(ADDPREVIOUSLINE_EVENT_ID, catom->blockId)
            && !catom->getInterface(catom->position.addY(-1))->isConnected()) {
        AddPreviousLine_event *evt = new AddPreviousLine_event(getScheduler()->now()+MSG_TIME_ADD, catom);
        getScheduler()->schedule(evt);
    }
}

void Neighborhood::sendMessageToAddLeft() {
    if (catom->getInterface(catom->position.addY(-1))->isConnected()
            && BlockCode::target->isInTarget(catom->position.addY(-1).addX(-1))) {
        CanFillLeft_message *msg = new CanFillLeft_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addY(-1))));
        numberMessagesToAddBlock++;
        return;
    }

    addNeighborToLeft();
}

void Neighborhood::sendMessageToAddRight() {
    if (catom->getInterface(catom->position.addY(1))->isConnected()
            && BlockCode::target->isInTarget(catom->position.addY(1).addX(1))) {
        CanFillRight_message *msg = new CanFillRight_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addY(1))));
        numberMessagesToAddBlock++;
        return;
    }

    addNeighborToRight();
}

void Neighborhood::sendResponseMessageToAddLeft() {
    if (catom->getInterface(catom->position.addY(1))->isConnected() &&
            catom->getInterface(catom->position.addX(-1))->isConnected()) {
        CanFillLeftResponse_message *msg = new CanFillLeftResponse_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addY(1))));
        numberMessagesToAddBlock++;
    }
}

void Neighborhood::sendResponseMessageToAddRight() {
    if (catom->getInterface(catom->position.addX(1))->isConnected() &&
            catom->getInterface(catom->position.addY(-1))->isConnected()) {
        CanFillRightResponse_message *msg = new CanFillRightResponse_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.addY(-1))));
        numberMessagesToAddBlock++;
    }
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
        Color c = BlockCode::target->getTargetColor(pos);
        if (neighbors.isPositionBlockable(pos))
            //world->addBlock(0, blockCodeBuilder, pos, LIGHTGREY, 0, false);
            world->addBlock(0, blockCodeBuilder, pos, c, 0, false);
        else if (neighbors.isPositionBlocked(pos)) {
            world->addBlock(0, blockCodeBuilder, pos, RED, 0, false);
            numberBlockedModules++;
            cout << "number of blocked modules = " << numberBlockedModules << endl;
            cout << "---- ERROR ----\nPosition " << pos << " blocked" << endl;

            std::this_thread::sleep_for(std::chrono::milliseconds(100000));
        }
        else {
            //world->addBlock(0, blockCodeBuilder, pos, LIGHTGREY, 0, false);
            world->addBlock(0, blockCodeBuilder, pos, c, 0, false);
            //std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        world->linkBlock(pos);
        return true;
    }
    return false;
}


void Neighborhood::checkDependencies() {
    if (catom->position[2]%2) {
        if (!(BlockCode::target->isInTarget(catom->position.addZ(-1).addY(2))))
            reconf->confirmNorthLeft = true;
        if (!(BlockCode::target->isInTarget(catom->position.addZ(-1).addY(2).addX(1)))) {
            reconf->confirmNorthRight = true;
        }
        if (!(BlockCode::target->isInTarget(catom->position.addZ(-1).addX(-1)))) {
            reconf->confirmWestLeft = true;
        }
        if (!(BlockCode::target->isInTarget(catom->position.addZ(-1).addY(1).addX(-1)))) {
            reconf->confirmWestRight = true;
        }
        if (!(BlockCode::target->isInTarget(catom->position.addZ(-1).addY(-1).addX(1)))) {
            reconf->confirmSouthLeft = true;
        }
        if (!(BlockCode::target->isInTarget(catom->position.addZ(-1).addY(-1)))) {
            reconf->confirmSouthRight = true;
        }
        if (!(BlockCode::target->isInTarget(catom->position.addZ(-1).addY(1).addX(2)))) {
            reconf->confirmEastLeft = true;
        }
        if (!(BlockCode::target->isInTarget(catom->position.addZ(-1).addX(2)))) {
            reconf->confirmEastRight = true;
        }
    }
    else {
        if (!BlockCode::target->isInTarget(catom->position.addZ(-1).addY(1).addX(-1)))
            reconf->confirmNorthLeft = true;
        if (!BlockCode::target->isInTarget(catom->position.addZ(-1).addY(1)))
            reconf->confirmNorthRight = true;
    }
}
void Neighborhood::sendMessageToNextPlaneNorthLeft() {
    if (catom->getInterface(5)->isConnected()) {
        NextPlaneConfirmationNorthLeft_message *msg = new NextPlaneConfirmationNorthLeft_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(5)));
        numberMessagesToAddBlock++;
    }
}

void Neighborhood::sendMessageToNextPlaneNorthRight() {
    if (catom->getInterface(4)->isConnected()) {
        NextPlaneConfirmationNorthRight_message *msg = new NextPlaneConfirmationNorthRight_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(4)));
        numberMessagesToAddBlock++;
    }
}

void Neighborhood::sendMessageToNextPlaneWestLeft() {
    if (catom->getInterface(2)->isConnected()) {
        NextPlaneConfirmationWestLeft_message *msg = new NextPlaneConfirmationWestLeft_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(2)));
        numberMessagesToAddBlock++;
    }
}

void Neighborhood::sendMessageToNextPlaneWestRight() {
    if (catom->getInterface(5)->isConnected()) {
        NextPlaneConfirmationWestRight_message *msg = new NextPlaneConfirmationWestRight_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(5)));
        numberMessagesToAddBlock++;
    }
}

void Neighborhood::sendMessageToNextPlaneSouthLeft() {
    if (catom->getInterface(3)->isConnected()) {
        NextPlaneConfirmationSouthLeft_message *msg = new NextPlaneConfirmationSouthLeft_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(3)));
        numberMessagesToAddBlock++;
    }
}

void Neighborhood::sendMessageToNextPlaneSouthRight() {
    if (catom->getInterface(2)->isConnected()) {
        NextPlaneConfirmationSouthRight_message *msg = new NextPlaneConfirmationSouthRight_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(2)));
        numberMessagesToAddBlock++;
    }
}

void Neighborhood::sendMessageToNextPlaneEastLeft() {
    if (catom->getInterface(4)->isConnected()) {
        NextPlaneConfirmationEastLeft_message *msg = new NextPlaneConfirmationEastLeft_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(4)));
        numberMessagesToAddBlock++;
    }
}

void Neighborhood::sendMessageToNextPlaneEastRight() {
    if (catom->getInterface(3)->isConnected()) {
        NextPlaneConfirmationEastRight_message *msg = new NextPlaneConfirmationEastRight_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(3)));
        numberMessagesToAddBlock++;
    }
}
