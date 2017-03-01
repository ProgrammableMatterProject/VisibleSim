#include "neighbor.h"
#include "neighborRestriction.h"
#include "catoms3DWorld.h"
#include "../CSG/csgUtils.h"

using namespace Catoms3D;

void Neighbor::addNeighbors(BlockCodeBuilder blockCodeBuilder) {
    for (int i = 0; i < 12; i++) {
        Cell3DPosition neighborGridPos = catom->position;
        int *neighborPosPointer = (catom->position[2]%2) ? NeighborRestriction::neighborDirectionsOdd[i] : NeighborRestriction::neighborDirectionsEven[i];

        neighborGridPos.pt[0] += neighborPosPointer[0];
        neighborGridPos.pt[1] += neighborPosPointer[1];
        neighborGridPos.pt[2] += neighborPosPointer[2];
        if (neighborGridPos[2] != catom->position[2])
            continue;
        addNeighbor(blockCodeBuilder, neighborGridPos);
    }
}

void Neighbor::addNeighbor(BlockCodeBuilder blockCodeBuilder, Cell3DPosition pos) {
    Catoms3DWorld *world = Catoms3DWorld::getWorld();
    NeighborRestriction neighbors;
    Color color;
    if (world->lattice->isFree(pos) && CsgUtils::csgRoot->isInside(CsgUtils::getWorldPosition(pos), color)) {
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

void Neighbor::addNeighborToLeft(BlockCodeBuilder blockCode, Reconf reconf) {
    if (reconf.needSync())
        catom->setColor(RED);
    addNeighbor(blockCode, catom->position.addX(-1));
}

void Neighbor::addNeighborToRight(BlockCodeBuilder blockCode, Reconf reconf) {
    if (reconf.needSync())
        catom->setColor(RED);
    addNeighbor(blockCode, catom->position.addX(1));
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

void Neighbor::sendMessageLineCompleted(Reconf reconf, SIDE_COMPLETED side)
{
    int offset = (side == (SIDE_COMPLETED)LEFT) ? 1 : -1;
    Cell3DPosition neighborPosition = catom->position.addX(offset);
    Message *msg;
    if (side == LEFT) {
        msg = new Left_side_completed_message((reconf.isSeed() ? 1 : 0));
    }
    else {
        msg = new Right_side_completed_message((reconf.isSeed() ? 1 : 0));
    }

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, catom->getInterface(neighborPosition)));
}

void Neighbor::tryAddNextLineNeighbor(BlockCodeBuilder blockCodeBuilder, Reconf &reconf) {
    if  (reconf.isSeed()) {
        addNeighbor(blockCodeBuilder, catom->position.addY(1));
    }
}

void Neighbor::checkLineCompleted(Reconf &reconf) {
    if (reconf.isOnLeftBorder())
        sendMessageLineCompleted(reconf, LEFT);
    if (reconf.isOnRightBorder()) {
        cout << "OKAOSKAO" << endl;
        sendMessageLineCompleted(reconf, RIGHT);
    }
}

