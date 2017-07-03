#include "neighborhood.h"
#include "neighborRestriction.h"
#include "catoms3DWorld.h"

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
    if (BlockCode::target->isInTarget(catom->position.addZ(1))) {
        cout << catom->blockId << endl;
        addNeighbor(catom->position.addZ(1));
    }
    else if (BlockCode::target->isInTarget(catom->position.addY(1).addZ(1))) {
        cout << catom->blockId << endl;
        addNeighbor(catom->position.addY(1).addZ(1));
    }
}

void Neighborhood::addNeighborToPreviousPlane()
{
    if (BlockCode::target->isInTarget(catom->position.addZ(-1)))
        addNeighbor(catom->position.addZ(-1));
    else if (BlockCode::target->isInTarget(catom->position.addY(1).addZ(-1)))
        addNeighbor(catom->position.addY(1).addZ(-1));
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
        if (reconf->isLineCompleted() || !reconf->createdFromPrevious) {
            addNeighbor(catom->position.addY(1));
        }
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
    if (reconf->needSyncToLeftNext()) {
        if (syncNext->isInternalBorder(1)) {
            //cout << "Sync to Left Next " << catom->blockId << endl;
            syncNext->sync();
        }
        else {
            addNeighborToLeft();
        }
        addNeighborToRight();
    }
    else if (reconf->needSyncToLeftPrevious()) {
        if (syncPrevious->isInternalBorder(3)) {
            //cout << "Sync to Left Previous " << catom->blockId << endl;
            //catom->setColor(BLACK);
            syncPrevious->sync();
        }
        else {
            addNeighborToRight();
        }
        addNeighborToLeft();
    }
    else if (reconf->needSyncToRightNext()) {
        addNeighborToLeft();
    }
    else if (reconf->needSyncToRightPrevious()) {
        addNeighborToRight();
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
    Catoms3D::Catoms3DWorld *world = Catoms3D::Catoms3DWorld::getWorld();
    NeighborRestriction neighbors;
    if (world->lattice->isFree(pos) && BlockCode::target->isInTarget(pos)) {
        if (neighbors.isPositionBlockable(pos))
            world->addBlock(0, blockCodeBuilder, pos, WHITE, 0, false);
        else if (neighbors.isPositionBlocked(pos))
            world->addBlock(0, blockCodeBuilder, pos, RED, 0, false);
        else {
            world->addBlock(0, blockCodeBuilder, pos, WHITE, 0, false);
        }
        world->linkBlock(pos);
    }
}
