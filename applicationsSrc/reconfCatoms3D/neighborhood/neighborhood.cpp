#include "neighborhood.h"
#include "neighborRestriction.h"
#include "catoms3DWorld.h"

#define WAIT_TIME 5

Neighborhood::Neighborhood(Catoms3D::Catoms3DBlock *c, Reconf *r, Sync *s, SyncCCW *sccw, BlockCodeBuilder bcb)
{
    catom = c;
    reconf = r;
    sync = s;
    syncCCW = sccw;
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

void Neighborhood::tryAddNextLineNeighbor()
{
    if  (reconf->isSeedNext()) {
        if (reconf->isLineCompleted()) {
            addNeighbor(catom->position.addY(1));
        }
    }
}

void Neighborhood::tryAddPreviousLineNeighbor()
{
    if  (reconf->isSeedPrevious()) {
        if (reconf->isLineCompleted()) {
            addNeighbor(catom->position.addY(-1));
        }
    }
}

bool Neighborhood::isFirstCatomOfLine()
{
    if (catom->getInterface(catom->position.addX(-1))->connectedInterface == NULL &&
            catom->getInterface(catom->position.addX(1))->connectedInterface == NULL)
        return true;
    return false;
}

void Neighborhood::tryAddNeighbors()
{
    if (reconf->needSyncToLeft()) {
        if (syncCCW->isInternalBorder(1)) {
            syncCCW->sync();
        }
        else {
            addNeighborToLeft();
            addNeighborToRight();
        }
    }
    else if (reconf->needSyncToRight()) {
        if (!syncCCW->isInternalBorder(0)) {
            reconf->setSeedNext();
            tryAddNextLineNeighbor();
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
            world->addBlock(0, blockCodeBuilder, pos, PINK, 0, false);
        else if (neighbors.isPositionBlocked(pos))
            world->addBlock(0, blockCodeBuilder, pos, RED, 0, false);
        else {

            world->addBlock(0, blockCodeBuilder, pos, WHITE, 0, false);
        }
        world->linkBlock(pos);
    }
}

