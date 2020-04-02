#include "reconf.h"
#include "robots/catoms3D/catoms3DWorld.h"

Reconf::Reconf(Catoms3D::Catoms3DBlock *c) : catom(c)
{
    init = false;
    childConfirm = 0;
    nChildren = 0;
    seedNext = false;
    seedPrevious = false;
    isPlaneCompleted = false;
    syncPlaneNodeParent = NULL;
    syncPlaneNode = NULL;

    isPlaneParent = false;

    canFillLeft = false;
    canFillRight = false;
}

bool Reconf::isInternalSeedNext()
{
    if (catom->getInterface(catom->position.offsetY(1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.offsetX(1).offsetY(1)) &&
            BlockCode::target->isInTarget(catom->position.offsetY(1)) ){
        return true;
    }
    return false;
}

bool Reconf::isInternalSeedPrevious()
{
    if (catom->getInterface(catom->position.offsetY(-1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.offsetX(-1).offsetY(-1)) &&
            BlockCode::target->isInTarget(catom->position.offsetY(-1)) ){
        return true;
    }
    return false;
}

bool Reconf::isBorderSeedNext()
{
    if (catom->getInterface(catom->position.offsetY(1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.offsetX(1)) &&
        BlockCode::target->isInTarget(catom->position.offsetY(1)) ){
        return true;
    }
    return false;
}

bool Reconf::isBorderSeedPrevious()
{
    if (catom->getInterface(catom->position.offsetY(-1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.offsetX(-1)) &&
        BlockCode::target->isInTarget(catom->position.offsetY(-1)) ){
        return true;
    }
    return false;
}

// A sync module cant be seed to avoid two seeds constructing the same line (merge lines and avoid cycle)
bool Reconf::isSeedNext()
{
    seedNext = seedNext || ((isInternalSeedNext() || isBorderSeedNext()));// && !needSyncToRightNext());
    //if (seedNext)
        //catom->setColor(LIGHTBLUE);
    return seedNext;
}

bool Reconf::isSeedPrevious()
{
    seedPrevious = seedPrevious || ((isInternalSeedPrevious() || isBorderSeedPrevious()));// && !needSyncToRightPrevious());
    //if (seedPrevious)
        //catom->setColor(YELLOW);
    return seedPrevious;
}

bool Reconf::isOnBorder()
{
    Cell3DPosition pos = catom->position;
    if (BlockCode::target->isInTarget(pos) &&
        (!BlockCode::target->isInTarget(pos.offsetX(-1)) ||
        !BlockCode::target->isInTarget(pos.offsetX(1)) ||
        !BlockCode::target->isInTarget(pos.offsetY(-1)) ||
        !BlockCode::target->isInTarget(pos.offsetY(1))))
        return true;
    return false;
}

bool Reconf::areNeighborsPlaced()
{
    Cell3DPosition pos = catom->position;
    if (BlockCode::target->isInTarget(pos.offsetX(-1)) && !catom->getInterface(pos.offsetX(-1))->isConnected())
        return false;
    if (BlockCode::target->isInTarget(pos.offsetX(1)) && !catom->getInterface(pos.offsetX(1))->isConnected())
        return false;
    if (BlockCode::target->isInTarget(pos.offsetY(-1)) && !catom->getInterface(pos.offsetY(-1))->isConnected())
        return false;
    if (BlockCode::target->isInTarget(pos.offsetY(1)) && !catom->getInterface(pos.offsetY(1))->isConnected())
        return false;
    return true;
}
