#include "reconf.h"
#include "catoms3DWorld.h"

Reconf::Reconf(Catoms3D::Catoms3DBlock *c) : catom(c)
{
    init = false;
    seedNext = false;
    seedPrevious = false;
    floor = -1;
    confirmNorthLeft = false;
    confirmNorthRight = false;
    confirmWestLeft = false;
    confirmWestRight = false;
    confirmSouthLeft = false;
    confirmSouthRight = false;
    confirmEastLeft = false;
    confirmEastRight = false;

    childConfirm = 0;
    nChildren = 0;

    isLineParent = false;
    isPlaneParent = false;
    isPlaneCompleted = false;
    parentPlaneFinished = false;
    planeSeed = false;
}

bool Reconf::isInternalSeedNext()
{
    if (catom->getInterface(catom->position.addY(1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.addX(1).addY(1)) &&
            BlockCode::target->isInTarget(catom->position.addY(1)) ){
        return true;
    }
    return false;
}

bool Reconf::isInternalSeedPrevious()
{
    if (catom->getInterface(catom->position.addY(-1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.addX(-1).addY(-1)) &&
            BlockCode::target->isInTarget(catom->position.addY(-1)) ){
        return true;
    }
    return false;
}

bool Reconf::isBorderSeedNext()
{
    if (catom->getInterface(catom->position.addY(1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.addX(1)) &&
        BlockCode::target->isInTarget(catom->position.addY(1)) ){
        return true;
    }
    return false;
}

bool Reconf::isBorderSeedPrevious()
{
    if (catom->getInterface(catom->position.addY(-1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.addX(-1)) &&
        BlockCode::target->isInTarget(catom->position.addY(-1)) ){
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

bool Reconf::arePreviousPlaneNeighborsComplete()
{
    Cell3DPosition neighbors[4];
    if (catom->position[2] % 2) {
        neighbors[0] = catom->position.addZ(-1).addX(1);
        neighbors[1] = catom->position.addZ(-1);
        neighbors[2] = catom->position.addZ(-1).addY(1);
        neighbors[3] = catom->position.addZ(-1).addX(1).addY(1);
    }
    else {
        neighbors[0] = catom->position.addZ(-1).addX(-1);
        neighbors[1] = catom->position.addZ(-1);
        neighbors[2] = catom->position.addZ(-1).addY(-1);
        neighbors[3] = catom->position.addZ(-1).addX(-1).addY(-1);
    }
    for (int i = 0; i < 4; i++) {
        if (BlockCode::target->isInTarget(neighbors[i]) &&
                !catom->getInterface(neighbors[i])->isConnected())
            return false;
    }
    return true;
}

bool Reconf::canAddNextPlaneSeed()
{
    Cell3DPosition neighbors[2];
    if (catom->position[2] % 2) {
        neighbors[0] = catom->position.addY(-1);
        neighbors[1] = catom->position.addX(-1);
    }
    else {
        neighbors[0] = catom->position.addY(1);
        neighbors[1] = catom->position.addX(1);
    }
    for (int i = 0; i < 2; i++) {
        if (BlockCode::target->isInTarget(neighbors[i]) &&
                !catom->getInterface(neighbors[i])->isConnected()) {
            return false;
        }
    }
    return true;
}


bool Reconf::isPlaneSeed()
{
    if (planeSeed)
        return true;
    return planeSeed = Border::isPlaneSeed(catom->position);
}


bool Reconf::isOnBorder()
{
    Cell3DPosition pos = catom->position;
    if (BlockCode::target->isInTarget(pos) &&
        (!BlockCode::target->isInTarget(pos.addX(-1)) ||
        !BlockCode::target->isInTarget(pos.addX(1)) ||
        !BlockCode::target->isInTarget(pos.addY(-1)) ||
        !BlockCode::target->isInTarget(pos.addY(1))))
        return true;
    return false;
}

bool Reconf::areNeighborsPlaced()
{
    Cell3DPosition pos = catom->position;
    if (BlockCode::target->isInTarget(pos.addX(-1)) && !catom->getInterface(pos.addX(-1))->isConnected())
        return false;
    if (BlockCode::target->isInTarget(pos.addX(1)) && !catom->getInterface(pos.addX(1))->isConnected())
        return false;
    if (BlockCode::target->isInTarget(pos.addY(-1)) && !catom->getInterface(pos.addY(-1))->isConnected())
        return false;
    if (BlockCode::target->isInTarget(pos.addY(1)) && !catom->getInterface(pos.addY(1))->isConnected())
        return false;
    return true;
}

