#include "reconf.h"
#include "catoms3DWorld.h"

Reconf::Reconf(Catoms3D::Catoms3DBlock *c) : catom(c)
{
    init = false;
    seedNext = false;
    seedPrevious = false;
    planeFinished = false;
    planeFinishedAck = false;
    syncPlaneNodeParent = NULL;
    syncPlaneNode = NULL;
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

bool Reconf::checkPlaneCompleted()
{
    if (isHighest()) {
        return true;
    }
    return false;
}

bool Reconf::isHighestOfBorder(int idx) {
    Cell3DPosition currentPos = catom->position;
    getNextBorderNeighbor(idx, currentPos);

    while(currentPos != catom->position) {
        if (currentPos[1] > catom->position[1] ||
                (currentPos[1] == catom->position[1] && currentPos[0] < catom->position[0]))
            return false;
        getNextBorderNeighbor(idx, currentPos);
    }
    return true;
}

int Reconf::getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos) {
    vector<pair<int, int>> ccw_order = {{0,-1}, {1,0}, {0,1}, {-1,0}};
    int newIdx;

    for (int i = 0; i < 4; i++) {
        newIdx = (((idx+i-1)%4)+4)%4;
        Cell3DPosition nextPos = currentPos.addX(ccw_order[newIdx].first)
                                          .addY(ccw_order[newIdx].second);
        if (BlockCode::target->isInTarget(nextPos)) {
            idx = newIdx;
            currentPos = nextPos;
            if (i == 0)
                return 1;
            else if (i == 2)
                return -1;
            else if (i == 3)
                return -2;
            return 0;
        }
    }
    return 0;
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

bool Reconf::isHighest()
{
    Cell3DPosition pos = catom->position;
    if (!isOnBorder() || !BlockCode::target->isInTarget(pos))
        return false;
    if (!BlockCode::target->isInTarget(pos.addX(-1)))
        return isHighestOfBorder(1);
    if (!BlockCode::target->isInTarget(pos.addX(1)))
        return isHighestOfBorder(3);
    if (!BlockCode::target->isInTarget(pos.addY(-1)))
        return isHighestOfBorder(2);
    if (!BlockCode::target->isInTarget(pos.addY(1)))
        return isHighestOfBorder(0);
    return false;
}
