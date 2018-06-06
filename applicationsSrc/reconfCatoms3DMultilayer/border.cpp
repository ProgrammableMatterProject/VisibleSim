#include "border.h"

bool Border::isPlaneSeed(Cell3DPosition catomPos)
{
    // To avoid line cases
    if (couldBeSeed(catomPos.addX(1)) || couldBeSeed(catomPos.addY(-1)))
        return false;

    if (isSeedBorderOnNextPlane(catomPos.addZ(1))) {
        return true;
    }

    if (isSeedBorderOnCurrentPlane(catomPos)) {
        return true;
        //Cell3DPosition initialPos = getCurrentBorderForNextPlane(catomPos).addZ(1);
        //Cell3DPosition currentPos = initialPos;
        //int idx = getIdxForBorder(currentPos);
        //int nTurns = 0;
        //nTurns += Border::getNextBorderNeighbor(idx, currentPos);
        //while (currentPos != initialPos) {
            //if (isSeedBorderOnNextPlane(currentPos) && BlockCode::target->isInTarget(currentPos.addZ(-1)))
                //return false;
            //nTurns += getNextBorderNeighbor(idx, currentPos);
        //}
        //if (nTurns<= 0) return true;
    }
    return false;
}

bool Border::couldBeSeed(Cell3DPosition pos)
{
    if (BlockCode::target->isInTarget(pos) && BlockCode::target->isInTarget(pos.addZ(1)))
        if (isOnBorder(pos) || isOnBorder(pos.addZ(1)))
            return true;
    return false;
}

bool Border::isSeedBorderOnCurrentPlane(Cell3DPosition pos)
{
    if (BlockCode::target->isInTarget(pos.addZ(1)) && isOnBorder(pos) && isLowestOfBorderOnCurrentPlane(pos))
        return true;
    return false;
}

bool Border::isSeedBorderOnNextPlane(Cell3DPosition pos)
{
    if (BlockCode::target->isInTarget(pos) && isOnBorder(pos) && isLowestOfBorderOnNextPlane(pos))
        return true;
    return false;
}

bool Border::isLowestOfBorderOnCurrentPlane(Cell3DPosition pos) {
    int nTurns = 0;
    int idx = getIdxForBorder(pos);
    Cell3DPosition currentPos = pos;
    nTurns += getNextBorderNeighbor(idx, currentPos);
    while(currentPos != pos) {
        if ((currentPos[1] < pos[1] ||
                (currentPos[1] == pos[1] && currentPos[0] > pos[0])) && BlockCode::target->isInTarget(currentPos.addZ(1))) {
            return false;
        }
        nTurns += getNextBorderNeighbor(idx, currentPos);
    }
    if (nTurns <= 0) return true;
    return false;
}

bool Border::isOnBorder(Cell3DPosition pos)
{
    if (BlockCode::target->isInTarget(pos) &&
        (!BlockCode::target->isInTarget(pos.addX(-1)) ||
        !BlockCode::target->isInTarget(pos.addX(1)) ||
        !BlockCode::target->isInTarget(pos.addY(-1)) ||
        !BlockCode::target->isInTarget(pos.addY(1))))
        return true;
    return false;
}

bool Border::isLowestOfBorderOnNextPlane(Cell3DPosition pos) {
    int nTurns = 0;
    int idx = getIdxForBorder(pos);
    Cell3DPosition currentPos = pos;
    nTurns += getNextBorderNeighbor(idx, currentPos);
    while(currentPos != pos) {
        if ((currentPos[1] < pos[1] ||
                (currentPos[1] == pos[1] && currentPos[0] > pos[0])) && Catoms3D::getWorld()->getBlockByPosition(currentPos.addZ(-1)) != NULL)
        {
            return false;

        }
        nTurns += getNextBorderNeighbor(idx, currentPos);
    }
    if (nTurns > 0) return false;
    return true;
}

Cell3DPosition Border::getCurrentBorderForNextPlane(Cell3DPosition catomPos)
{
    Cell3DPosition currentPos = catomPos;
    int idx = getIdxForBorder(currentPos);
    getNextBorderNeighbor(idx, currentPos);
    while (currentPos != catomPos) {
        Cell3DPosition nextPlanePos = currentPos.addZ(1);
        if (BlockCode::target->isInTarget(nextPlanePos) &&
                isOnBorder(nextPlanePos))
            return currentPos;
        getNextBorderNeighbor(idx, currentPos);
    }
    return currentPos;
}

int Border::getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos) {
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

int Border::getIdxForBorder(Cell3DPosition pos) {
    if (isOnBorder(pos)  && BlockCode::target->isInTarget(pos)) {
        if (BlockCode::target->isInTarget(pos.addY(-1)) &&
            BlockCode::target->isInTarget(pos.addY(1)) &&
            !BlockCode::target->isInTarget(pos.addX(-1)) &&
            BlockCode::target->isInTarget(pos.addX(1)))
            return 0;
        if (BlockCode::target->isInTarget(pos.addY(-1)) &&
            !BlockCode::target->isInTarget(pos.addY(1)) &&
            BlockCode::target->isInTarget(pos.addX(-1)) &&
            BlockCode::target->isInTarget(pos.addX(1)))
            return 3;
        if (BlockCode::target->isInTarget(pos.addY(-1)) &&
            BlockCode::target->isInTarget(pos.addY(1)) &&
            BlockCode::target->isInTarget(pos.addX(-1)) &&
            !BlockCode::target->isInTarget(pos.addX(1)))
            return 2;
        if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
            BlockCode::target->isInTarget(pos.addY(1)) &&
            BlockCode::target->isInTarget(pos.addX(-1)) &&
            BlockCode::target->isInTarget(pos.addX(1)))
            return 1;
        // 2 empty neighbors
        if (BlockCode::target->isInTarget(pos.addY(-1)) &&
            !BlockCode::target->isInTarget(pos.addY(1)) &&
            !BlockCode::target->isInTarget(pos.addX(-1)) &&
            BlockCode::target->isInTarget(pos.addX(1)))
            return 3;
        if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
            BlockCode::target->isInTarget(pos.addY(1)) &&
            !BlockCode::target->isInTarget(pos.addX(-1)) &&
            BlockCode::target->isInTarget(pos.addX(1)))
            return 0;
        if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
            BlockCode::target->isInTarget(pos.addY(1)) &&
            BlockCode::target->isInTarget(pos.addX(-1)) &&
            !BlockCode::target->isInTarget(pos.addX(1)))
            return 1;
        if (BlockCode::target->isInTarget(pos.addY(-1)) &&
            !BlockCode::target->isInTarget(pos.addY(1)) &&
            BlockCode::target->isInTarget(pos.addX(-1)) &&
            !BlockCode::target->isInTarget(pos.addX(1)))
            return 2;
        if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
            !BlockCode::target->isInTarget(pos.addY(1)) &&
            BlockCode::target->isInTarget(pos.addX(-1)) &&
            BlockCode::target->isInTarget(pos.addX(1)))
            return 0;
        // critical case?
        if (BlockCode::target->isInTarget(pos.addY(-1)) &&
            BlockCode::target->isInTarget(pos.addY(1)) &&
            !BlockCode::target->isInTarget(pos.addX(-1)) &&
            !BlockCode::target->isInTarget(pos.addX(1)))
            return 2;
        if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
            BlockCode::target->isInTarget(pos.addY(1)) &&
            !BlockCode::target->isInTarget(pos.addX(-1)) &&
            BlockCode::target->isInTarget(pos.addX(1)))
            return 1;
        // 3 empty neighbors
        if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
            BlockCode::target->isInTarget(pos.addY(1)) &&
            !BlockCode::target->isInTarget(pos.addX(-1)) &&
            !BlockCode::target->isInTarget(pos.addX(1)))
            return 0;
        if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
            !BlockCode::target->isInTarget(pos.addY(1)) &&
            !BlockCode::target->isInTarget(pos.addX(-1)) &&
            BlockCode::target->isInTarget(pos.addX(1)))
            return 3;
        if (BlockCode::target->isInTarget(pos.addY(-1)) &&
            !BlockCode::target->isInTarget(pos.addY(1)) &&
            !BlockCode::target->isInTarget(pos.addX(-1)) &&
            !BlockCode::target->isInTarget(pos.addX(1)))
            return 2;
        if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
            !BlockCode::target->isInTarget(pos.addY(1)) &&
            BlockCode::target->isInTarget(pos.addX(-1)) &&
            !BlockCode::target->isInTarget(pos.addX(1)))
            return 1;
    }
    return 0;
}
