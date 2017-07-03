#include "syncPlane.h"
#include "catoms3DWorld.h"

SyncPlane::SyncPlane(Catoms3D::Catoms3DBlock *c, Reconf *r) {
    this->catom = c;
    this->reconf = r;
}

bool SyncPlane::isOnBorder()
{
    Cell3DPosition pos = catom->position.addZ(1);
    if (BlockCode::target->isInTarget(pos) &&
        (!BlockCode::target->isInTarget(pos.addX(-1)) ||
        !BlockCode::target->isInTarget(pos.addX(1)) ||
        !BlockCode::target->isInTarget(pos.addY(-1)) ||
        !BlockCode::target->isInTarget(pos.addY(1))))
        return true;
    return false;
}

bool SyncPlane::isSeed()
{
    Cell3DPosition pos = catom->position.addZ(1);
    if (!isOnBorder() || !BlockCode::target->isInTarget(pos))
        return false;
    //TODO check correctness
    if (!BlockCode::target->isInTarget(pos.addX(-1)))
        return isLowestOfBorder(1);
    if (!BlockCode::target->isInTarget(pos.addX(1)))
        return isLowestOfBorder(3);
    if (!BlockCode::target->isInTarget(pos.addY(-1)))
        return isLowestOfBorder(2);
    if (!BlockCode::target->isInTarget(pos.addY(1)))
        return isLowestOfBorder(0);
    return false;
}

bool SyncPlane::isLowestOfBorder(int idx) {
    int nTurns = 0;
    Cell3DPosition currentPos = catom->position.addZ(1);
    if (catom->blockId == 618) {
        cout << ">>>" << currentPos << endl;
    }

    nTurns += getNextBorderNeighbor(idx, currentPos);
        if (catom->blockId == 618) {
            cout << ">>>" << currentPos << endl;
        }

    while(currentPos != catom->position.addZ(1)) {

        if (catom->blockId == 618) {
            cout << ">>>" << currentPos << endl;
        }
        if (currentPos[1] < catom->position[1] ||
                (currentPos[1] == catom->position[1] && currentPos[0] < catom->position[0]))
        {
            if (catom->blockId == 618) {
                cout << "error in " << currentPos << endl;
            }
            return false;
        }
        nTurns += getNextBorderNeighbor(idx, currentPos);
    }
    return true;
}

int SyncPlane::getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos) {
    vector<pair<int, int>> ccw_order = {{0,-1}, {1,0}, {0,1}, {-1,0}};
    int newIdx;
    for (int i = 0; i < 4; i++) {
        newIdx = (((idx+i-1)%4)+4)%4;
        Cell3DPosition nextPos = currentPos.addX(ccw_order[newIdx].first)
                                          .addY(ccw_order[newIdx].second);
        if (BlockCode::target->isInTarget(nextPos)) {
            //Catoms3D::Catoms3DWorld::getWorld()->lattice->getBlock(nextPos.addZ(-1))->setColor(PINK);
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
