#include "syncPlane.h"
#include "catoms3DWorld.h"

SyncPlane_node *SyncPlane_node_manager::root = new SyncPlane_node(1, 0);

SyncPlane::SyncPlane(Catoms3D::Catoms3DBlock *c, Reconf *r) {
    this->catom = c;
    this->reconf = r;
}

bool SyncPlane::isOnBorder(Cell3DPosition pos)
{
    if (BlockCode::target->isInTarget(pos) &&
        (!BlockCode::target->isInTarget(pos.addX(-1)) ||
        !BlockCode::target->isInTarget(pos.addX(1)) ||
        !BlockCode::target->isInTarget(pos.addY(-1)) ||
        !BlockCode::target->isInTarget(pos.addY(1))))
        return true;
    return false;
}

int SyncPlane::getIdxForBorder(Cell3DPosition pos) {
    if (isOnBorder(pos)  && BlockCode::target->isInTarget(pos)) {
        if (!BlockCode::target->isInTarget(pos.addX(-1)))
            return 1;
        if (!BlockCode::target->isInTarget(pos.addX(1)))
            return 3;
        if (!BlockCode::target->isInTarget(pos.addY(-1)))
            return 2;
        if (!BlockCode::target->isInTarget(pos.addY(1)))
            return 0;
    }
    return 0;
}

bool SyncPlane::isSeed()
{
    Cell3DPosition posNext = catom->position.addZ(1);
    Cell3DPosition pos = catom->position;
    if (isOnBorder(pos) && isLowestOfBorder(pos) && BlockCode::target->isInTarget(posNext))
        return true;
    if (isOnBorder(posNext))
        return isLowestOfBorder(posNext);
    return false;
}


bool SyncPlane::isLowestOfBorder(Cell3DPosition pos) {
    int nTurns = 0;
    int idx = getIdxForBorder(pos);
    Cell3DPosition currentPos = pos;
    nTurns += getNextBorderNeighbor(idx, currentPos);

    while(currentPos != pos) {
        if (currentPos[1] < pos[1] ||
                (currentPos[1] == pos[1] && currentPos[0] < pos[0]))
            return false;
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

void SyncPlane::sendMessageCanConstructNextPlane()
{
    SyncPlane_message *msg = new SyncPlane_message(catom->position[2]);
    P2PNetworkInterface *interface = catom->getInterface(catom->position.addZ(-1));
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, interface));
}
