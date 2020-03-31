#include "syncPlane.h"
#include "catoms3DWorld.h"

#define DEBUG 0

SyncPlane_node *SyncPlane_node_manager::root = new SyncPlane_node(1, 0);

SyncPlane::SyncPlane(Catoms3D::Catoms3DBlock *c, Reconf *r) {
    this->catom = c;
    this->reconf = r;
}

SyncPlane::~SyncPlane() {
}

bool SyncPlane::isOnBorder(Cell3DPosition pos)
{
    if (BlockCode::target->isInTarget(pos) &&
        (!BlockCode::target->isInTarget(pos.offsetX(-1)) ||
        !BlockCode::target->isInTarget(pos.offsetX(1)) ||
        !BlockCode::target->isInTarget(pos.offsetY(-1)) ||
        !BlockCode::target->isInTarget(pos.offsetY(1))))
        return true;
    return false;
}

void debugPosition(Cell3DPosition pos) {
    cout << "debug position " << pos << " ";
    if (BlockCode::target->isInTarget(pos.offsetY(1)))
        cout << "hasNorth ";
    if (BlockCode::target->isInTarget(pos.offsetX(1)))
        cout << "hasRight ";
    if (BlockCode::target->isInTarget(pos.offsetY(-1)))
        cout << "hasLeft ";
    if (BlockCode::target->isInTarget(pos.offsetX(-1)))
        cout << "hasSouth ";
    cout << endl;
}

Cell3DPosition SyncPlane::getCurrentBorderForNextPlane()
{
    Cell3DPosition currentPos = catom->position;
    int idx = getIdxForBorder(currentPos);
    getNextBorderNeighbor(idx, currentPos);
    while (currentPos != catom->position) {
        Cell3DPosition nextPlanePos = currentPos.offsetZ(1);
        if (BlockCode::target->isInTarget(nextPlanePos) &&
                isOnBorder(nextPlanePos))
            return currentPos;
        getNextBorderNeighbor(idx, currentPos);
    }
    return currentPos;
}

bool SyncPlane::isSeed()
{
    // To avoid line cases
    if (couldBeSeed(catom->position.offsetX(-1)) || couldBeSeed(catom->position.offsetY(-1)))
        return false;

    if (isSeedBorderNextPlane(catom->position.offsetZ(1))) {
        return true;
    }

    if (isSeedBorder(catom->position)) {
        Cell3DPosition initialPos = getCurrentBorderForNextPlane().offsetZ(1);
        Cell3DPosition currentPos = initialPos;
        int idx = getIdxForBorder(currentPos);
        int nTurns = 0;
        nTurns += getNextBorderNeighbor(idx, currentPos);
        while (currentPos != initialPos) {
            if (isSeedBorderNextPlane(currentPos) && BlockCode::target->isInTarget(currentPos.offsetZ(-1))) {
                return false;
            }
            nTurns += getNextBorderNeighbor(idx, currentPos);
        }
        return true;
        //if (nTurns<= 0) return true;
    }


    return false;
}

bool SyncPlane::couldBeSeed(Cell3DPosition pos)
{
    if (BlockCode::target->isInTarget(pos) && BlockCode::target->isInTarget(pos.offsetZ(1)))
        if (isOnBorder(pos) || isOnBorder(pos.offsetZ(1)))
            return true;
    return false;
}

bool SyncPlane::isSeedBorder(Cell3DPosition pos)
{
    if (BlockCode::target->isInTarget(pos.offsetZ(1)) && isOnBorder(pos) && isLowestOfBorder(pos))
        return true;
    return false;
}

bool SyncPlane::isSeedBorderNextPlane(Cell3DPosition pos)
{
    if (BlockCode::target->isInTarget(pos) && isOnBorder(pos) && isLowestOfBorderNext(pos))
        return true;
    return false;
}

bool SyncPlane::isLowestOfBorderNext(Cell3DPosition pos) {
    int nTurns = 0;
    int idx = getIdxForBorder(pos);
    Cell3DPosition currentPos = pos;
    nTurns += getNextBorderNeighbor(idx, currentPos);
    while(currentPos != pos) {
        if ((currentPos[1] < pos[1] ||
                (currentPos[1] == pos[1] && currentPos[0] < pos[0])) && Catoms3D::getWorld()->getBlockByPosition(currentPos.offsetZ(-1)) != NULL)
        {
            return false;

        }
        nTurns += getNextBorderNeighbor(idx, currentPos);
    }
    if (nTurns > 0) return false;
    return true;
}

bool SyncPlane::isLowestOfBorder(Cell3DPosition pos) {
    int nTurns = 0;
    int idx = getIdxForBorder(pos);
    Cell3DPosition currentPos = pos;
    nTurns += getNextBorderNeighbor(idx, currentPos);
    while(currentPos != pos) {
        if ((currentPos[1] < pos[1] ||
                (currentPos[1] == pos[1] && currentPos[0] < pos[0])) && BlockCode::target->isInTarget(currentPos.offsetZ(1))) {
            return false;
        }
        nTurns += getNextBorderNeighbor(idx, currentPos);
    }
    if (nTurns <= 0) return true;
    return false;
}

int SyncPlane::getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos) {
    vector<pair<int, int>> ccw_order = {{0,-1}, {1,0}, {0,1}, {-1,0}};
    int newIdx;
    for (int i = 0; i < 4; i++) {
        newIdx = (((idx+i-1)%4)+4)%4;
        Cell3DPosition nextPos = currentPos.offsetX(ccw_order[newIdx].first)
                                          .offsetY(ccw_order[newIdx].second);
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

bool SyncPlane::hasAllNeighbors(Cell3DPosition pos)
{
    if (BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            BlockCode::target->isInTarget(pos.offsetY(1)) &&
            BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            BlockCode::target->isInTarget(pos.offsetX(1)))
        return true;
    return false;
}
void SyncPlane::sendMessageCanConstructNextPlane()
{
    SyncPlane_message *msg = new SyncPlane_message(catom->position[2]);
    P2PNetworkInterface *interface = catom->getInterface(catom->position.offsetZ(-1));
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 100, msg, interface));
}

int SyncPlane::getIdxForBorder(Cell3DPosition pos) {
    if (isOnBorder(pos)  && BlockCode::target->isInTarget(pos)) {
        if (BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            BlockCode::target->isInTarget(pos.offsetY(1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            BlockCode::target->isInTarget(pos.offsetX(1)))
            return 0;
        if (BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetY(1)) &&
            BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            BlockCode::target->isInTarget(pos.offsetX(1)))
            return 3;
        if (BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            BlockCode::target->isInTarget(pos.offsetY(1)) &&
            BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(1)))
            return 2;
        if (!BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            BlockCode::target->isInTarget(pos.offsetY(1)) &&
            BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            BlockCode::target->isInTarget(pos.offsetX(1)))
            return 1;
        // 2 empty neighbors
        if (BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetY(1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            BlockCode::target->isInTarget(pos.offsetX(1)))
            return 3;
        if (!BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            BlockCode::target->isInTarget(pos.offsetY(1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            BlockCode::target->isInTarget(pos.offsetX(1)))
            return 0;
        if (!BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            BlockCode::target->isInTarget(pos.offsetY(1)) &&
            BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(1)))
            return 1;
        if (BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetY(1)) &&
            BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(1)))
            return 2;
        if (!BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetY(1)) &&
            BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            BlockCode::target->isInTarget(pos.offsetX(1)))
            return 0;
        // critical case?
        if (BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            BlockCode::target->isInTarget(pos.offsetY(1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(1)))
            return 2;
        if (!BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            BlockCode::target->isInTarget(pos.offsetY(1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            BlockCode::target->isInTarget(pos.offsetX(1)))
            return 1;
        // 3 empty neighbors
        if (!BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            BlockCode::target->isInTarget(pos.offsetY(1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(1)))
            return 0;
        if (!BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetY(1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            BlockCode::target->isInTarget(pos.offsetX(1)))
            return 3;
        if (BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetY(1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(1)))
            return 2;
        if (!BlockCode::target->isInTarget(pos.offsetY(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetY(1)) &&
            BlockCode::target->isInTarget(pos.offsetX(-1)) &&
            !BlockCode::target->isInTarget(pos.offsetX(1)))
            return 1;
    }
    return 0;
}
