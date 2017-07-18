#include "sync.h"

Sync::Sync(Catoms3D::Catoms3DBlock *c, Reconf *r) {
    this->catom = c;
    this->reconf = r;
}

Sync::~Sync() {}

// idx is the direction the border comes for the first catom
bool Sync::isInternalBorder(int idx) {
    int nTurns = 0;
    Cell3DPosition currentPos = catom->position;
    nTurns += getNextBorderNeighbor(idx, currentPos);

    while(currentPos != catom->position)
        nTurns += getNextBorderNeighbor(idx, currentPos);
    if (nTurns > 0) return true;
    return false;
}

int Sync::getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos) {
    int newIdx;
    for (int i = 0; i < 4; i++) {
        newIdx = (((idx+i-1)%4)+4)%4;
        Cell3DPosition nextPos = currentPos.addX(cw_order[newIdx].first)
                                          .addY(cw_order[newIdx].second);
        if (BlockCode::target->isInTarget(nextPos)) {
            idx = newIdx;
            currentPos = nextPos;
            if (i == 0)
                return 1;
            else if (i == 2)
                return -1;
            else if (i == 3)
                return -2;
            else
                return 0;
        }
    }
    return 0;
}

