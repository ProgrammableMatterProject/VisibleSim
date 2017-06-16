#include "sync.h"

Sync::Sync(Catoms3D::Catoms3DBlock *c, Reconf *r) {
    this->catom = c;
    this->reconf = r;
}

Sync::~Sync() {}

// idx is the direction the border comes for the first catom
bool Sync::isInternalBorder(int idx) {
    int oldIdx = idx;
    int nTurns = 0;
    Cell3DPosition currentPos = catom->position;

    for (int i = 0; i < 4; i++) {
        int idx = (((oldIdx+i-1)%4)+4)%4;
        Cell3DPosition nextPos = currentPos.addX(cw_order[idx].first)
                                          .addY(cw_order[idx].second);
        if (BlockCode::target->isInTarget(nextPos)) {
            if (i == 0)
                nTurns++;
            else if (i == 2)
                nTurns--;
            else if (i == 3)
                nTurns -= 2;
            oldIdx = idx;
            currentPos = nextPos;
            break;
        }
    }

    while(currentPos != catom->position) {
        for (int i = 0; i < 4; i++) {
            int idx = (((oldIdx+i-1)%4)+4)%4;
            Cell3DPosition nextPos = currentPos.addX(cw_order[idx].first)
                                              .addY(cw_order[idx].second);
            if (BlockCode::target->isInTarget(nextPos)) {
                if (i == 0) {
                    nTurns++;
                }
                else if (i == 2)
                    nTurns--;
                else if (i == 3)
                    nTurns -= 2;
                oldIdx = idx;
                currentPos = nextPos;
                break;
            }
        }
    }
    if (nTurns > 0) return true;
    return false;
}

