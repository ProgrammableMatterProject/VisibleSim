#include "coatingBorder.hpp"

#include "world.h"

Border::Border(std::function<bool(const Cell3DPosition&)> _isInG,
               Neighborhood *_neighborhood) : isInG(_isInG), nbh(_neighborhood) {

    lattice = BaseSimulator::getWorld()->lattice;
}

int Border::getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos) const {
    int newIdx;
    vector<pair<int, int>> cw_order = {{0,-1}, {-1,0}, {0,1}, {1,0}};

    for (int i = 0; i < 4; i++) {
        newIdx = (((idx+i-1)%4)+4)%4;

        Cell3DPosition nextPos = currentPos
            .addX(cw_order[newIdx].first)
            .addY(cw_order[newIdx].second);

        if (isInG(nextPos)) {
            idx = newIdx;
            currentPos = nextPos;
            switch (i) {
                case 0: return 1;
                case 2: return -1;
                case 3: return -2;
                default: return 0;
            }
        }
    }

    return 0;
}

int Border::getNextBorderNeighborCCW(int &idx, Cell3DPosition &currentPos) const {
    vector<pair<int, int>> ccw_order = {{0,-1}, {1,0}, {0,1}, {-1,0}};
    int newIdx;
    for (int i = 0; i < 4; i++) {
        newIdx = (((idx+i-1)%4)+4)%4;
        Cell3DPosition nextPos = currentPos.addX(ccw_order[newIdx].first)
                                          .addY(ccw_order[newIdx].second);
        if (isInG(nextPos)) {
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

bool Border::isOnInternalHole(const Cell3DPosition& pos) const {
    // From Thadeu's Sync/sync.cpp
    int nTurns = 0;
    int idx = getIndexForBorder(pos);
    if (idx == -1) return false;

    if (not isOnBorder(pos))
        return false;

    // lattice->highlightCell(pos, BLACK);

    Cell3DPosition currentPos = pos;
    nTurns += getNextBorderNeighbor(idx, currentPos);

    // lattice->highlightCell(currentPos,YELLOW);

    while(currentPos != pos) {
        // lattice->unhighlightCell(currentPos);
        nTurns += getNextBorderNeighbor(idx, currentPos);
        // lattice->highlightCell(currentPos,YELLOW);
        // usleep(20000);
    }

    if (nTurns > 0) return true;

    return nTurns > 0;
}

bool Border::isOnBorder(const Cell3DPosition& pos) const {
    if (isInG(pos) and
        (!nbh->directionIsInCSG(pos, West) or
         !nbh->directionIsInCSG(pos, East) or
         !nbh->directionIsInCSG(pos, North) or
         !nbh->directionIsInCSG(pos, South)))
        return true;

    return false;
}

int Border::getIndexForBorder(const Cell3DPosition& pos) const {
    if (isOnBorder(pos)) {
        bool hasNorth = nbh->directionIsInCSG(pos, North);
        bool hasSouth = nbh->directionIsInCSG(pos, South);
        bool hasWest = nbh->directionIsInCSG(pos, West);
        bool hasEast = nbh->directionIsInCSG(pos, East);
        int nNeighbor = hasNorth + hasSouth + hasWest + hasEast;

        if (nNeighbor == 1) {
            if (hasNorth)
                return 0;
            else if ( hasEast)
                return 3;
            else if (hasSouth)
                return 2;
            else if (hasWest)
                return 1;
        } else if (nNeighbor == 2) {
            if (hasSouth) {
                if (hasWest)
                    return 2;
                else if (hasEast)
                    return 3;
                else if (hasNorth)
                    return 2;
            } else if (hasNorth) {
                if (hasEast)
                    return 0;
                else if (hasWest)
                    return 1;
            } else if (hasWest and hasEast) {
                return 0;
            }
        } else if (nNeighbor == 3) {
            if (hasSouth) {
                if (hasNorth) {
                    if (hasEast)
                        return 0;
                    else return 2; // has West
                } else if (hasWest and hasEast) {
                    return 3;
                }
            } else if (hasNorth and hasWest and hasEast) {
                return 1;
            }
        }
    }

    return -1;
}
