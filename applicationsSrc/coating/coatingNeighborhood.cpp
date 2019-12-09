#include "coatingNeighborhood.hpp"

#include "world.h"
#include <unistd.h>

Neighborhood::Neighborhood(std::function<bool(const Cell3DPosition&)> _isInG)
    : isInG(_isInG) {

    lattice = BaseSimulator::getWorld()->lattice;
}

Cell3DPosition Neighborhood::cellInDirection(const Cell3DPosition& ref,
                                             PlanarDir d) const {
    return ref + planarPos[d];
}

bool Neighborhood::directionIsInCSG(const Cell3DPosition& ref, PlanarDir d) const {
    const Cell3DPosition& pos = ref + planarPos[d];
    return isInG(pos);
}

bool Neighborhood::isNorthSeed(const Cell3DPosition& pos) const {
    // cout << "isInG(" << pos << "): " << isInG(pos) << endl;
    // cout << "isNorthLineOnMerge(" << pos << "): " << isNorthLineOnMerge(pos) << endl;
    // cout << "isInG(" << pos.addY(1) << "): " << isInG(pos.addY(1)) << endl;
    // cout << "isInG(" << pos.addX(1).addY(1) << "): "
    //      << isInG(pos.addX(1).addY(1)) << endl;
    // cout << "isInG(" << pos.addX(1) << "): " << isInG(pos.addX(1)) << endl;

    return isInG(pos) and
        not isNorthLineOnMerge(pos) and isInG(pos.addY(1))
        and (not isInG(pos.addX(1).addY(1)) or not isInG(pos.addX(1)));
}

bool Neighborhood::isSouthSeed(const Cell3DPosition& pos) const {
    return isInG(pos) and
        not isSouthLineOnMerge(pos) and isInG(pos.addY(-1))
        and (not isInG(pos.addX(-1).addY(-1)) or not isInG(pos.addX(-1)));
}

bool Neighborhood::isNorthLineOnMerge(const Cell3DPosition& pos) const {
    if (not isInG(pos)) return false;

    // Find n, the distance to the next module along the current line
    unsigned int n = 0;
    int upperXBound = lattice->getGridUpperBounds(pos[2])[0];
    Cell3DPosition nPos;
    for (int x = pos[0] + 1; x <= upperXBound; x++) {
        nPos.set(x, pos[1], pos[2]);

        if (isInG(nPos)) {
            n = nPos[0] - pos[0] - 1;
            break;
        }
    }

    if (n == 0
        or not isInG(pos.addX(n + 1))
        or not isInG(pos.addX(n + 1).addY(1))) return false;

    for (unsigned int j = 1; j <= n; j++) {
        nPos.set(pos[0] + j, pos[1] + 1, pos[2]);

        if (not isInG(nPos)) return false;
    }

    return true;
}

bool Neighborhood::isSouthLineOnMerge(const Cell3DPosition& pos) const {
    if (not isInG(pos)) return false;

    // Find n, the distance to the next module preceding this one on the current line
    unsigned int n = 0;
    int lowerXBound = lattice->getGridUpperBounds(pos[2])[0];
    Cell3DPosition nPos;
    for (int x = pos[0] - 1; x >= lowerXBound; x--) {
        nPos.set(x, pos[1], pos[2]);

        if (isInG(nPos)) {
            n = pos[0] - nPos[0] + 1;
            break;
        }
    }

    if (n == 0
        or not isInG(pos.addX(n - 1))
        or not isInG(pos.addX(n - 1).addY(-1))) return false;

    for (unsigned int j = 1; j <= n; j++) {
        nPos.set(pos[0] - j, pos[1] + 1, pos[2]);

        if (not isInG(nPos)) return false;
    }

    return true;
}

int Neighborhood::getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos) const {
    int newIdx;
    vector<pair<int, int>> ccw_order = {{0,-1}, {1,0}, {0,1}, {-1,0}};
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

// CCW
// int Border::getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos) {
//     vector<pair<int, int>> ccw_order = {{0,-1}, {1,0}, {0,1}, {-1,0}};
//     int newIdx;
//     for (int i = 0; i < 4; i++) {
//         newIdx = (((idx+i-1)%4)+4)%4;
//         Cell3DPosition nextPos = currentPos.addX(ccw_order[newIdx].first)
//                                           .addY(ccw_order[newIdx].second);
//         if (BlockCode::target->isInTarget(nextPos)) {
//             idx = newIdx;
//             currentPos = nextPos;
//             if (i == 0)
//                 return 1;
//             else if (i == 2)
//                 return -1;
//             else if (i == 3)
//                 return -2;
//             return 0;
//         }
//     }
//     return 0;
// }

bool Neighborhood::isOnInternalHole(const Cell3DPosition& pos) const {
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

bool Neighborhood::isOnBorder(const Cell3DPosition& pos) const {
    if (isInG(pos) and
        (!directionIsInCSG(pos, West) or
         !directionIsInCSG(pos, East) or
         !directionIsInCSG(pos, North) or
         !directionIsInCSG(pos, South)))
        return true;

    return false;
}

int Neighborhood::getIndexForBorder(const Cell3DPosition& pos) const {
    if (isOnBorder(pos)) {
        bool hasNorth = directionIsInCSG(pos, North);
        bool hasSouth = directionIsInCSG(pos, South);
        bool hasWest = directionIsInCSG(pos, West);
        bool hasEast = directionIsInCSG(pos, East);
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
