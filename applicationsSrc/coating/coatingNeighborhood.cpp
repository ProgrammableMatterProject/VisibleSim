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

bool Neighborhood::isOnInternalHole(const Cell3DPosition& pos, PlanarDir d) const {
    // From Thadeu's Sync/sync.cpp
    int nTurns = 0;
    int idx;

    if (directionIsInCSG(pos, North) and directionIsInCSG(pos, South)
        and directionIsInCSG(pos, East) and directionIsInCSG(pos, West))
        return false;


    if (d == West) {
        if (not directionIsInCSG(pos, West) or not directionIsInCSG(pos, SouthWest))
            return false;

        idx = 0;
    } else if (d == East) {
        if (not directionIsInCSG(pos, East) or not directionIsInCSG(pos, NorthEast))
            return false;

        idx = 2;
    } else {
        stringstream err;
        err << "isOnInternalHole(" << pos << ", " << planarDirectionIndexToString(d) << endl;
        throw NotImplementedException(err.str());
    }

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
