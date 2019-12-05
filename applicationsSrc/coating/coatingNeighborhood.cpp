#include "coatingNeighborhood.hpp"

#include "world.h"

Neighborhood::Neighborhood(std::function<bool(const Cell3DPosition&)> _isInG)
    : isInG(_isInG) {

    lattice = BaseSimulator::getWorld()->lattice;
}

Cell3DPosition Neighborhood::cellInDirection(const Cell3DPosition& ref,
                                             CCWDir d) const {
    return ref + CCWDPos[d];
}

bool Neighborhood::directionIsInCSG(const Cell3DPosition& ref, CCWDir d) const {
    const Cell3DPosition& pos = ref + CCWDPos[d];
    return isInG(pos);
}

bool Neighborhood::isNorthSeed(const Cell3DPosition& pos) const {
    cout << "isInG(" << pos << "): " << isInG(pos) << endl;
    cout << "isNorthLineOnMerge(" << pos << "): " << isNorthLineOnMerge(pos) << endl;
    cout << "isInG(" << pos.addY(1) << "): " << isInG(pos.addY(1)) << endl;
    cout << "isInG(" << pos.addX(1).addY(1) << "): "
         << isInG(pos.addX(1).addY(1)) << endl;
    cout << "isInG(" << pos.addX(1) << "): " << isInG(pos.addX(1)) << endl;

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

bool Neighborhood::isOnInternalHole(const Cell3DPosition& pos) const {
    return false; // TODO
}
