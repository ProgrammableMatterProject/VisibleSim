#include "coatingSeeding.hpp"

#include "world.h"

Seeding::Seeding(std::function<bool(const Cell3DPosition&)> _isInG,
                 Neighborhood *_neighborhood, Border *_border)
    : isInG(_isInG), nbh(_neighborhood), border(_border) {
    lattice = BaseSimulator::getWorld()->lattice;
}

bool Seeding::isNorthSeed(const Cell3DPosition& pos) const {
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

bool Seeding::isSouthSeed(const Cell3DPosition& pos) const {
    return isInG(pos) and
        not isSouthLineOnMerge(pos) and isInG(pos.addY(-1))
        and (not isInG(pos.addX(-1).addY(-1)) or not isInG(pos.addX(-1)));
}

bool Seeding::isNorthLineOnMerge(const Cell3DPosition& pos) const {
    if (not isInG(pos) or nbh->hasNeighborInDirection(pos, SkewFCCLattice::Direction::C1North))
        return false;

    Cell3DPosition xPos = pos.addX(1);
    Cell3DPosition xyPos = pos + Cell3DPosition(1, 1, 0);
    if (!isInG(xPos) and isInG(xyPos)) {

        int upperXBound = lattice->getGridUpperBounds(pos[2])[0];

        // Thadeu-style weird af loop
        for (int i = 2; pos[0] + i < upperXBound; i++) {
            xPos = pos.addX(i);
            xyPos = pos + Cell3DPosition(i, 1, 0);

            if (!isInG(xPos) and isInG(xyPos))
                continue;

            if (isInG(xPos) and isInG(xyPos))
                return border->isOnInternalHole(pos);

            return false;
        }
    }

    return false;
}

bool Seeding::isSouthLineOnMerge(const Cell3DPosition& pos) const {
    if (not isInG(pos) or nbh->hasNeighborInDirection(pos, SkewFCCLattice::Direction::C7South))
        return false;

    Cell3DPosition xPos = pos.addX(-1);
    Cell3DPosition xyPos = pos + Cell3DPosition(-1, -1, 0);
    if (!isInG(xPos) and isInG(xyPos)) {

        int lowerXBound = lattice->getGridLowerBounds(pos[2])[0];

        // Thadeu-style weird af loop
        for (int i = 2; pos[0] - i > lowerXBound; i++) {
            xPos = pos.addX(-i);
            xyPos = pos + Cell3DPosition(-i, -1, 0);

            if (!isInG(xPos) and isInG(xyPos))
                continue;

            if (isInG(xPos) and isInG(xyPos))
                return border->isOnInternalHole(pos);

            return false;
        }
    }

    return false;
}

bool Seeding::isPlaneSeed(const Cell3DPosition& pos) const {
    return not (couldBeSeed(nbh->cellInDirection(pos, East))
                or couldBeSeed(nbh->cellInDirection(pos, South)))
        and (isSeedBorderOnNextPlane(pos.addZ(1)) or isSeedBorderOnCurrentPlane(pos));
}

bool Seeding::couldBeSeed(const Cell3DPosition& pos) const {
    const Cell3DPosition& zPos = pos + Cell3DPosition(0,0,1);
    if (isInG(pos) and isInG(zPos))
        if (border->isOnBorder(pos) or border->isOnBorder(zPos))
            return true;
    return false;
}

bool Seeding::isSeedBorderOnCurrentPlane(const Cell3DPosition& pos) const {
    return isInG(pos.addZ(1)) and border->isOnBorder(pos) and isLowestOfBorderOnPlane(pos);
}

bool Seeding::isSeedBorderOnNextPlane(const Cell3DPosition& pos) const {
    return isInG(pos) and border->isOnBorder(pos) and isLowestOfBorderOnPlane(pos, true);
}

bool Seeding::isLowestOfBorderOnPlane(const Cell3DPosition& pos, bool next) const {
    int nTurns = 0;
    int idx = border->getIndexForBorder(pos);
    Cell3DPosition currentPos = pos;

    nTurns += border->getNextBorderNeighbor(idx, currentPos);

    while(currentPos != pos) {
        Cell3DPosition planePos = currentPos +
            (next ? Cell3DPosition(0,0,-1) : Cell3DPosition(0,0,1));

        if ((currentPos[1] < pos[1] or (currentPos[1] == pos[1] and currentPos[0] > pos[0]))
            and isInG(planePos)) {
            return false;
        }

        nTurns += border->getNextBorderNeighbor(idx, currentPos);
    }

    return nTurns <= 0;
}
