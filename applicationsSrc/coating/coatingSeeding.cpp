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
        not isSouthLineOnMerge(pos) and isInG(pos.addY(1))
        and (not isInG(pos.addX(1).addY(1)) or not isInG(pos.addX(1)));
}

bool Seeding::isSouthSeed(const Cell3DPosition& pos) const {
    return isInG(pos) and
        not isNorthLineOnMerge(pos) and isInG(pos.addY(-1))
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
        and ( (isSeedBorderOnNextPlane(pos + backwardSeed, pos)
               or isSeedBorderOnNextPlane(pos + forwardSeed, pos)
               or isSeedBorderOnNextPlane(pos + rightwardSeed, pos)
               or isSeedBorderOnNextPlane(pos + leftwardSeed, pos)
            )
        // and isSeedBorderOnCurrentPlane(pos)
        and isLowestOfBorderOnCurrentPlane(pos)
        );
}

bool Seeding::couldBeSeed(const Cell3DPosition& pos) const {
    const Cell3DPosition& zPos = pos + forwardSeed;
    const Cell3DPosition& zPosAlt = pos + backwardSeed;
    return isInG(pos) and (isInG(zPos) or isInG(zPosAlt)) and
        (border->isOnBorder(pos) or border->isOnBorder(zPos) or border->isOnBorder(zPosAlt));
}

bool Seeding::isSeedBorderOnCurrentPlane(const Cell3DPosition& pos) const {
    return isInG(pos + forwardSeed) and border->isOnBorder(pos)
        and isLowestOfBorderOnCurrentPlane(pos);
}

bool Seeding::isSeedBorderOnNextPlane(const Cell3DPosition& pos,
                                      const Cell3DPosition& candidate) const {

    if (candidate == Cell3DPosition(21, 5, 3)) {
        cout << "candidate: " << candidate << endl;
        cout << "pos: " << pos << endl;
        cout << "isInG(pos): " << isInG(pos) << endl;
        cout << "border->isOnBorder(pos): " << border->isOnBorder(pos) << endl;
        cout << "isLowestOfBorderOnNextPlane(pos): "
             << isLowestOfBorderOnNextPlane(pos, candidate) << endl;
    }

    return isInG(pos) and border->isOnBorder(pos)
        and isLowestOfBorderOnNextPlane(pos, candidate);
}

bool Seeding::isLowestOfBorderOnCurrentPlane(const Cell3DPosition& pos) const {
    int nTurns = 0;
    int idx = border->getIndexForBorder(pos);
    Cell3DPosition currentPos = pos;
    nTurns += border->getNextBorderNeighborCCW(idx, currentPos);
    while(currentPos != pos) {
        if ((currentPos[1] < pos[1] or (currentPos[1] == pos[1] and currentPos[0] > pos[0]))
            and (isInG(currentPos + forwardSeed))) {
            return false;
        }

        nTurns += border->getNextBorderNeighborCCW(idx, currentPos);
    }

    return nTurns <= 0;
}

bool Seeding::isLowestOfBorderOnNextPlane(const Cell3DPosition& pos,
                                          const Cell3DPosition& candidate) const {
    int nTurns = 0;
    int idx = border->getIndexForBorder(pos);
    Cell3DPosition currentPos = pos;
    nTurns += border->getNextBorderNeighborCCW(idx, currentPos);
    while(currentPos != pos) {
        cout << currentPos << endl;

        if ((currentPos[1] < pos[1] or (currentPos[1] == pos[1] and currentPos[0] > pos[0]))
            and (isInG(currentPos - backwardSeed) or isInG(currentPos - forwardSeed)
                 or isInG(currentPos - rightwardSeed) or isInG(currentPos - leftwardSeed))
            and not lattice->cellsAreAdjacent(currentPos, candidate)) {

            return false;
        }

        nTurns += border->getNextBorderNeighborCCW(idx, currentPos);
    }
    return nTurns <= 0;
}

void Seeding::print(const Cell3DPosition& pos) const {
    cout << "not (couldBeSeed(nbh->cellInDirection(pos, East)):"
         << couldBeSeed(nbh->cellInDirection(pos, East)) << endl;
    cout << "or couldBeSeed(nbh->cellInDirection(pos, South))):"
         << couldBeSeed(nbh->cellInDirection(pos, South)) << endl;
    cout << "and (isSeedBorderOnNextPlane(pos + backwardSeed):"
         << (isSeedBorderOnNextPlane(pos + backwardSeed, pos)) << endl;
    cout << "or isSeedBorderOnNextPlane(pos + forwardSeed):"
         << isSeedBorderOnNextPlane(pos + forwardSeed, pos) << endl;
    cout << "or isSeedBorderOnNextPlane(pos + rightwardSeed):"
         << isSeedBorderOnNextPlane(pos + rightwardSeed, pos) << endl;
    cout << "or isSeedBorderOnNextPlane(pos + leftwardSeed):"
         << isSeedBorderOnNextPlane(pos + leftwardSeed, pos) << endl;

    cout << "isLowestOfBorderOnCurrentPlane(pos):"
         << isLowestOfBorderOnCurrentPlane(pos) << endl;

    cout << "isSeedBorderOnCurrentPlane(pos):"
         << isSeedBorderOnCurrentPlane(pos) << endl;
}
