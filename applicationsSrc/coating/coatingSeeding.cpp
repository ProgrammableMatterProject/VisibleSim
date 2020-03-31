#include "coatingSeeding.hpp"

#include "world.h"

#include <unistd.h>

Seeding::Seeding(std::function<bool(const Cell3DPosition&)> _isInG,
                 Neighborhood *_neighborhood, Border *_border)
    : isInG(_isInG), nbh(_neighborhood), border(_border) {
    lattice = BaseSimulator::getWorld()->lattice;
}

bool Seeding::isNorthSeed(const Cell3DPosition& pos) const {
    // cout << "isInG(" << pos << "): " << isInG(pos) << endl;
    // cout << "isNorthLineOnMerge(" << pos << "): " << isNorthLineOnMerge(pos) << endl;
    // cout << "isInG(" << pos.addY(1) << "): " << isInG(pos.addY(1)) << endl;
    // cout << "isInG(" << pos.offsetX(1).addY(1) << "): "
    //      << isInG(pos.offsetX(1).addY(1)) << endl;
    // cout << "isInG(" << pos.offsetX(1) << "): " << isInG(pos.offsetX(1)) << endl;

    return isInG(pos) and
        not isSouthLineOnMerge(pos) and isInG(pos.offsetY(1))
        and (not isInG(pos.offsetX(1).offsetY(1)) or not isInG(pos.offsetX(1)));
}

bool Seeding::isSouthSeed(const Cell3DPosition& pos) const {
    return isInG(pos) and
        not isNorthLineOnMerge(pos) and isInG(pos.offsetY(-1))
        and (not isInG(pos.offsetX(-1).offsetY(-1)) or not isInG(pos.offsetX(-1)));
}

bool Seeding::isNorthLineOnMerge(const Cell3DPosition& pos) const {
    if (not isInG(pos) or nbh->hasNeighborInDirection(pos, SkewFCCLattice::Direction::C1North))
        return false;

    Cell3DPosition xPos = pos.offsetX(1);
    Cell3DPosition xyPos = pos + Cell3DPosition(1, 1, 0);
    if (!isInG(xPos) and isInG(xyPos)) {

        int upperXBound = lattice->getGridUpperBounds(pos[2])[0];

        // Thadeu-style weird af loop
        for (int i = 2; pos[0] + i < upperXBound; i++) {
            xPos = pos.offsetX(i);
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

    Cell3DPosition xPos = pos.offsetX(-1);
    Cell3DPosition xyPos = pos + Cell3DPosition(-1, -1, 0);
    if (!isInG(xPos) and isInG(xyPos)) {

        int lowerXBound = lattice->getGridLowerBounds(pos[2])[0];

        // Thadeu-style weird af loop
        for (int i = 2; pos[0] - i > lowerXBound; i++) {
            xPos = pos.offsetX(-i);
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

bool Seeding::isPlaneSeed(const Cell3DPosition& pos,
                          SeedDirection &seedDir) const {

    if (not isInG(pos)) return false;

    // return not (couldBeSeed(nbh->cellInDirection(pos, East))
    //             or couldBeSeed(nbh->cellInDirection(pos, South)))
    //     and ( (isSeedBorderOnNextPlane(pos, seedDir))
    //           // and isSeedBorderOnCurrentPlane(pos)
    //           // or isLowestOfBorderOnCurrentPlane(pos)
    //     );

    return isSeedBorderOnNextPlane(pos, seedDir);
}

bool Seeding::couldBeSeed(const Cell3DPosition& pos) const {
    const Cell3DPosition& zPos = pos + seedDirPositions[Zward];
    const Cell3DPosition& zPosAlt = pos + seedDirPositions[RevZward];
    return isInG(pos) and (isInG(zPos) or isInG(zPosAlt)) and
        (border->isOnBorder(pos) or border->isOnBorder(zPos)
         or border->isOnBorder(zPosAlt));
}

bool Seeding::isSeedBorderOnCurrentPlane(const Cell3DPosition& pos) const {
    return isInG(pos + seedDirPositions[Zward]) and border->isOnBorder(pos)
        and isLowestOfBorderOnCurrentPlane(pos);
}

bool Seeding::isSeedBorderOnNextPlane(const Cell3DPosition& pos,
                                      SeedDirection &seedDir) const {
    // Try all the upwards directions from pos successively, after ensuring
    //  that there are no better candidate within the neighborhood of the upward neighbor

    for (int dir = 0; dir <= RZward; dir++) {
        const Cell3DPosition& firstPos = pos + seedDirPositions[dir];

        if (isInG(firstPos) and border->isOnBorder(firstPos)
            and not isNotLowestOfBorder.count(firstPos)
            and isLowestOfBorderOnNextPlane(firstPos)) {

            // cout << pos << " - " << dir << ": true";
            if (firstPositionOfPlaneHasNoBetterCandidate(firstPos, (SeedDirection)dir)) {
                // NOTE: Might as well return false if firstPositionOfPlaneHasNo... = false

                seedDir = (SeedDirection)dir;
                return true;
            } else return false;
        }

        // cout << pos << " - " << dir << ": false" << endl;
    }

    return false;
}

bool Seeding::isLowestOfBorderOnCurrentPlane(const Cell3DPosition& pos) const {
    int nTurns = 0;
    int idx = border->getIndexForBorder(pos);
    Cell3DPosition currentPos = pos;
    nTurns += border->getNextBorderNeighborCCW(idx, currentPos);
    while(currentPos != pos) {
        if ((currentPos[1] < pos[1] or (currentPos[1] == pos[1] and currentPos[0] > pos[0]))
            // and (isInG(currentPos + forwardSeed))) {
            and (isInG(currentPos + seedDirPositions[RevZward]))) {
            isNotLowestOfBorder.insert(pos);
            return false;
        }

        nTurns += border->getNextBorderNeighborCCW(idx, currentPos);
    }

    return nTurns <= 0;
}

bool Seeding::isLowestOfBorderOnNextPlane(const Cell3DPosition& pos) const {
    int nTurns = 0;
    int idx = border->getIndexForBorder(pos);
    Cell3DPosition currentPos = pos;
    nTurns += border->getNextBorderNeighborCCW(idx, currentPos);
    while(currentPos != pos) {
        // cout << currentPos << endl;

        if ((currentPos[1] < pos[1] or (currentPos[1] == pos[1] and currentPos[0] > pos[0]))
            and hasLowerNeighborInCoating(currentPos)) {

            // and (isInG(currentPos - backwardSeed)//  or isInG(currentPos - forwardSeed)
            //      or isInG(currentPos - rightwardSeed) or isInG(currentPos - leftwardSeed)
            //     )
            // and not lattice->cellsAreAdjacent(currentPos, candidate)) {

            return false;
        }

        nTurns += border->getNextBorderNeighborCCW(idx, currentPos);
    }

    return nTurns <= 0;
}

bool Seeding::findLowestOfBorderFrom(const Cell3DPosition& pos,
                                     Cell3DPosition &lowest) const {
    int nTurns = 0;
    int idx = border->getIndexForBorder(pos);

    lowest = pos;

    // static int tsms = 500;

    // lattice->highlightCell(pos, BLUE);
    // usleep(tsms * 1000);

    Cell3DPosition currentPos = pos;
    nTurns += border->getNextBorderNeighborCCW(idx, currentPos);
    while(currentPos != pos) {
        if (currentPos[1] < lowest[1]
            or (currentPos[1] == lowest[1] and currentPos[0] > lowest[0])) {
            // if (pos != lowest) lattice->unhighlightCell(lowest);
            lowest = currentPos;

            // lattice->highlightCell(lowest);
            // usleep(tsms * 1000);
        }

        nTurns += border->getNextBorderNeighborCCW(idx, currentPos);
    }

    // lattice->highlightCell(lowest, (nTurns <= 0 ? GREEN : RED));
    // usleep(tsms * 1000);
    // lattice->unhighlightCell(lowest);

    return nTurns <= 0;
}

bool Seeding::hasLowerNeighborInCoating(const Cell3DPosition& pos) const {
    for (const Cell3DPosition& np : lattice->getNeighborhood(pos)) {
        if (np[2] < pos[2] and isInG(np)) return true;
    }

    return false;
}

bool Seeding::firstPositionOfPlaneHasNoBetterCandidate(const Cell3DPosition& firstPos,
                                                       SeedDirection dir) const {
    // All previous (before dir) seed candidates must be free for direction dir
    //  to be best candidate
    for (int d = 0; d < dir; d++) {
        if (isInG(firstPos - seedDirPositions[d])) {
            // cout << " better candidate: " << firstPos - seedDirPositions[d] << endl;

            return false;
        }
    }

    // cout << " best: " << endl;

    return true;
}

void Seeding::print(const Cell3DPosition& pos) const {
    cout << "not (couldBeSeed(nbh->cellInDirection(pos, East)):"
         << couldBeSeed(nbh->cellInDirection(pos, East)) << endl;
    cout << "or couldBeSeed(nbh->cellInDirection(pos, South))):"
         << couldBeSeed(nbh->cellInDirection(pos, South)) << endl;
    SeedDirection seedDir;
    cout << "and (isSeedBorderOnNextPlane(pos):"
         << isSeedBorderOnNextPlane(pos, seedDir) << " - sd: " << seedDir << endl;
    // cout << "or isSeedBorderOnNextPlane(pos + forwardSeed):"
    //      << isSeedBorderOnNextPlane(pos + forwardSeed, pos) << endl;
    // cout << "or isSeedBorderOnNextPlane(pos + rightwardSeed):"
    //      << isSeedBorderOnNextPlane(pos + rightwardSeed, pos) << endl;
    // cout << "or isSeedBorderOnNextPlane(pos + leftwardSeed):"
    //      << isSeedBorderOnNextPlane(pos + leftwardSeed, pos) << endl;

    //     cout << "isLowestOfBorderOnCurrentPlane(pos):"
    //          << isLowestOfBorderOnCurrentPlane(pos) << endl;

    //     cout << "isSeedBorderOnCurrentPlane(pos):"
    //          << isSeedBorderOnCurrentPlane(pos) << endl;
}
