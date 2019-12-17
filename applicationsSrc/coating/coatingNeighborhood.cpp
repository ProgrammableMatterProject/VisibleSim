#include "coatingNeighborhood.hpp"

#include "world.h"

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

bool Neighborhood::hasNeighborInDirection(const Cell3DPosition& pos,
                                          SkewFCCLattice::Direction dir) const {
    return lattice->cellHasBlock(lattice->getCellInDirection(pos, dir));
}

bool Neighborhood::hasNoHorizontalNeighbor(const Cell3DPosition& pos) const {
    return (not directionIsInCSG(pos, North)
            or not hasNeighborInDirection(pos, SkewFCCLattice::Direction::C1North))
        and (not directionIsInCSG(pos, East)
             or not hasNeighborInDirection(pos, SkewFCCLattice::Direction::C0East))
        and (not directionIsInCSG(pos, South)
             or not hasNeighborInDirection(pos, SkewFCCLattice::Direction::C7South))
        and (not directionIsInCSG(pos, West)
             or not hasNeighborInDirection(pos, SkewFCCLattice::Direction::C6West));
}
