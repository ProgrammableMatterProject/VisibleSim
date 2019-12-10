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

bool Neighborhood::hasNeighborInDirection(const Cell3DPosition& pos,
                                          SkewFCCLattice::Direction dir) const {
    return lattice->cellHasBlock(lattice->getCellInDirection(pos, dir));
}
