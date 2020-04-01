#include "coatingNeighborhood.hpp"

#include "base/world.h"

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

pair<short, short> Neighborhood::getOrthogonalDirections(short d) {
    switch (SkewFCCLattice::Direction(d)) {

        case SkewFCCLattice::Direction::C0East:
            return make_pair(SkewFCCLattice::Direction::C1North,
                             SkewFCCLattice::Direction::C7South);
            break;

        case SkewFCCLattice::Direction::C1North:
            return make_pair(SkewFCCLattice::Direction::C6West,
                             SkewFCCLattice::Direction::C0East);
            break;

        case SkewFCCLattice::Direction::C6West:
            return make_pair(SkewFCCLattice::Direction::C1North,
                             SkewFCCLattice::Direction::C7South);
            break;

        case SkewFCCLattice::Direction::C7South:
            return make_pair(SkewFCCLattice::Direction::C6West,
                             SkewFCCLattice::Direction::C0East);
            break;

        default:
            stringstream err;
            err << "Neighborhood::getOrthogonalDirections(" << d << ")";
            throw NotImplementedException(err.str());
    }
}
