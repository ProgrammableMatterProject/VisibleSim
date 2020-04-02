#ifndef CoatingNeighborhood_H_
#define CoatingNeighborhood_H_

#include "robots/catoms3D/catoms3DBlock.h"

#include "coatingUtils.hpp"

#include "grid/lattice.h"

using namespace Catoms3D;

class Neighborhood {
private:
    std::function<bool(const Cell3DPosition&)> isInG;
    Lattice *lattice;
public :
    Neighborhood(std::function<bool(const Cell3DPosition&)> _isInG);
    ~Neighborhood() {};

    /**
     * @param d CCWDir direction to evaluate
     * @return cell in direction d from the current module
     */
    Cell3DPosition cellInDirection(const Cell3DPosition& pos, PlanarDir d) const;

    /**
     * @param d direction to evaluate
     * @return true if cell in direction d from the current module is in the CSG object
     */
    bool directionIsInCSG(const Cell3DPosition& pos, PlanarDir d) const;

    /**
     * @param pos reference position
     * @param dir
     * @return true if module has a neighbor in lattice direction dir
     */
    bool hasNeighborInDirection(const Cell3DPosition& pos,
                                SkewFCCLattice::Direction dir) const;

    /**
     * @param pos
     * @return true if pos has no horizontal neighbor in position
     */
    bool hasNoHorizontalNeighbor(const Cell3DPosition& pos) const;

    /**
     * Returns the same plane directions orthogonal to direction d in the FCC Lattice,
     *  (therefore the square lattice formed by a single layer of FCC)
     * @param d direction to evaluate
     * @return the pair of directions orthogonal to d in the FCC lattice
     */
    static pair<short, short> getOrthogonalDirections(short d);
};

#endif /* CoatingNeighborhood_H_ */
