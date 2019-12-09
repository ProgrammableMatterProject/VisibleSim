#ifndef CoatingNeighborhood_H_
#define CoatingNeighborhood_H_

#include "catoms3DBlock.h"

#include "coatingUtils.hpp"

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
     * @return true if catom is a north seed and should attract modules to its north column
     */
    bool isNorthSeed(const Cell3DPosition& pos) const;

    /**
     * @return true if catom is a north seed and should attract modules to its south column
     */
    bool isSouthSeed(const Cell3DPosition& pos) const;

    /**
     * @return true if the position at the north of the current module is a merge point
     *  between the current column and an EAST position
     */
    bool isNorthLineOnMerge(const Cell3DPosition& pos) const;

    /**
     * @return true if the position at the south of the current module is a merge point
     *  between the current column and a WEST position
     */
    bool isSouthLineOnMerge(const Cell3DPosition& pos) const;

    /**
     * @param pos position to evaluate
     * @return true if catom is on the inner border of an internal hole in the shape,
     *  false otherwise
     */
    bool isOnInternalHole(const Cell3DPosition& pos) const;

    /**
     * @param idx current rotation index
     * @param currentPos current search position
     * @return the position of the next neighbor along the internal border
     */
    int getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos) const;

    /**
     * @param pos
     * @return true if pos is on an internal or external border
     */
    bool isOnBorder(const Cell3DPosition& pos) const;

    /**
     * @param pos
     * @return The index corresponding to the direction of the border following for pos
     */
    int getIndexForBorder(const Cell3DPosition& pos) const;
};

#endif /* CoatingNeighborhood_H_ */
