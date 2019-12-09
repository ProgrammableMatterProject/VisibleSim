#ifndef CoatingBorder_H_
#define CoatingBorder_H_

#include "catoms3DBlock.h"

#include "coatingNeighborhood.hpp"

using namespace Catoms3D;

class Border {
private:
    std::function<bool(const Cell3DPosition&)> isInG;
    Lattice *lattice;
    Neighborhood *nbh;
public :
    Border(std::function<bool(const Cell3DPosition&)> _isInG,
           Neighborhood *_neighborhood);
    ~Border() {};

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

    /**
     * @param pos
     * @return true is pos is the seed of its plane
     */
    bool isPlaneSeed(const Cell3DPosition& pos) const;

    /**
     * @param pos
     * @return true if position pos could be a plane seed according to its neighborhood
     */
    bool couldBeSeed(const Cell3DPosition& pos) const;

    /**
     * When the area of the next plane is bigger than the one of the current plane.
     * One module of the current plane border should be chosen. A border following over the current plane in G is executed and the lowest position with a module on its top is actually elected a 3D Seed.
     * @param pos
     * @return true if the next plane is bigger than the current one and
     *  pos is the seed module of this next plane
     */
    bool isSeedBorderOnCurrentPlane(const Cell3DPosition& pos) const;

    /**
     * When the area of the next plane is smaller to that of the current plane.
     * Executes the border following algorithm int he border of the next plane and checks if pos is the lowest module with a filled position under it.
     * @param pos
     * @return true if the next plane is smaller than the current one and
     *  pos is the seed module of this next plane
     */
    bool isSeedBorderOnNextPlane(const Cell3DPosition& pos) const;

    /**
     * Checks if pos is the lowest module of the plane (current or next)
     *  with a filled position under it.
     * @param pos
     * @param next current plane if false, next plane if true;
     * @return true if pos is the lowest module of the plane (current or next)
     *  with a filled position under it.
     */
    bool isLowestOfBorderOnPlane(const Cell3DPosition& pos, bool next = false) const;
};

#endif /* CoatingBorder_H_ */
