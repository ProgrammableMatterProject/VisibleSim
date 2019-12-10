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
};

#endif /* CoatingBorder_H_ */
