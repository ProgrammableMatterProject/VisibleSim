#ifndef CoatingSeeding_H_
#define CoatingSeeding_H_

#include "catoms3DBlock.h"

#include "coatingNeighborhood.hpp"
#include "coatingBorder.hpp"

using namespace Catoms3D;

class Seeding {
private:
    std::function<bool(const Cell3DPosition&)> isInG;
    Lattice *lattice;
    Neighborhood *nbh;
    Border *border;
public :
    Seeding(std::function<bool(const Cell3DPosition&)> _isInG,
           Neighborhood *_neighborhood, Border *_border);
    ~Seeding() {};

    enum SeedDirection {
        Zward = 0, RevZward, LZward, RZward
    };

    static inline constexpr Cell3DPosition seedDirPositions[4] = {
        Cell3DPosition(-1,0,1), Cell3DPosition(0,-1,1),
        Cell3DPosition(-1,-1,1), Cell3DPosition(0,0,1),
    };

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
     * A plane seed module is a module that is responsible for attracting the first module of
     *  the next plane to its z + 1 coordinate
     * @param pos
     * @return true is pos is the seed of its plane
     */
    bool isPlaneSeed(const Cell3DPosition& pos,
                     SeedDirection &seedDir) const;

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
    bool isSeedBorderOnNextPlane(const Cell3DPosition& pos,
                                 SeedDirection &seedDir) const;

    /**
     * @param pos
     * @return true if pos is the module of the next plane with minimum coordinates
     *  and a module in G under it
     */
    bool isLowestOfBorderOnNextPlane(const Cell3DPosition& pos) const;

    /**
     * @param pos
     * @return true if pos is the module of the current plane with minimum coordinates
     *  and a module in G over it
     */
    bool isLowestOfBorderOnCurrentPlane(const Cell3DPosition& pos) const;

    /**
     * Provides a detailed print of why position pos is a seed or not
     * @param pos
     */
    void print(const Cell3DPosition& pos) const;

    bool hasLowerNeighborInCoating(const Cell3DPosition& pos) const;
    bool firstPositionOfPlaneHasNoBetterCandidate(const Cell3DPosition& firstPos,
                                                  SeedDirection dir) const;
};

#endif /* CoatingBorder_H_ */
