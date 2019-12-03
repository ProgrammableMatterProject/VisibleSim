/*! @file lattice.h
 * @brief Header file for the lattice simulation environment.
 * @author pthalamy
 *
 * Inspired by github.com/nazandre:VisibleSimConfigGenerator
 *
 */

#ifndef LATTICE_H__
#define LATTICE_H__

#include <string>
#include <vector>

#include "utils.h"
#include "exceptions.h"
#include "buildingBlock.h"
#include "vector3D.h"
#include "cell3DPosition.h"

namespace BaseSimulator {

/*! @brief Abstract class Lattice
 *
 */
class Lattice {
public:
    class OutOfLatticeInsertionException : public VisibleSimException {
    public:
        OutOfLatticeInsertionException(const Cell3DPosition& p)
            {
                stringstream ss;
                ss <<  "Trying to insert a block out of the grid at " << p << endl;
                m_msg = ss.str();
            }
    };

    class DoubleInsertionException : public VisibleSimException {
    public:
        DoubleInsertionException(const Cell3DPosition& p) {
            stringstream ss;
            ss << "Trying to insert a block on non-empty cell at " << p << endl;
            m_msg = ss.str();
        }
    };

    class InvalidDimensionsException : public VisibleSimException {
    public:
        InvalidDimensionsException(const Cell3DPosition& size) {
            stringstream ss;
            ss << "Lattice size in any direction cannot be negative or null: "
                << size << endl;
            m_msg = ss.str();
        }
    };

protected:
    static const string directionName[];

    map<const Cell3DPosition, Color> mapHighlightedCells;
public:
    enum Direction {MAX_NB_NEIGHBORS}; //!< Labels for a lattice cell's neighboring cells (virtual)
    /**
     * @brief Returns an integer corresponding to the direction opposite to d
     * @param d id of the direction from which we want the opposite
     * @return id of direction opposite to d
     */
    virtual short getOppositeDirection(short d) { return -1; }
    /**
     * @brief Returns the name string corresponding to direction d
     * @param d id of the direction from which we want the name
     * @return name string corresponding to direction d
     */
    virtual string getDirectionString(short d);

    /**
     * @brief Returns the direction in which cell neighbor is, relative to cell p
     * @param p the reference cell
     * @return a short int representing a lattice specific direction enum value
     */
    short getDirection(const Cell3DPosition &p, const Cell3DPosition &neighbor);

    Cell3DPosition gridSize; //!< The size of the 3D grid
    Vector3D gridScale; //!< The real size of a cell in the simulated world (Dimensions of a block)
    BuildingBlock **grid; //!< The grid as a 1-Dimensional array of BuildingBlock pointers
    unsigned int nbModules = 0; //!< The number of modules currently part of the lattice

    /**
     * @param z if z != -1, returns the bounds for the grid at height z
     * @note Applies only to non orthogonal coordinate systems such as SkewFCCLattice
     * @return the coordinates of the start of the grid
     */
    virtual Cell3DPosition getGridLowerBounds(int z = -1) const;

    /**
     * @param z if z != -1, returns the bounds for the grid at height z
     * @note Applies only to non orthogonal coordinate systems such as SkewFCCLattice
     * @return the coordinates of the end of the grid
     */
    virtual Cell3DPosition getGridUpperBounds(int z = -1) const;

    /**
     * @brief Abstract Lattice constructor.
     */
    Lattice();
    /**
     * @brief Abstract Lattice constructor. Builds a new lattice with the provided parameters
     * @param gsz The size of the grid
     * @param gsc The real size of a block on the grid, also equal to the scale of the grid
     */
    Lattice(const Cell3DPosition &gsz, const Vector3D &gsc);
    /**
     * @brief Abstract Lattice destructor. Responsible for deleting the grid array.
     */
    virtual ~Lattice();

    /**
     * @brief Adds block bb to cell with position p of the grid
     * @param bb The block to add to the grid
     * @param p The position of the cell on which the block will be inserted
     * @param count indicates whether the inserted modules should be counted in nbModules
     */
    void insert(BuildingBlock* bb, const Cell3DPosition &p, bool count = true);
    /**
     * @brief Removes block on cell p of the grid
     * @param p The position of the block to remove
     * @param count indicates whether the removed modules should be counted towards nbModules
     */
    void remove(const Cell3DPosition &p, bool count = true);
    /**
     * @brief Indicates if cell at position p is inside the grid
     * @param p The position of the cell to test
     * @return true if cell at position p is in grid, false otherwise
     */
    virtual bool isInGrid(const Cell3DPosition &p) const;
    /**
     * @brief Returns a one-dimensional array index for grid position p
     * @param p The position of the cell for which the index is needed
     * @return The index of the cell in the lattice's 1D array of cells
     */
    virtual unsigned int getIndex(const Cell3DPosition &p) const;
    /**
     * @brief Indicates if cell at position p has a block on it
     * @param p The position of the cell to test
     * @return true if cell at position p has a block on it, false otherwise or if p is out of grid
     */
    bool cellHasBlock(const Cell3DPosition &p) const;
    /**
     * @brief Indicates if cell at position p is an empty grid cell
     * @param p The position of the cell to test
     * @return true if cell at position p is in grid and empty, false otherwise
     */
    bool isFree(const Cell3DPosition &p) const;
    /**
     * @brief Returns a pointer to the block on cell p of the grid
     * @param p The position of the block to get
     * @return A pointer to the block on cell p or NULL if p is not in grid or empty
     */
    BuildingBlock *getBlock(const Cell3DPosition &p) const;
    /**
     * @brief Returns the location of all alive neighbors for cell pos
     * @param pos The cell to consider
     * @return A vector containing the position of all alive neighbors of cell pos
     */
    std::vector<Cell3DPosition> getActiveNeighborCells(const Cell3DPosition &pos);
    /**
     * @brief Returns the location of all empty neighbor cell positions around pos
     * @param pos The cell to consider
     * @return A vector containing the position of all free cells around pos
     */
    std::vector<Cell3DPosition> getFreeNeighborCells(const Cell3DPosition &pos);
    /**
     * @brief Returns the location of all neighbor cells for cell pos
     * @param pos The cell to consider
     * @return A vector containing the position of all cells (empty and full) around pos
     */
    std::vector<Cell3DPosition> getNeighborhood(const Cell3DPosition &pos);
    /**
     * @brief Indicates whether cells in argument are adjacent to each other
     * @param p1 the first cell
     * @param p2 the second cell
     * @return a boolean indicating whether the two cells p1 and p2 are adjacent
     */
    bool cellsAreAdjacent(const Cell3DPosition &p1, const Cell3DPosition &p2);

    /**
     * @brief Returns the total number of cells on the grid
     * @return Total number of cells on the grid
     */
    inline int getNumberOfCells() { return gridSize[0] * gridSize[1] * gridSize[2]; };

    /**
     * @brief Transforms an integer grid position into a real world position considering a 1x1x1 grid scale
     * @param pos The grid position to consider
     * @return The corresponding world position
     */
    virtual Vector3D gridToUnscaledWorldPosition(const Cell3DPosition &pos) = 0; // TODO: REFACTOR

    /**
     * @brief Transforms a real world position considering a 1x1x1 grid scale into an integer grid position
     * @param pos The unscaled world position to consider
     * @return The corresponding grid position
     */
    virtual Cell3DPosition unscaledWorldToGridPosition(const Vector3D &pos) = 0;

    /**
     * @brief Transforms an integer grid position into a real world position considering the actual scale of the lattice
     * @param pos The grid position to consider
     * @return The corresponding world position
     */
    virtual Vector3D gridToWorldPosition(const Cell3DPosition &pos);

    /**
     * @brief Transforms a real world position into its grid equivalent
     * @param pos The world position to consider
     * @return The corresponding grid position
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos) = 0;
    /**
     * @brief Returns the relative position of all cells around cell p
     * @param p The position of the cell to consider
     * @return A vector containing all relative position of neighbor cells
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p) = 0;
    /**
     * @brief Overriden getter to get the maximum number of neighbor a lattice cell can have
     * @return the maximum number of neighbor for the callee lattice
     */
    virtual inline const int getMaxNumNeighbors() { return MAX_NB_NEIGHBORS; };

    /**
     * @brief Returns the Cell3DPosition in some direction from a reference cell
     * @return Position of the cell in direction "direction" from cell pRef
     */
    virtual Cell3DPosition getCellInDirection(const Cell3DPosition &pRef,
                                              int direction) = 0;

    /**
     * Computes the distance between two lattice cells (Manhattan/network distance).
     *
     * @param p1 first cell
     * @param p2 second cell
     *
     * @return distance between p1 and p2
     */
    virtual unsigned int getCellDistance(const Cell3DPosition &p1, const Cell3DPosition &p2);

    virtual void glDraw() {};

    virtual void highlightCell(const Cell3DPosition& pos, const Color &color = YELLOW);
    virtual void unhighlightCell(const Cell3DPosition& pos);
    virtual void resetCellHighlights(); // Unhighlight all highlighted cells
};

/*! @brief 2-Dimensional Lattice abstract class
 *
 */
class Lattice2D : public Lattice {
    static const string directionName[];
public:
    enum Direction {MAX_NB_NEIGHBORS}; //!< @copydoc Lattice::Direction

    /**
     * @brief Abstract Lattice 2D constructor.
     */
    Lattice2D();
    /**
     * @brief Abstract Lattice 2D constructor.
     * @param gsz The size of the grid
     * @param gsc The real size of a block on the grid, also equal to the scale of the grid
     */
    Lattice2D(const Cell3DPosition &gsz, const Vector3D &gsc);
    /**
     * @brief Abstract Lattice2D destructor.
     */
    virtual ~Lattice2D();

    /**
     * @copydoc Lattice::gridToUnscaledWorldPosition
     */
    virtual Vector3D gridToUnscaledWorldPosition(const Cell3DPosition &pos) override = 0;

    /**
     * @copydoc Lattice::unscaledWorldToGridPosition
     */
    virtual Cell3DPosition unscaledWorldToGridPosition(const Vector3D &pos) override = 0;

    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos) override = 0;
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p) override = 0;
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */
    virtual inline const int getMaxNumNeighbors() override = 0;


    /**
     * @copydoc Lattice::getCellInDirection
     */
    virtual Cell3DPosition getCellInDirection(const Cell3DPosition &pRef,
                                              int direction) override = 0;
};

/*! @brief 3-Dimensional Lattice abstract class
 *
 */
class Lattice3D : public Lattice {
    static const string directionName[];
public:
    enum Direction {MAX_NB_NEIGHBORS}; //!< @copydoc Lattice::Direction

    /**
     * @brief Abstract Lattice 3D constructor.
     */
    Lattice3D();
    /**
     * @brief Abstract Lattice 3D constructor.
     * @param gsz The size of the grid
     * @param gsc The real size of a block on the grid, also equal to the scale of the grid
     */
    Lattice3D(const Cell3DPosition &gsz, const Vector3D &gsc);
    /**
     * @brief Abstract Lattice3D destructor.
     */
    virtual ~Lattice3D();

    /**
     * @copydoc Lattice::gridToUnscaledWorldPosition
     */
    virtual Vector3D gridToUnscaledWorldPosition(const Cell3DPosition &pos) override = 0;
    /**
     * @copydoc Lattice::unscaledWorldToGridPosition
     */
    virtual Cell3DPosition unscaledWorldToGridPosition(const Vector3D &pos) override = 0;
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos) override = 0;
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p) override = 0;
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */
    virtual inline const int getMaxNumNeighbors() override = 0;

    /**
     * @copydoc Lattice::getCellInDirection
     */
    virtual Cell3DPosition getCellInDirection(const Cell3DPosition &pRef,
                                              int direction) override = 0;
};

/*! @brief Square 2D Lattice
 *
 * Used by SmartBlocks
 *
 */
class SLattice : public Lattice2D {
    vector<Cell3DPosition> nCells{
        Cell3DPosition(0,1,0),  // NORTH
            Cell3DPosition(1,0,0), // EAST
            Cell3DPosition(0,-1,0), // SOUTH
            Cell3DPosition(-1,0,0)  // WEST
            }; //!< Vector containing relative position of neighboring cells

    static const string directionName[];
public:
    enum Direction {North = 0, East, South, West, MAX_NB_NEIGHBORS}; //!< @copydoc Lattice::Direction
    //!< @copydoc Lattice::getOppositeDirection
    virtual short getOppositeDirection(short d) override;
    //!< @copydoc Lattice::getDirectionString
    virtual string getDirectionString(short d) override;

    /**
     * @brief SLattice constructor.
     */
    SLattice();
    /**
     * @brief SLattice constructor.
     * @param gsz The size of the grid
     * @param gsc The real size of a block on the grid, also equal to the scale of the grid
     */
    SLattice(const Cell3DPosition &gsz, const Vector3D &gsc);
    /**
     * @brief SLattice destructor.
     */
    ~SLattice();

    /**
     * @copydoc Lattice::gridToUnscaledWorldPosition
     */
    virtual Vector3D gridToUnscaledWorldPosition(const Cell3DPosition &pos) override;
    /**
     * @copydoc Lattice::unscaledWorldToGridPosition
     */
    virtual Cell3DPosition unscaledWorldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p) override;
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */
    virtual inline const int getMaxNumNeighbors() override { return MAX_NB_NEIGHBORS; }
    /**
     * @copydoc Lattice::getCellInDirection
     */
    virtual Cell3DPosition getCellInDirection(const Cell3DPosition &pRef,
                                              int direction) override;
};

/*! @brief Hexagonal 2D Lattice
 *
 * Used by Catoms2D blocks. Be careful, the 2 dimensions are **x and z**.
 *
 */
class HLattice : public Lattice2D {
    // This is in the same order as pickingTextures / NeighborDirections
    vector<Cell3DPosition> nCellsOdd{
        Cell3DPosition(1,0,0),  // RIGHT
            Cell3DPosition(1,0,1), // TOP-RIGHT
            Cell3DPosition(0,0,1), // TOP-LEFT
            Cell3DPosition(-1,0,0), // LEFT
            Cell3DPosition(0,0,-1), // BOTTOM-LEFT
            Cell3DPosition(1,0,-1)  // BOTTOM-RIGHT
            }; //!< Vector containing relative position of neighboring cells for even(z) cells
    vector<Cell3DPosition> nCellsEven{
        Cell3DPosition(1,0,0),  // RIGHT
            Cell3DPosition(0,0,1), // TOP-RIGHT
            Cell3DPosition(-1,0,1), // TOP-LEFT
            Cell3DPosition(-1,0,0), // LEFT
            Cell3DPosition(-1,0,-1), // BOTTOM-LEFT
            Cell3DPosition(0,0,-1)   // BOTTOM-RIGHT
            }; //!< Vector containing relative position of neighboring cells for odd(z) cells

    static const string directionName[];
public:
    enum Direction {Right = 0, TopRight = 1, TopLeft = 2,
                    Left = 3, BottomLeft = 4, BottomRight = 5, MAX_NB_NEIGHBORS}; //!< @copydoc Lattice::Direction
    //!< @copydoc Lattice::getOppositeDirection
    virtual short getOppositeDirection(short d) override;
    //!< @copydoc Lattice::getDirectionString
    virtual string getDirectionString(short d) override;

    /**
     * @brief HLattice constructor.
     */
    HLattice();
    /**
     * @brief HLattice constructor.
     * @param gsz The size of the grid
     * @param gsc The real size of a block on the grid, also equal to the scale of the grid
     */
    HLattice(const Cell3DPosition &gsz, const Vector3D &gsc);
    /**
     * @brief HLattice destructor.
     */
    ~HLattice();

    /**
     * @copydoc Lattice::gridToUnscaledWorldPosition
     */
    virtual Vector3D gridToUnscaledWorldPosition(const Cell3DPosition &pos) override;
    /**
     * @copydoc Lattice::unscaledWorldToGridPosition
     */
    virtual Cell3DPosition unscaledWorldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::gridToWorldPosition
     */
    virtual Vector3D gridToWorldPosition(const Cell3DPosition &pos) override;
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p) override;
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */
    virtual inline const int getMaxNumNeighbors() override { return MAX_NB_NEIGHBORS; }
    /**
     * @copydoc Lattice::getCellInDirection
     */
    virtual Cell3DPosition getCellInDirection(const Cell3DPosition &pRef,
                                              int direction) override;
};

/*! @brief 3D Face-Centered Cubic Lattice
 *
 * Used by Catoms3D
 *
 */
class FCCLattice : public Lattice3D {
    // The index i of the relative position in the vector corresponds to the cell on interface i of a block
    vector<Cell3DPosition> nCellsOdd{
        Cell3DPosition(1,0,0),  // 0
            Cell3DPosition(0,1,0), // 1
            Cell3DPosition(1,1,1), // 2
            Cell3DPosition(0,1,1), // 3
            Cell3DPosition(0,0,1), // 4
            Cell3DPosition(1,0,1), // 5
            Cell3DPosition(-1,0,0), // 6
            Cell3DPosition(0,-1,0), // 7
            Cell3DPosition(0,0,-1), // 8
            Cell3DPosition(1,0,-1), // 9
            Cell3DPosition(1,1,-1), // 10
            Cell3DPosition(0,1,-1), // 11
            }; //!< Vector containing relative position of neighboring cells for even(z) cells;

    vector<Cell3DPosition> nCellsEven{
        Cell3DPosition(1,0,0), // 0
            Cell3DPosition(0,1,0),  // 1
            Cell3DPosition(0,0,1),  // 2
            Cell3DPosition(-1,0,1), // 3
            Cell3DPosition(-1,-1,1), // 4
            Cell3DPosition(0,-1,1), // 5
            Cell3DPosition(-1,0,0), // 6
            Cell3DPosition(0,-1,0), // 7
            Cell3DPosition(-1,-1,-1), // 8
            Cell3DPosition(0,-1,-1),  // 9
            Cell3DPosition(0,0,-1),   // 10
            Cell3DPosition(-1,0,-1) // 11
            }; //!< Vector containing relative position of neighboring cells for odd(z) cells;

    //!< Neighborhood Planes for blocking cells computation

    Cell3DPosition sideOneOddXY[4] = { Cell3DPosition(1,0,-1), Cell3DPosition(1,1,-1),
                                      Cell3DPosition(0,1,-1), Cell3DPosition(0,0,-1) };
    Cell3DPosition sideTwoOddXY[4] = { Cell3DPosition(1,0,1),Cell3DPosition(1,1,1),
                                       Cell3DPosition(0,1,1),Cell3DPosition(0,0,1) };
    Cell3DPosition sideOneEvenXY[4] = { Cell3DPosition(0,0,-1),Cell3DPosition(-1,-1,-1),
                                        Cell3DPosition(-1,0,-1),Cell3DPosition(0,-1,-1) };
    Cell3DPosition sideTwoEvenXY[4] = { Cell3DPosition(0,0,1),Cell3DPosition(-1,-1,1),
                                        Cell3DPosition(-1,0,1), Cell3DPosition(0,-1,1) };

    Cell3DPosition sideOneOddXZ[5] = { Cell3DPosition(1,0,-1),Cell3DPosition(1,1,-1),
                                       Cell3DPosition(1,1,1),Cell3DPosition(1,0,1),
                                       Cell3DPosition(1,0,0) };
    Cell3DPosition sideTwoOddXZ[5] = { Cell3DPosition(0,1,-1),Cell3DPosition(0,0,-1),
                                       Cell3DPosition(0,1,1),Cell3DPosition(0,0,1),
                                       Cell3DPosition(-1,0,0) };
    Cell3DPosition sideOneEvenXZ[5] = { Cell3DPosition(0,0,-1),Cell3DPosition(0,-1,-1),
                                        Cell3DPosition(0,0,1),Cell3DPosition(0,-1,1),
                                        Cell3DPosition(1,0,0) };
    Cell3DPosition sideTwoEvenXZ[5] = { Cell3DPosition(-1,0,-1),Cell3DPosition(-1,-1,-1),
                                        Cell3DPosition(-1,0,1),Cell3DPosition(-1,-1,1),
                                        Cell3DPosition(-1,0,0) };
    Cell3DPosition sideOneOddYZ[5] = { Cell3DPosition(0,0,-1),Cell3DPosition(1,0,-1),
                                       Cell3DPosition(0,0,1),Cell3DPosition(1,0,1),
                                       Cell3DPosition(0,-1,0) };
    Cell3DPosition sideTwoOddYZ[5] = { Cell3DPosition(1,1,-1),Cell3DPosition(0,1,-1),
                                       Cell3DPosition(1,1,1),Cell3DPosition(0,1,1),
                                       Cell3DPosition(0,1,0) };
    Cell3DPosition sideOneEvenYZ[5] = { Cell3DPosition(0,-1,-1),Cell3DPosition(-1,-1,-1),
                                        Cell3DPosition(0,-1,1),Cell3DPosition(-1,-1,1),
                                        Cell3DPosition(0,-1,0) };
    Cell3DPosition sideTwoEvenYZ[5] = { Cell3DPosition(0,0,-1),Cell3DPosition(-1,0,-1),
                                           Cell3DPosition(0,0,1),Cell3DPosition(-1,0,1),
                                           Cell3DPosition(0,1,0) };

    Cell3DPosition xyPos[4] = { Cell3DPosition(-1,0,0), Cell3DPosition(1,0,0),
                                Cell3DPosition(0,-1,0), Cell3DPosition(0,1,0) };

    static const string directionName[];
    bool *tabLockedCells;
    unsigned short *tabDistances;

    // NEIGHBORDHOOD RESTRICTIONS
    enum class BlockingPositionPlane { XY, YZ, XZ };
    /**
     * Sets the sideOne and sideTwo pointers to the cells from both sides of the input plane and in direction d, of the right odd/even neighborhood
     * @param plane
     * @param pos reference position
     * @param sideOne
     * @param sideTwo
     * @param d direction of the requested cell
     * @param evenZ
     */
    void setPlaneSides(BlockingPositionPlane plane,
                       const Cell3DPosition& pos,
                       Cell3DPosition& sideOne, Cell3DPosition& sideTwo,
                       int d, bool evenZ);
    bool isPositionUnblockedSide(const Cell3DPosition &pos);
    bool isPositionUnblocked(const Cell3DPosition &pos, BlockingPositionPlane plane);
    bool isPositionUnblockedSide(const Cell3DPosition &pos, const Cell3DPosition &ignore);
    bool isPositionUnblocked(const Cell3DPosition &pos, const Cell3DPosition &ignore,
                             BlockingPositionPlane plane);
public:
    enum Direction {
        C0East, C1North, C2TopNE, C3TopNW,
        C4TopSW, C5TopSE, C6West, C7South,
        C8BottomSW,C9BottomSE, C10BottomNE, C11BottomNW,
        MAX_NB_NEIGHBORS
    };//!< @copydoc Lattice::Direction

    // enum Direction {Con0 = 0, Con1, Con2, Con3, Con4, Con5,
    //                 Con6, Con7, Con8, Con9, Con10, Con11, MAX_NB_NEIGHBORS}; //!< @copydoc Lattice::Direction
    //!< @copydoc Lattice::getOppositeDirection
    virtual short getOppositeDirection(short d) override;
    //!< @copydoc Lattice::getDirectionString
    virtual string getDirectionString(short d) override;

    /**
     * @brief FCCLattice constructor.
     */
    FCCLattice();
    /**
     * @brief FCCLattice constructor.
     * @param gsz The size of the grid
     * @param gsc The real size of a block on the grid, also equal to the scale of the grid
     */
    FCCLattice(const Cell3DPosition &gsz, const Vector3D &gsc);
    /**
     * @brief FCCLattice destructor.
     */
    ~FCCLattice();

    /**
     * @copydoc Lattice::gridToUnscaledWorldPosition
     */
    virtual Vector3D gridToUnscaledWorldPosition(const Cell3DPosition &pos) override;
    /**
     * @copydoc Lattice::unscaledWorldToGridPosition
     */
    virtual Cell3DPosition unscaledWorldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p) override;
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */
    virtual inline const int getMaxNumNeighbors() override { return MAX_NB_NEIGHBORS; }

    /**
     * @copydoc Lattice::getCellInDirection
     */
    virtual Cell3DPosition getCellInDirection(const Cell3DPosition &pRef,
                                              int direction) override;

    // NEIGHBORHOOD RESTRICTIONS
    bool isPositionBlocked(const Cell3DPosition &pos);
    bool isPositionBlocked(const Cell3DPosition &pos, const Cell3DPosition &ignore);
    // bool isPositionBlockable(const Cell3DPosition &pos);

    // FIXME: @Pthy: These are blockcode related and should not be in the simulator core.
    bool lockCell(const Cell3DPosition &pos);
    bool unlockCell(const Cell3DPosition &pos);
    void initTabDistances();
    unsigned short getDistance(const Cell3DPosition &pos);
    void setDistance(const Cell3DPosition &pos,unsigned short d);
    void glDraw() override;
};


/*! @brief 3D Face-Centered Cubic Lattice with skew coordinate system (bent z-axis)
 *
 * Used by Catoms3D
 *
 */
class SkewFCCLattice : public FCCLattice {
    // The index i of the relative position in the vector corresponds to the cell on interface i of a block
    vector<Cell3DPosition> nCells{
        Cell3DPosition(1,0,0),  // 0
        Cell3DPosition(0,1,0), // 1
        Cell3DPosition(0,0,1), // 2
        Cell3DPosition(-1,0,1), // 3
        Cell3DPosition(-1,-1,1), // 4
        Cell3DPosition(0,-1,1), // 5
        Cell3DPosition(-1,0,0), // 6
        Cell3DPosition(0,-1,0), // 7
        Cell3DPosition(0,0,-1), // 8
        Cell3DPosition(1,0,-1), // 9
        Cell3DPosition(1,1,-1), // 10
        Cell3DPosition(0,1,-1), // 11
        }; //!< Vector containing relative position of neighboring cells;

    static const string directionName[];
public:
    /**
     * @brief SkewFCCLattice constructor.
     */
    SkewFCCLattice();
    /**
     * @brief SkewFCCLattice constructor.
     * @param gsz The size of the grid
     * @param gsc The real size of a block on the grid, also equal to the scale of the grid
     */
    SkewFCCLattice(const Cell3DPosition &gsz, const Vector3D &gsc);
    /**
     * @brief FCCLattice destructor.
     */
    ~SkewFCCLattice();

    /**
     * @copydoc Lattice::getIndex
     */
    virtual unsigned int getIndex(const Cell3DPosition &p) const override;

    /**
     * @copydoc Lattice::getGridLowerBounds
     */
    virtual Cell3DPosition getGridLowerBounds(int z = -1) const override;

    /**
     * @copydoc Lattice::getGridUpperBounds
     */
    virtual Cell3DPosition getGridUpperBounds(int z = -1) const override;

    /**
     * @copydoc Lattice::gridToUnscaledWorldPosition
     */
    virtual Vector3D gridToUnscaledWorldPosition(const Cell3DPosition &pos) override;
    /**
     * @copydoc Lattice::unscaledWorldToGridPosition
     */
    virtual Cell3DPosition unscaledWorldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p) override;

    /**
     * @copydoc Lattice::getCellInDirection
     */
    virtual Cell3DPosition getCellInDirection(const Cell3DPosition &pRef,
                                              int direction) override;

    /**
     * @copydoc Lattice::getCellDistance
     */
    virtual unsigned int getCellDistance(const Cell3DPosition &p1, const Cell3DPosition &p2) override;
}; // SkewFCCLattice

/*! @brief 3D Simple Cubic Lattice
 *
 * Used by BlinkyBlocks, RobotBlocks and Okteen
 *
 */
class SCLattice : public Lattice3D {
    vector<Cell3DPosition> nCells{
        Cell3DPosition(0,0,-1), // BOTTOM
            Cell3DPosition(0,1,0), // BACK
            Cell3DPosition(1,0,0),  // RIGHT
            Cell3DPosition(-1,0,0),  // LEFT
            Cell3DPosition(0,-1,0),  // FRONT
            Cell3DPosition(0,0,1)  // TOP
            }; //!< Vector containing relative position of neighboring cells
    static const string directionName[];
public:
    enum Direction { Bottom = 0, Back = 1, Right, Left, Front, Top, MAX_NB_NEIGHBORS}; //!< @copydoc Lattice::Direction
    //!< @copydoc Lattice::getOppositeDirection
    virtual short getOppositeDirection(short d) override;
    //!< @copydoc Lattice::getDirectionString
    virtual string getDirectionString(short d) override;
    Cell3DPosition getNeighborRelativePos(Direction d) { return nCells[d]; };
    /**
     * @brief SCLattice constructor.
     */
    SCLattice();
    /**
     * @brief SCLattice constructor.
     * @param gsz The size of the grid
     * @param gsc The real size of a block on the grid, also equal to the scale of the grid
     */
    SCLattice(const Cell3DPosition &gsz, const Vector3D &gsc);
    /**
     * @brief SCLattice destructor.
     */
    ~SCLattice();

    /**
     * @copydoc Lattice::gridToUnscaledWorldPosition
     */
    virtual Vector3D gridToUnscaledWorldPosition(const Cell3DPosition &pos) override;
    /**
     * @copydoc Lattice::unscaledWorldToGridPosition
     */
    virtual Cell3DPosition unscaledWorldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p) override;
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */
    virtual inline const int getMaxNumNeighbors() override { return MAX_NB_NEIGHBORS; }

    /**
     * @copydoc Lattice::getCellInDirection
     */
    virtual Cell3DPosition getCellInDirection(const Cell3DPosition &pRef,
                                              int direction) override;
    /**
     * @copydoc Lattice::glDraw
     */
    virtual void glDraw() override;
};

/*! @brief 3D Broadcast Lattice
 *
 * Used by MultiRobots. All blocks are connected to each other.
 *
 *
 */
class BCLattice : public Lattice3D {
public:
    enum Direction {BROADCAST = 0, MAX_NB_NEIGHBORS}; //!< @copydoc Lattice::Direction
    //!< @copydoc Lattice::getOppositeDirection
    virtual short getOppositeDirection(short d) override;
    //!< @copydoc Lattice::getDirectionString
    virtual string getDirectionString(short d) override;

    list<BuildingBlock*> connected; //!< contains all cells with a block on it

    /**
     * @brief BCLattice constructor.
     */
    BCLattice();
    /**
     * @brief BCLattice constructor.
     * @param gsz The size of the grid
     * @param gsc The real size of a block on the grid, also equal to the scale of the grid
     */
    BCLattice(const Cell3DPosition &gsz, const Vector3D &gsc);
    /**
     * @brief BCLattice destructor.
     */
    ~BCLattice();

    /**
     * @copydoc Lattice::gridToUnscaledWorldPosition
     */
    virtual Vector3D gridToUnscaledWorldPosition(const Cell3DPosition &pos) override;
    /**
     * @copydoc Lattice::unscaledWorldToGridPosition
     */
    virtual Cell3DPosition unscaledWorldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos) override;
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p) override;
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */
    virtual inline const int getMaxNumNeighbors() override { return MAX_NB_NEIGHBORS; }

    /**
     * @copydoc Lattice::getCellInDirection
     */
    virtual Cell3DPosition getCellInDirection(const Cell3DPosition &pRef,
                                              int direction) override
    {
         // Does not apply to mobile-type modular robots
         return Cell3DPosition(0,0,0);
    }
};


}

#endif
