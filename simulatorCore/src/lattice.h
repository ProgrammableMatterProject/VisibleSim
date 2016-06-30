/*! @file lattice.h
 * @brief Header file for the lattice simulation environment.
 *
 * Inspired by github.com/nazandre:VisibleSimConfigGenerator
 *
 */

#ifndef LATTICE_H__
#define LATTICE_H__

#include <string>
#include <vector>

#include "buildingBlock.h"
#include "vector3D.h"
#include "cell3DPosition.h"

namespace BaseSimulator {

/*! @brief Abstract class Lattice
 *
 */
class Lattice {
    struct InvalidInsertionException : std::exception {
        const char* what() const noexcept {return "Ooops!\n";}
    };    
public:
    enum Direction {};
    
    Cell3DPosition gridSize; //!< The size of the 3D grid
    Vector3D gridScale; //!< The real size of a cell in the simulated world (Dimensions of a block)
    BuildingBlock **grid; //!< The grid as a 1-Dimensional array of BuildingBlock pointers

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
     */
    void insert(BuildingBlock* bb, const Cell3DPosition &p);
    /**
     * @brief Removes block on cell p of the grid
     * @param p The position of the block to remove
     */
    void remove(const Cell3DPosition &p);
    /**
     * @brief Indicates if cell at position p is inside the grid
     * @param p The position of the cell to test
     * @return true if cell at position p is in grid, false otherwise
     */
    bool isInGrid(const Cell3DPosition &p);
    /**
     * @brief Returns a one-dimensional array index for grid position p
     * @param p The position of the cell for which the index is needed
     * @return The index of the cell in the lattice's 1D array of cells
     */
    int getIndex(const Cell3DPosition &p);
    /**
     * @brief Indicates if cell at position p has a block on it
     * @param p The position of the cell to test
     * @return true if cell at position p has a block on it, false otherwise or if p is out of grid
     */
    bool cellHasBlock(const Cell3DPosition &p);
    /**
     * @brief Indicates if cell at position p is an empty grid cell
     * @param p The position of the cell to test
     * @return true if cell at position p is in grid and empty, false otherwise
     */
    bool isFree(const Cell3DPosition &p);
    /**
     * @brief Returns a pointer to the block on cell p of the grid
     * @param p The position of the block to get
     * @return A pointer to the block on cell p or NULL if p is not in grid or empty
     */
    BuildingBlock *getBlock(const Cell3DPosition &p);
    /**
     * @brief Returns the location of all alive neighbors for cell pos
     * @param pos The cell to consider
     * @return A vector containing the position of all alive neighbors of cell pos
     */
    std::vector<Cell3DPosition> getActiveNeighborCells(const Cell3DPosition &pos);
    /**
     * @brief Returns the location of all neighbor cells for cell pos
     * @param pos The cell to consider
     * @return A vector containing the position of all cells (empty and full) around pos
     */
    std::vector<Cell3DPosition> getNeighborhood(const Cell3DPosition &pos);

    /**
     * @brief Returns the total number of cells on the grid
     * @return Total number of cells on the grid
     */
    inline int getNumberOfCells() { return gridSize[0] * gridSize[1] * gridSize[2]; };

    /**
     * @brief Transforms an integer grid position into a real world position
     * @param pos The grid position to consider
     * @return The corresponding world position
     */
    virtual Vector3D gridToWorldPosition(const Cell3DPosition &pos) = 0;
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
    virtual inline const int getMaxNumNeighbors() { return 0; };

    virtual inline const int getOppositeDirection(Direction i) { return 0; };
};

/*! @brief 2-Dimensional Lattice abstract class
 *
 */
class Lattice2D : public Lattice {

public:
    enum Direction {};

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
     * @copydoc Lattice::gridToWorldPosition
     */
    virtual Vector3D gridToWorldPosition(const Cell3DPosition &pos) = 0;
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos) = 0;
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p) = 0;
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */    
    virtual inline const int getMaxNumNeighbors() = 0;
};

/*! @brief 3-Dimensional Lattice abstract class
 *
 */
class Lattice3D : public Lattice {

public:
    enum Direction {};

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
     * @copydoc Lattice::gridToWorldPosition
     */
    virtual Vector3D gridToWorldPosition(const Cell3DPosition &pos) = 0;
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos) = 0;
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p) = 0;
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */    
    virtual inline const int getMaxNumNeighbors() = 0;
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
    enum Direction {North = 0, East, South, West, MAX_NB_NEIGHBORS};
    static int getOpposite(int d);
    static string getString(int d);

    static const int maxNumNeighbors = 4; //!< The maximum number of neighbors every cell on a lattice can have
    
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
     * @copydoc Lattice::gridToWorldPosition
     */
    virtual Vector3D gridToWorldPosition(const Cell3DPosition &pos);
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos);
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p);
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */    
    virtual inline const int getMaxNumNeighbors() { return maxNumNeighbors; }

    virtual inline const int getOppositeDirection(Direction i) { return 0; };
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
                    Left = 3, BottomLeft = 4, BottomRight = 5, MAX_NB_NEIGHBORS};
    static int getOpposite(int d);
    static string getString(int d);

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

    static const int maxNumNeighbors = 6; //!< The maximum number of neighbors every cell on a lattice can have
    
    /**
     * @copydoc Lattice::gridToWorldPosition
     */
    virtual Vector3D gridToWorldPosition(const Cell3DPosition &pos);
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos);
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p);
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */    
    virtual inline const int getMaxNumNeighbors() { return maxNumNeighbors; }
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

    static const string directionName[];
public:
    enum Direction {Con0 = 0, Con1, Con2, Con3, Con4, Con5,
                    Con6, Con7, Con8, Con9, Con10, Con11, MAX_NB_NEIGHBORS};
    static int getOpposite(int d);
    static string getString(int d);
    
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

    static const int maxNumNeighbors = 12; //!< The maximum number of neighbors every cell on a lattice can have
    
    /**
     * @copydoc Lattice::gridToWorldPosition
     */
    virtual Vector3D gridToWorldPosition(const Cell3DPosition &pos);
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos);
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p);
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */    
    virtual inline const int getMaxNumNeighbors() { return maxNumNeighbors; }
};

/*! @brief 3D Simple Cubic Lattice
 *
 * Used by BlinkyBlocks and RobotBlocks
 *
 */
class SCLattice : public Lattice3D {
    vector<Cell3DPosition> nCells{
        Cell3DPosition(0,0,-1), // BOTTOM
            Cell3DPosition(0,1,0), // BACK
            Cell3DPosition(1,0,0),  // RIGHT
            Cell3DPosition(0,-1,0),  // FRONT
            Cell3DPosition(-1,0,0),  // LEFT
            Cell3DPosition(0,0,1)  // TOP
            }; //!< Vector containing relative position of neighboring cells
    static const string directionName[];
public:
    enum Direction { Bottom = 0, Back = 1, Right, Front, Left, Top, MAX_NB_NEIGHBORS};
    static int getOpposite(int d);
    static string getString(int d);
    
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

    static const int maxNumNeighbors = 6; //!< The maximum number of neighbors every cell on a lattice can have
    
    /**
     * @copydoc Lattice::gridToWorldPosition
     */
    virtual Vector3D gridToWorldPosition(const Cell3DPosition &pos);
    /**
     * @copydoc Lattice::worldToGridPosition
     */
    virtual Cell3DPosition worldToGridPosition(const Vector3D &pos);
    /**
     * @copydoc Lattice::getRelativeConnectivity
     */
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(const Cell3DPosition &p);
    /**
     * @copydoc Lattice::getMaxNumNeighbors
     */    
    virtual inline const int getMaxNumNeighbors() { return maxNumNeighbors; }
};

}

#endif
