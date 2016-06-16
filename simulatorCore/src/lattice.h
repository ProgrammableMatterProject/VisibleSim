/*! \file lattice.h
 * \brief Header file for the lattice simulation environment. 
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
#include "Cell3DPosition.h"

namespace BaseSimulator {

/*! \brief Abstract class Lattice
 * 
 */
class Lattice {

public:
    Cell3DPosition gridSize; //!< The size of the 3D grid
    Vector3D gridScale; //!< The real size of a cell in the simulated world (Dimensions of a block)
    BuildingBlock **grid; //!< The grid as a 1-Dimensional array of BuildingBlock pointers
    
    Lattice();
    Lattice(Cell3DPosition &gsz, Vector3D &gsc);
    virtual ~Lattice();

    void insert(BuildingBlock* n, Cell3DPosition &p);
    void remove(Cell3DPosition &p);
    bool isInGrid(Cell3DPosition &p); 
    int getIndex(Cell3DPosition &p);
    bool isFree(Cell3DPosition &p);
    BuildingBlock *getBlock(Cell3DPosition &p);
    std::vector<Cell3DPosition> getNeighborCells(BuildingBlock *bb);
    std::vector<Cell3DPosition> getNeighborhood(BuildingBlock *bb);
    
    virtual Vector3D gridToWorldPosition(Cell3DPosition &pos) = 0;
    virtual Cell3DPosition worldToGridPosition(Vector3D &pos) = 0;    
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(Cell3DPosition &p) = 0;
};

/*! \brief 2-Dimensional Lattice abstract class
 * 
 */
class Lattice2D : public Lattice {

public:
    Lattice2D();
    Lattice2D(Cell3DPosition &gsz, Vector3D &gridscale);
    virtual ~Lattice2D();

    virtual Vector3D gridToWorldPosition(Cell3DPosition &pos) = 0;
    virtual Cell3DPosition worldToGridPosition(Vector3D &pos) = 0;
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(Cell3DPosition &p) = 0;
};

/*! \brief 3-Dimensional Lattice abstract class
 * 
 */
class Lattice3D : public Lattice {

public:
    Lattice3D();
    Lattice3D(Cell3DPosition &gsz, Vector3D &gridscale);
    virtual ~Lattice3D();

    virtual Vector3D gridToWorldPosition(Cell3DPosition &pos) = 0;
    virtual Cell3DPosition worldToGridPosition(Vector3D &pos) = 0;
    virtual std::vector<Cell3DPosition> getRelativeConnectivity(Cell3DPosition &p) = 0;
};

/*! \brief Square 2D Lattice
 * 
 * Used by SmartBlocks
 * 
 */
class SLattice : public Lattice2D {
    vector<Cell3DPosition> nCells{
        Cell3DPosition(1,0,0),
            Cell3DPosition(-1,0,0),
            Cell3DPosition(0,1,0),
            Cell3DPosition(0,-1,0),
            }; //!< Vector containing relative position of neighboring cells
public:
    SLattice();
    SLattice(Cell3DPosition &gsz, Vector3D &gridscale);
    ~SLattice();

    Vector3D gridToWorldPosition(Cell3DPosition &pos);
    Cell3DPosition worldToGridPosition(Vector3D &pos);
    std::vector<Cell3DPosition> getRelativeConnectivity(Cell3DPosition &p);
};

/*! \brief Hexagonal 2D Lattice
 * 
 * Used by Catoms2D blocks. Be careful, the 2 dimensions are **x and z**.
 * 
 */
class HLattice : public Lattice2D {
    vector<Cell3DPosition> nCellsEven{
        Cell3DPosition(1,0,0),
            Cell3DPosition(-1,0,0),
            Cell3DPosition(0,0,1),
            Cell3DPosition(0,0,-1),
            Cell3DPosition(1,0,1),
            Cell3DPosition(1,0,-1)
            }; //!< Vector containing relative position of neighboring cells for even(z) cells    
    vector<Cell3DPosition> nCellsOdd{
        Cell3DPosition(1,0,0),
            Cell3DPosition(-1,0,0),
            Cell3DPosition(0,0,1),
            Cell3DPosition(0,0,-1),
            Cell3DPosition(-1,0,1),
            Cell3DPosition(-1,0,-1)
            }; //!< Vector containing relative position of neighboring cells for odd(z) cells
public:
    HLattice();
    HLattice(Cell3DPosition &gsz, Vector3D &gridscale);
    ~HLattice();

    Vector3D gridToWorldPosition(Cell3DPosition &pos);
    Cell3DPosition worldToGridPosition(Vector3D &pos);
    std::vector<Cell3DPosition> getRelativeConnectivity(Cell3DPosition &p);
};

/*! \brief 3D Face-Centered Cubic Lattice
 * 
 * Used by Catoms3D
 * 
 */
class FCCLattice : public Lattice3D {
    vector<Cell3DPosition> nCellsEven{
        // z + 1
        Cell3DPosition(0,0,1),
            Cell3DPosition(0,1,1),
            Cell3DPosition(1,0,1),
            Cell3DPosition(1,1,1),
            // z
            Cell3DPosition(-1,0,0),
            Cell3DPosition(1,0,0),
            Cell3DPosition(0,-1,0),
            Cell3DPosition(0,1,0),
            // z - 1
            Cell3DPosition(0,0,-1),
            Cell3DPosition(0,1,-1),
            Cell3DPosition(1,0,-1),
            Cell3DPosition(1,1,-1)
            }; //!< Vector containing relative position of neighboring cells for even(z) cells;

    vector<Cell3DPosition> nCellsOdd{
        // z + 1
        Cell3DPosition(-1,-1,1),
            Cell3DPosition(-1,0,1),
            Cell3DPosition(0,0,1),
            Cell3DPosition(0,-1,1),
            // z
            Cell3DPosition(-1,0,0),
            Cell3DPosition(1,0,0),
            Cell3DPosition(0,-1,0),
            Cell3DPosition(0,1,0),
            // z - 1
            Cell3DPosition(-1,-1,-1),
            Cell3DPosition(-1,0,-1),
            Cell3DPosition(0,0,-1),
            Cell3DPosition(0,-1,-1)
            }; //!< Vector containing relative position of neighboring cells for odd(z) cells;
public:
    FCCLattice();
    FCCLattice(Cell3DPosition &gsz, Vector3D &gridscale);
    ~FCCLattice();

    Vector3D gridToWorldPosition(Cell3DPosition &pos);
    Cell3DPosition worldToGridPosition(Vector3D &pos);
    std::vector<Cell3DPosition> getRelativeConnectivity(Cell3DPosition &p);
};

/*! \brief 3D Simple Cubic Lattice
 * 
 * Used by BlinkyBlocks and RobotBlocks
 * 
 */
class SCLattice : public Lattice3D {
    vector<Cell3DPosition> nCells{
        Cell3DPosition(1,0,0),
            Cell3DPosition(-1,0,0),
            Cell3DPosition(0,0,1),
            Cell3DPosition(0,0,-1),
            Cell3DPosition(0,1,0),
            Cell3DPosition(0,-1,0)
            }; //!< Vector containing relative position of neighboring cells 
public:
    SCLattice();
    SCLattice(Cell3DPosition &gsz, Vector3D &gridscale);
    ~SCLattice();

    Vector3D gridToWorldPosition(Cell3DPosition &pos);
    Cell3DPosition worldToGridPosition(Vector3D &pos);
    std::vector<Cell3DPosition> getRelativeConnectivity(Cell3DPosition &p);
};

}

#endif
