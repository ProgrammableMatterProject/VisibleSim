
#include "lattice.h"
#include "utils.h"
#include "trace.h"

using namespace BaseSimulator;
using namespace utils;
using namespace std;

// #define LATTICE_LOG 1

/********************* Lattice *********************/

Lattice::Lattice() {
    grid = NULL;
}

Lattice::Lattice(const Cell3DPosition &gsz, const Vector3D &gsc) {
    gridSize = gsz;
    gridScale = gsc;

    if (gsz[0] <= 0 || gsz[1] <= 0 || gsz[2] <= 0) {
        cerr << "error: Incorrect lattice size: size in any direction cannot be negative or null)" << endl;
    }

    grid = new BuildingBlock*[gridSize[0] * gridSize[1] * gridSize[2]]{NULL};

#ifdef LATTICE_LOG
    cerr << "l.new(gridSize = " << gridSize << ", gridScale = " << gridScale << ")" << endl;
#endif
}

Lattice::~Lattice() {
    delete []grid;
}

int Lattice::getIndex(const Cell3DPosition &p) {
    int index = p[0] + (p[1] + p[2] * gridSize[1]) * gridSize[0];
#ifdef LATTICE_LOG
    // cerr << "index: " << index << "(/total = " << gridSize[0]*gridSize[1]*gridSize[2] << ")" << endl;
#endif
    return index;
}

void Lattice::insert(BuildingBlock* bb, const Cell3DPosition &p) {
    int index = getIndex(p);

    if (!grid[index]) {
        grid[index] = bb;
    } else {
        cerr << "error: trying to add block of id " << bb->blockId << " on non-empty cell " << p << endl;
        throw new InvalidInsertion;
        // exit(EXIT_FAILURE);
    }

#ifdef LATTICE_LOG
    cerr << "l.insert(" << bb->blockId << ") on " << p << " = i:" << getIndex(p) << endl;
#endif
}

void Lattice::remove(const Cell3DPosition &p) {
    grid[getIndex(p)] = NULL;
}

BuildingBlock* Lattice::getBlock(const Cell3DPosition &p) {
    return isInGrid(p) ? grid[getIndex(p)] : NULL;
}

bool Lattice::isFree(const Cell3DPosition &p) {
    if (!isInGrid(p))
        return false;
    else
        return (getBlock(p) == NULL);
}

bool Lattice::cellHasBlock(const Cell3DPosition &p) {
    if (!isInGrid(p)) {
        return false;
    } else {
        return (getBlock(p) != NULL);
    }
}

bool Lattice::isInGrid(const Cell3DPosition &p) {
    return isInRange(p[0], 0, gridSize[0] - 1)
        && isInRange(p[1], 0, gridSize[1] - 1)
        && isInRange(p[2], 0, gridSize[2] - 1);
}

vector<Cell3DPosition> Lattice::getActiveNeighborCells(const Cell3DPosition &pos) {
    vector<Cell3DPosition> activeNeighborCells;

    for (Cell3DPosition p : getNeighborhood(pos)) { // Check if each neighbor cell has an active node on it
        if (!isFree(p)) {
            activeNeighborCells.push_back(p);         // Add its position to the result
        }
    }

    return activeNeighborCells;
}

vector<Cell3DPosition> Lattice::getNeighborhood(const Cell3DPosition &pos) {
    vector<Cell3DPosition> neighborhood;
    vector<Cell3DPosition> relativeNCells =
        getRelativeConnectivity(pos);

    for (Cell3DPosition p : relativeNCells) { // Check if each neighbor cell is in grid
        Cell3DPosition v = pos + p;
        if (isInGrid(v)) {
            neighborhood.push_back(v);         // Add its position to the result
        }
    }

    return neighborhood;
}

/********************* Lattice2D *********************/
Lattice2D::Lattice2D() : Lattice() {}
Lattice2D::Lattice2D(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice(gsz,gsc) {}
Lattice2D::~Lattice2D() {}

/********************* Lattice3D *********************/
Lattice3D::Lattice3D() : Lattice() {}
Lattice3D::Lattice3D(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice(gsz,gsc) {}
Lattice3D::~Lattice3D() {}

/********************* HLattice *********************/
HLattice::HLattice() : Lattice2D() {}
HLattice::HLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice2D(gsz,gsc) {}
HLattice::~HLattice() {}


// If new blocks are added with different shapes for the same lattice, we may consider
//  using a different function again for each block, or at least different sub operations
Vector3D HLattice::gridToWorldPosition(const Cell3DPosition &pos) {
    Vector3D res;

    res.pt[2] = M_SQRT3_2 * pos[2] * gridScale[2];
    res.pt[1] = 2.5;//-gridScale[1] / 2.0; // Catoms are centered on grid cells
    res.pt[0] = (pos[0] + ((int)(pos[2] + 0.01) % 2) * 0.5) * gridScale[0]; // +0.01 because of round problem

/*cout << "------------computation gridToWorldPosition--------------" << endl;
  cout << pos << endl;
  cout << ((int)pos[2]%2)*0.5 << endl;
  cout << (int)pos[2]%2 << endl;
  cout << pos[2] << endl;
  cout << (int)(pos[2]+0.01) << endl;
  cout << res << endl;
  cout << "---------------------------------------------------------" << endl;*/

    return res;
}

Cell3DPosition HLattice::worldToGridPosition(const Vector3D &pos) {
    Cell3DPosition res;

    res.pt[2] = round(pos[2] / (M_SQRT3_2 * gridScale[2]));
    res.pt[1] = 0;              // grid is 2D (x,z)
    res.pt[0] = (short)((pos[0] / gridScale[0]) - ((int)res.pt[2] % 2) * 0.5);

    /*
      cout << "------------computation worldToGridPosition--------------" << endl;
      cout << pos << endl;
      cout << res << endl;
      cout << "---------------------------------------------------------" << endl;*/

    return res;
}

vector<Cell3DPosition> HLattice::getRelativeConnectivity(const Cell3DPosition &p) {
    return IS_EVEN(p[2]) ? nCellsEven : nCellsOdd;
}

/********************* SLattice *********************/
SLattice::SLattice() : Lattice2D() {}
SLattice::SLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice2D(gsz,gsc) {}
SLattice::~SLattice() {}

vector<Cell3DPosition> SLattice::getRelativeConnectivity(const Cell3DPosition &p) {
    return nCells;
}

Vector3D SLattice::gridToWorldPosition(const Cell3DPosition &pos) {
    return Vector3D(pos[0] * gridScale[0],
                    pos[1] * gridScale[1],
                    0);
}

Cell3DPosition SLattice::worldToGridPosition(const Vector3D &pos) {
    return Cell3DPosition(pos[0] / gridScale[0],
                          pos[1] / gridScale[1],
                          0);
}

/********************* FCCLattice *********************/
FCCLattice::FCCLattice() : Lattice3D() {}
FCCLattice::FCCLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice3D(gsz,gsc) {}
FCCLattice::~FCCLattice() {}

vector<Cell3DPosition> FCCLattice::getRelativeConnectivity(const Cell3DPosition &p) {
    return IS_EVEN(p[2]) ? nCellsEven : nCellsOdd;
}


Vector3D FCCLattice::gridToWorldPosition(const Cell3DPosition &pos) {
    Vector3D res;

    res.pt[3] = 1.0;
    res.pt[2] = M_SQRT2_2 * (pos[2] + 0.5) * gridScale[2];
    if (IS_EVEN(pos[2])) {
        res.pt[1] = (pos[1] + 0.5) * gridScale[1];
        res.pt[0] = (pos[0] + 0.5) * gridScale[0];
    } else {
        res.pt[1] = (pos[1] + 1.0) * gridScale[1];
        res.pt[0] = (pos[0] + 1.0) * gridScale[0];
    }

//    OUTPUT << "world :"<< res << endl;

    return res;
}

Cell3DPosition FCCLattice::worldToGridPosition(const Vector3D &pos) {
    Cell3DPosition res;
    static const double round=0.05;
    double v;

    res.pt[2] = short(pos[2] / (M_SQRT2_2 * gridScale[2]) - 0.5 + round);

    if (IS_EVEN(res[2])) {
        v = (pos[0] - gridScale[0]) / gridScale[0] + 0.5;
        res.pt[0] = v < 0 ? short(v - round) : short(v + round);

        v = (pos[1] - gridScale[1]) / gridScale[1] + 0.5;
        res.pt[1] = v < 0 ? short(v - round) : short(v + round);
    } else {
        v = (pos[0] - gridScale[0]) / gridScale[0];
        res.pt[0] = v < 0 ? short(v - round) : short(v + round);
        v = (pos[1] - gridScale[1]) / gridScale[1];
        res.pt[1] = v < 0 ? short(v - round) : short(v + round);
    }

    return res;
}

/********************* SCLattice *********************/
SCLattice::SCLattice() : Lattice3D() {}
SCLattice::SCLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice3D(gsz,gsc) {}
SCLattice::~SCLattice() {}

vector<Cell3DPosition> SCLattice::getRelativeConnectivity(const Cell3DPosition &p) {
    return nCells;
}

Vector3D SCLattice::gridToWorldPosition(const Cell3DPosition &pos) {
    return Vector3D(pos[0] * gridScale[0],
                    pos[1] * gridScale[1],
                    pos[2] * gridScale[2]);
}

Cell3DPosition SCLattice::worldToGridPosition(const Vector3D &pos) {
    return Cell3DPosition(pos[0] / gridScale[0],
                          pos[1] / gridScale[1],
                          pos[2] / gridScale[2]);
}

// std::vector<int> SCLattice::getActiveNeighborDirections(BuildingBlock *bb) {
//     vector<int> neighborDirections;
//     vector<Cell3DPosition> relativeNCells =
//         getRelativeConnectivity(bb->position);

//     // Check if each neighbor cell is in grid and if it is, determine the corresponding direction
//     for (Cell3DPosition p : relativeNCells) {
//         Cell3DPosition v = bb->position + p;
//         if (isInGrid(v) && !isFree(v)) {
//             neighborDirections.push_back(NeighborDirection::determineDirection(p));
//         }
//     }

//     return neighborDirections;
// }
