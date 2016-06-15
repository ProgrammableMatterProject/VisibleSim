
#include "lattice.h"
#include "utils.h"

using namespace BaseSimulator;
using namespace utils;
using namespace std;

/********************* Lattice *********************/

Lattice::Lattice() {
    grid = NULL;
}

Lattice::Lattice(Cell3DPosition &gsz, Vector3D &gsc) {
    gridSize = gsz;
    gridScale = gsc;
    
    if (gsz[0] <= 0 || gsz[1] <= 0 || gsz[2] <= 0) {
        cerr << "error: Incorrect lattice size: size in any direction cannot be negative or null)" << endl;
    }
    
    grid = new BuildingBlock*[gridSize[0] * gridSize[1] * gridSize[2]]{NULL};
}

Lattice::~Lattice() {
    delete []grid;
}

int Lattice::getIndex(Cell3DPosition &p) {
    int index = p[0] + (p[1] + p[2] * p[1]) * p[0];
    //cout << "index: " << index << "(/total = " << size.x*size.y*size.z << ")" << endl;
    return index;
}

void Lattice::insert(BuildingBlock* bb, Cell3DPosition &p) {
    grid[getIndex(p)] = bb;
}

void Lattice::remove(Cell3DPosition &p) {
    grid[getIndex(p)] = NULL;
}

BuildingBlock* Lattice::getBlock(Cell3DPosition &p) {
    return grid[getIndex(p)];
}

bool Lattice::isFree(Cell3DPosition &p) {
  return (getBlock(p) == NULL);
}

bool Lattice::isInGrid(Cell3DPosition &p) {
    return isInRange(p[0], 0, gridSize[0] - 1)
        && isInRange(p[1], 0, gridSize[1] - 1)
        && isInRange(p[2], 0, gridSize[2] - 1);
}

vector<Cell3DPosition> Lattice::getNeighborCells(BuildingBlock *bb) {
  vector<Cell3DPosition> neighborPos;
  vector<Cell3DPosition> relativeNCells =
      getRelativeConnectivity(bb->position); 

  for (Cell3DPosition p : relativeNCells) { // Check if each neighbor cell has an active node on it
    Cell3DPosition v = bb->position + p;
    if (isInGrid(v)) {
	neighborPos.push_back(v);         // Add its position to the result
    }
  }
  
  return neighborPos;
}

/********************* Lattice2D *********************/
Lattice2D::Lattice2D() : Lattice() {}
Lattice2D::Lattice2D(Cell3DPosition &gsz, Vector3D &gsc) : Lattice(gsz,gsc) {}
Lattice2D::~Lattice2D() {}

/********************* Lattice3D *********************/
Lattice3D::Lattice3D() : Lattice() {}
Lattice3D::Lattice3D(Cell3DPosition &gsz, Vector3D &gsc) : Lattice(gsz,gsc) {}
Lattice3D::~Lattice3D() {}

/********************* HLattice *********************/
HLattice::HLattice() : Lattice2D() {}
HLattice::HLattice(Cell3DPosition &gsz, Vector3D &gsc) : Lattice2D(gsz,gsc) {}
HLattice::~HLattice() {}

vector<Cell3DPosition> HLattice::getRelativeConnectivity(Cell3DPosition &p) {
    return IS_EVEN(p[2]) ? nCellsEven : nCellsOdd;
}

/********************* SLattice *********************/
SLattice::SLattice() : Lattice2D() {}
SLattice::SLattice(Cell3DPosition &gsz, Vector3D &gsc) : Lattice2D(gsz,gsc) {}
SLattice::~SLattice() {}

vector<Cell3DPosition> SLattice::getRelativeConnectivity(Cell3DPosition &p) {
    return nCells;
}

/********************* FCCLattice *********************/
FCCLattice::FCCLattice() : Lattice3D() {}
FCCLattice::FCCLattice(Cell3DPosition &gsz, Vector3D &gsc) : Lattice3D(gsz,gsc) {}
FCCLattice::~FCCLattice() {}

vector<Cell3DPosition> FCCLattice::getRelativeConnectivity(Cell3DPosition &p) {
    return IS_EVEN(p[2]) ? nCellsEven : nCellsOdd;
}

/********************* SCLattice *********************/
SCLattice::SCLattice() : Lattice3D() {}
SCLattice::SCLattice(Cell3DPosition &gsz, Vector3D &gsc) : Lattice3D(gsz,gsc) {}
SCLattice::~SCLattice() {}

vector<Cell3DPosition> SCLattice::getRelativeConnectivity(Cell3DPosition &p) {
    return nCells;
}

