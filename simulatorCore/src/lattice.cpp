
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
  vector<Cell3DPosition> activeNeighborCells;
  
  for (Cell3DPosition p : getNeighborhood(bb)) { // Check if each neighbor cell has an active node on it
    if (!isFree(p)) {
	activeNeighborCells.push_back(p);         // Add its position to the result
    }
  }
  
  return activeNeighborCells;
}

vector<Cell3DPosition> Lattice::getNeighborhood(BuildingBlock *bb) {
  vector<Cell3DPosition> neighborhood;
  vector<Cell3DPosition> relativeNCells =
      getRelativeConnectivity(bb->position); 

  for (Cell3DPosition p : relativeNCells) { // Check if each neighbor cell is in grid
    Cell3DPosition v = bb->position + p;
    if (isInGrid(v)) {
	neighborhood.push_back(v);         // Add its position to the result
    }
  }
  
  return neighborhood;
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

Vector3D HLattice::gridToWorldPosition(Cell3DPosition &pos) {
    Vector3D res;
    
    res.pt[2] = M_SQRT3_2 * pos[2] * gridScale[2];
    res.pt[1] = gridScale[1] / 2.0; // Catoms are centered on grid cells
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

Cell3DPosition HLattice::worldToGridPosition(Vector3D &pos) {
    Cell3DPosition res;

    res.pt[2] = round(pos[2] / (M_SQRT3_2 * gridScale[2]));
    res.pt[1] = 0;              // grid is 2D (x,z)
    res.pt[0] = (int)(pos[0] / (gridScale[0] - ((int)res.pt[2] % 2) * 0.5));

    /*
      cout << "------------computation worldToGridPosition--------------" << endl;
      cout << pos << endl;
      cout << res << endl;
      cout << "---------------------------------------------------------" << endl;*/

    return res;  
}

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

Vector3D SLattice::gridToWorldPosition(Cell3DPosition &pos) {    
    return Vector3D(pos[0] * gridScale[0],
                    pos[1] * gridScale[1],
                    0);
}

Cell3DPosition SLattice::worldToGridPosition(Vector3D &pos) {
    return Cell3DPosition(pos[0] / gridScale[0],
                          pos[1] / gridScale[1],
                          0);
}

/********************* FCCLattice *********************/
FCCLattice::FCCLattice() : Lattice3D() {}
FCCLattice::FCCLattice(Cell3DPosition &gsz, Vector3D &gsc) : Lattice3D(gsz,gsc) {}
FCCLattice::~FCCLattice() {}

vector<Cell3DPosition> FCCLattice::getRelativeConnectivity(Cell3DPosition &p) {
    return IS_EVEN(p[2]) ? nCellsEven : nCellsOdd;
}


Vector3D FCCLattice::gridToWorldPosition(Cell3DPosition &pos) {
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

Cell3DPosition FCCLattice::worldToGridPosition(Vector3D &pos) {
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
SCLattice::SCLattice(Cell3DPosition &gsz, Vector3D &gsc) : Lattice3D(gsz,gsc) {}
SCLattice::~SCLattice() {}

vector<Cell3DPosition> SCLattice::getRelativeConnectivity(Cell3DPosition &p) {
    return nCells;
}

Vector3D SCLattice::gridToWorldPosition(Cell3DPosition &pos) {    
    return Vector3D(pos[0] * gridScale[0],
                    pos[1] * gridScale[1],
                    pos[2] * gridScale[2]);
}

Cell3DPosition SCLattice::worldToGridPosition(Vector3D &pos) {
    return Cell3DPosition(pos[0] / gridScale[0],
                          pos[1] / gridScale[1],
                          pos[2] / gridScale[2]);
}
