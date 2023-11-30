#include <climits>

#include "lattice.h"
#include "../utils/utils.h"
#include "../utils/trace.h"

using namespace BaseSimulator;
using namespace utils;
using namespace std;

// #define LATTICE_LOG 1

/********************* Lattice *********************/

const string Lattice::directionName[] = {"unknown"};

Lattice::Lattice() {
    grid = nullptr;
}

Lattice::Lattice(const Cell3DPosition &gsz, const Vector3D &gsc) {
    gridSize = gsz;
    gridScale = gsc;

    if (gsz[0] <= 0 || gsz[1] <= 0 || gsz[2] <= 0) {
        cerr << "error: Incorrect lattice size: size in any direction cannot be negative or nullptr" << endl;
        throw InvalidDimensionsException(gsz);
    }

    grid = new BuildingBlock*[gridSize[0] * gridSize[1] * gridSize[2]];
    // Initializes grid to nullptr
    BuildingBlock **ptr = grid;
    int i=gridSize[0] * gridSize[1] * gridSize[2];
    while (i--) {
        *ptr=nullptr;
        ptr++;
    }

#ifdef LATTICE_LOG
    cerr << "l.new(gridSize = " << gridSize << ", gridScale = " << gridScale << ")" << endl;
#endif
}

Lattice::~Lattice() {
    delete [] grid;
}

unsigned int Lattice::getIndex(const Cell3DPosition &p) const {
    unsigned int index = p[0] + (p[1] + p[2] * gridSize[1]) * gridSize[0];

    static const unsigned int GRID_MAX_INDEX = gridSize[2] * gridSize[1] * gridSize[0];
    assert(index < GRID_MAX_INDEX);

#ifdef LATTICE_LOG
    // cerr << "index: " << index << "(/total = " << gridSize[0]*gridSize[1]*gridSize[2] << ")" << endl;
#endif
    return index;
}

Cell3DPosition Lattice::getGridLowerBounds(int z) const {
    return Cell3DPosition(0,0,0);
}

Cell3DPosition Lattice::getGridUpperBounds(int z) const {
    return gridSize - Cell3DPosition(1,1,1);
}


void Lattice::insert(BuildingBlock* bb, const Cell3DPosition &p, bool count) {
    // try {
    int index = getIndex(p);
    if (not isInGrid(p))
        throw OutOfLatticeInsertionException(p);
    else if (not isFree(p))
        throw DoubleInsertionException(p);
    else {
        grid[index] = bb;
        if (count) nbModules++;
    }
    // } catch (DoubleInsertionException const& e) {
    //     cerr << e.what();
    //     VS_ASSERT(false);//FIXME: should be handled by the user, but catch clauses in main are not catching the exceptions for some reason.
    // }

#ifdef LATTICE_LOG
    cerr << "l.insert(" << bb->blockId << ") on " << p << " = i:" << getIndex(p) << endl;
#endif
}

void Lattice::remove(const Cell3DPosition &p, bool count) {
    grid[getIndex(p)] = nullptr;
    if (count) nbModules--;
}

BuildingBlock* Lattice::getBlock(const Cell3DPosition &p) const {
    return isInGrid(p) ? grid[getIndex(p)] : nullptr;
}

bool Lattice::isFree(const Cell3DPosition &p) const {
    if (!isInGrid(p))
        return false;
    else
        return (getBlock(p) == nullptr);
}

bool Lattice::cellHasBlock(const Cell3DPosition &p) const {
    if (!isInGrid(p)) {
        return false;
    } else {
        return (getBlock(p) != nullptr);
    }
}

bool Lattice::isInGrid(const Cell3DPosition &p) const {
    const Cell3DPosition& lb = getGridLowerBounds(p[2]);
    const Cell3DPosition& ub = getGridUpperBounds(p[2]);

    return isInRange(p[0], lb.pt[0], ub.pt[0])
           && isInRange(p[1], lb.pt[1], ub.pt[1])
           && isInRange(p[2], lb.pt[2], ub.pt[2]);
}

unsigned int
Lattice::getCellDistance(const Cell3DPosition &p1, const Cell3DPosition &p2) const {
    throw NotImplementedException("distance function for current lattice type");
}

vector<Cell3DPosition> Lattice::getActiveNeighborCells(const Cell3DPosition &pos) const {
    vector<Cell3DPosition> activeNeighborCells;

    for (const Cell3DPosition &p : getNeighborhood(pos)) { // Check if each neighbor cell has an active node on it
        if (!isFree(p)) {
            activeNeighborCells.push_back(p);         // Add its position to the result
        }
    }

    return activeNeighborCells;
}

Vector3D Lattice::gridToWorldPosition(const Cell3DPosition &pos) const {
    Vector3D res = gridToUnscaledWorldPosition(pos).dot(gridScale);
    //OUTPUT << "gridToWorldPosition" << pos << " -> " << res << endl;
    return res;
}

vector<Cell3DPosition> Lattice::getFreeNeighborCells(const Cell3DPosition &pos) const {
    vector<Cell3DPosition> freeNeighborCells;
    for (auto &p : getNeighborhood(pos)) { // Check if each neighbor cell has an active node on it
        if (isFree(p)) {
            freeNeighborCells.push_back(p);         // Add its position to the result
        }
    }

    const vector<Cell3DPosition> neighborhoodAway={{-1,-1,0},{-1,1,0},{1,-1,0},{1,1,0},{0,0,2},{0,0,-2}};
    for (auto &p : neighborhoodAway) { // Check if each neighbor cell has an active node on it
        if (isFree(p+pos)) {
            freeNeighborCells.push_back(p+pos);         // Add its position to the result
        }
    }
    return freeNeighborCells;
}

vector<Cell3DPosition> Lattice::getNeighborhood(const Cell3DPosition &pos) const {
    vector<Cell3DPosition> neighborhood;
    const vector<Cell3DPosition> &relativeNCells =
            getRelativeConnectivity(pos);

    for (const Cell3DPosition &p : relativeNCells) { // Check if each neighbor cell is in grid
        Cell3DPosition v = pos + p;
        if (isInGrid(v)) {
            neighborhood.push_back(v);         // Add its position to the result
        }
    }

    return neighborhood;
}

short Lattice::getDirection(const Cell3DPosition &p, const Cell3DPosition &neighbor) const {
    auto neighborhood = getNeighborhood(p);
    for (unsigned short i = 0; i < neighborhood.size(); i++) {
        if (neighbor == neighborhood[i]) return i;
    }

    return -1;
}

bool Lattice::cellsAreAdjacent(const Cell3DPosition &p1, const Cell3DPosition &p2) const {
    for (const Cell3DPosition pos : getNeighborhood(p1))
        if (p2 == pos) return true;

    return false;
}


string Lattice::getDirectionString(short d) const {
    return isInRange(d, 0, this->getMaxNumNeighbors() - 1) ?
           directionName[d] : "undefined";
}

void Lattice::highlightCell(const Cell3DPosition& pos, const Color &color) {
    if (mapHighlightedCells.find(pos) != mapHighlightedCells.end())
        mapHighlightedCells[pos] = color;
    else
        mapHighlightedCells.insert(make_pair(pos, color));
}

void Lattice::unhighlightCell(const Cell3DPosition& pos) {
    mapHighlightedCells.erase(pos);
}

void Lattice::resetCellHighlights() {
    mapHighlightedCells.clear();
}

void Lattice::highlightAllCellsThatVerify(std::function<bool(const Cell3DPosition&)> predicate,
                                          Color color) {
    Cell3DPosition pos;
    for (short iz = 0; iz <= getGridUpperBounds()[2]; iz++){
        const Cell3DPosition& glb = getGridLowerBounds(iz);
        const Cell3DPosition& ulb = getGridUpperBounds(iz);
        for (short iy = glb[1]; iy <= ulb[1]; iy++) {
            for (short ix = glb[0]; ix <= ulb[0]; ix++) {
                pos.set(ix,iy,iz);

                if (predicate(pos))
                    highlightCell(pos, color);
            }
        }
    }
}

void Lattice::
unhighlightAllCellsThatVerify(std::function<bool(const Cell3DPosition&)> predicate) {
    Cell3DPosition pos;
    for (short iz = 0; iz <= getGridUpperBounds()[2]; iz++){
        const Cell3DPosition& glb = getGridLowerBounds(iz);
        const Cell3DPosition& ulb = getGridUpperBounds(iz);
        for (short iy = glb[1]; iy <= ulb[1]; iy++) {
            for (short ix = glb[0]; ix <= ulb[0]; ix++) {
                pos.set(ix,iy,iz);

                if (predicate(pos))
                    unhighlightCell(pos);
            }
        }
    }
}

Cell3DPosition Lattice::getOppositeCell(const Cell3DPosition& pRef,
                                        const Cell3DPosition& pDir) const {
    short d = getDirection(pRef, pDir);
    return getOppositeCell(pRef, d);
}

Cell3DPosition Lattice::getOppositeCell(const Cell3DPosition& pRef, short d) const {
    short oppD = getOppositeDirection(d);
    return getCellInDirection(pRef, oppD);
}

/********************* FreeLattice *********************/
FreeLattice::FreeLattice() : Lattice3D() {}
FreeLattice::FreeLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice3D(gsz,gsc) {}
FreeLattice::~FreeLattice()=default;

const string FreeLattice::directionName[] = {"North","East","South","West","Top","Bottom"};

vector<Cell3DPosition> FreeLattice::getRelativeConnectivity(const Cell3DPosition &p) const {
    return nCells;
}

Vector3D FreeLattice::gridToUnscaledWorldPosition_base(const Cell3DPosition &pos) {
    return Vector3D(pos[0], pos[1], pos[2], 0);
}

Vector3D FreeLattice::gridToUnscaledWorldPosition(const Cell3DPosition &pos) const {
    return gridToUnscaledWorldPosition_base(pos);
}

Cell3DPosition FreeLattice::unscaledWorldToGridPosition(const Vector3D &pos) const {
    return Cell3DPosition(pos[0], pos[1], pos[2]);
}

Cell3DPosition FreeLattice::worldToGridPosition(const Vector3D &pos) const {
    return Cell3DPosition(pos[0] / gridScale[0],
                          pos[1] / gridScale[1],
                          pos[2] / gridScale[2]);
}

Cell3DPosition FreeLattice::getCellInDirection(const Cell3DPosition &pRef, int direction) const {
    return pRef + nCells[direction];
}

short FreeLattice::getOppositeDirection(short d) const {
    switch(d) {
        case North :
            return South;
            break;
        case East :
            return West;
            break;
        case South :
            return North;
            break;
        case West :
            return East;
            break;
        case Top :
            return Bottom;
            break;
        case Bottom :
            return Top;
            break;
        default:
            ERRPUT << "*** ERROR *** : unknown face: " << d << endl;

            return -1;
            break;
    }
}

string FreeLattice::getDirectionString(short d) const {
    return isInRange(d, 0, this->getMaxNumNeighbors() - 1) ?
           directionName[d] : "undefined";

}



/********************* Lattice2D *********************/
Lattice2D::Lattice2D() : Lattice() {}
Lattice2D::Lattice2D(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice(gsz,gsc) {}
Lattice2D::~Lattice2D() {}
const string Lattice2D::directionName[] = {"unknown"};
/********************* Lattice3D *********************/
Lattice3D::Lattice3D() : Lattice() {}
Lattice3D::Lattice3D(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice(gsz,gsc) {}
Lattice3D::~Lattice3D() {}
const string Lattice3D::directionName[] = {"unknown"};
/********************* HLattice *********************/
HLattice::HLattice() : Lattice2D() {}
HLattice::HLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice2D(gsz,gsc) {}
HLattice::~HLattice() {}

Vector3D HLattice::gridToUnscaledWorldPosition_base(const Cell3DPosition &pos) {
    Vector3D res;

    /*res.pt[2] = M_SQRT3_2 * pos[2];
    // Dividing by gridScale might pose problem for CSG
    res.pt[1] = 2.5;//-gridScale[1] / 2.0; // Catoms are centered on grid cells
    res.pt[0] = (pos[0] + ((int)(pos[2] + 0.01) % 2) * 0.5); // +0.01 because of round problem
    */
    res.set((pos[0] + ((int)(pos[2] + 0.01f) % 2) * 0.5f),2.5,M_SQRT3_2 * pos[2]);

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

Vector3D HLattice::gridToUnscaledWorldPosition(const Cell3DPosition &pos) const {
    return gridToUnscaledWorldPosition_base(pos);
}

Vector3D HLattice::gridToWorldPosition(const Cell3DPosition &pos) const {
    return gridToUnscaledWorldPosition(pos).dot(Vector3D(gridScale[0], 1, 1));
}

Cell3DPosition HLattice::unscaledWorldToGridPosition(const Vector3D &pos) const {
    Cell3DPosition res;

    res.pt[2] = round(pos[2] / M_SQRT3_2);
    res.pt[1] = 0;              // grid is 2D (x,z)
    res.pt[0] = round((pos[0] - ((int)res.pt[2] % 2) * 0.5));

    return res;
}

Cell3DPosition HLattice::worldToGridPosition(const Vector3D &pos) const {
    Cell3DPosition res;

    res.pt[2] = round(pos[2] / (M_SQRT3_2 * gridScale[2]));
    res.pt[1] = 0;              // grid is 2D (x,z)
    res.pt[0] = round((pos[0] / gridScale[0] - ((int)res.pt[2] % 2) * 0.5));

    /*
      cout << "------------computation worldToGridPosition--------------" << endl;
      cout << pos << endl;
      cout << res << endl;
      cout << "---------------------------------------------------------" << endl;
    */
    return res;
}

vector<Cell3DPosition> HLattice::getRelativeConnectivity(const Cell3DPosition &p) const {
    return IS_EVEN(p[2]) ? nCellsEven : nCellsOdd;
}

Cell3DPosition HLattice::getCellInDirection(const Cell3DPosition &pRef, int direction) const
{
    return pRef + getRelativeConnectivity(pRef)[direction];
}

/************************************************************
 *   HLattice::NeighborDirections
 ************************************************************/

const string HLattice::directionName[] = {"Right","TopRight","TopLeft",
                                          "Left","BottomLeft","BottomRight"};

short HLattice::getOppositeDirection(short d) const {
    switch (Direction(d)) {
        case BottomLeft:
            return TopRight;
            break;
        case Left:
            return Right;
            break;
        case TopLeft:
            return BottomRight;
            break;
        case BottomRight:
            return TopLeft;
            break;
        case Right:
            return Left;
            break;
        case TopRight:
            return BottomLeft;
            break;
        default:
            ERRPUT << "*** ERROR *** : unknown face: " << d << endl;
            return -1;
            break;
    }
}

string HLattice::getDirectionString(short d) const {
    return isInRange(d, 0, this->getMaxNumNeighbors() - 1) ?
           directionName[d] : "undefined";
}

/********************* HHLattice *********************/
HHLattice::HHLattice() : HLattice() {}
HHLattice::HHLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : HLattice(gsz,gsc) {}
HHLattice::~HHLattice() {}

Vector3D HHLattice::gridToUnscaledWorldPosition_base(const Cell3DPosition &pos) {
    Vector3D res;

    /*res.pt[2] = 0;
    res.pt[1] = pos[1]*M_SQRT3_2;
    res.pt[0] = pos[0]+0.5*pos[1];*/
    res.set(pos[0]+0.5f*pos[1],pos[1]*M_SQRT3_2,0);

    return res;
}

Vector3D HHLattice::gridToUnscaledWorldPosition(const Cell3DPosition &pos) const {
    return gridToUnscaledWorldPosition_base(pos);
}

Vector3D HHLattice::gridToWorldPosition(const Cell3DPosition &pos) const {
    return gridToUnscaledWorldPosition(pos).dot(Vector3D(gridScale[0], gridScale[1], 1));
}

Cell3DPosition HHLattice::unscaledWorldToGridPosition(const Vector3D &pos) const {
    Cell3DPosition res;

    res.pt[2] = 0;
    res.pt[1] = round(pos[1]/M_SQRT3_2);              // grid is 2D (x,y)
    res.pt[0] = round(pos[0] - res.pt[1]* 0.5);

    return res;
}

Cell3DPosition HHLattice::worldToGridPosition(const Vector3D &pos) const {
    Cell3DPosition res;

    res.pt[2] = 0;
    res.pt[1] = round(pos[1] / (M_SQRT3_2 * gridScale[1]));
    res.pt[0] = round(pos[0] / gridScale[0] - res.pt[1] * 0.5);
    return res;
}

vector<Cell3DPosition> HHLattice::getRelativeConnectivity(const Cell3DPosition &p) const {
    return nCells;
}

Cell3DPosition HHLattice::getCellInDirection(const Cell3DPosition &pRef, int direction) const {
    return pRef + getRelativeConnectivity(pRef)[direction];
}


/************************************************************
 *   HHLattice::NeighborDirections
 ************************************************************/

const string HHLattice::directionName[] = {"East","NorthEast","NorthWest","West","SouthWest","SouthEst"};

short HHLattice::getOppositeDirection(short d) const {
    return (d+3)%6;
}

string HHLattice::getDirectionString(short d) const {
    return isInRange(d, 0, this->getMaxNumNeighbors() - 1) ?
           directionName[d] : "undefined";

}


/********************* SLattice *********************/
SLattice::SLattice() : Lattice2D() {}
SLattice::SLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice2D(gsz,gsc) {}
SLattice::~SLattice() {}

vector<Cell3DPosition> SLattice::getRelativeConnectivity(const Cell3DPosition &p) const {
    return nCells;
}

Vector3D SLattice::gridToUnscaledWorldPosition_base(const Cell3DPosition &pos) {
    return Vector3D(pos[0], pos[1], 0, 0);
}

Vector3D SLattice::gridToUnscaledWorldPosition(const Cell3DPosition &pos) const {
    return gridToUnscaledWorldPosition_base(pos);
}

Cell3DPosition SLattice::unscaledWorldToGridPosition(const Vector3D &pos) const {
    return Cell3DPosition(pos[0], pos[1], 0);
}

Cell3DPosition SLattice::worldToGridPosition(const Vector3D &pos) const {
    return Cell3DPosition(pos[0] / gridScale[0],
                          pos[1] / gridScale[1],
                          0);
}

Cell3DPosition SLattice::getCellInDirection(const Cell3DPosition &pRef, int direction) const {
    return pRef + nCells[direction];
}

/************************************************************
 *   SLattice::NeighborDirections
 ************************************************************/

const string SLattice::directionName[] = {"North","East","South","West"};

short SLattice::getOppositeDirection(short d) const {
    switch(d) {
        case North :
            return South;
            break;
        case East :
            return West;
            break;
        case South :
            return North;
            break;
        case West :
            return East;
            break;
        default:
            ERRPUT << "*** ERROR *** : unknown face: " << d << endl;
            return -1;
            break;
    }
}

string SLattice::getDirectionString(short d) const {
    return isInRange(d, 0, this->getMaxNumNeighbors() - 1) ?
           directionName[d] : "undefined";

}


/********************* FCCLattice *********************/
FCCLattice::FCCLattice() : Lattice3D() {
}

FCCLattice::FCCLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice3D(gsz,gsc) {}


FCCLattice::~FCCLattice() {
}

vector<Cell3DPosition> FCCLattice::getRelativeConnectivity(const Cell3DPosition &p) const {
    return IS_EVEN(p[2]) ? nCellsEven : nCellsOdd;
}

Vector3D FCCLattice::gridToUnscaledWorldPosition_base(const Cell3DPosition &pos) {
    Vector3D res;

    res.set(3,1.0);
    res.set(2,M_SQRT2_2 * (pos[2] + 0.5));
    if (IS_EVEN(pos[2])) {
        res.set(1,pos[1] + 0.5);
        res.set(0,pos[0] + 0.5);
    } else {
        res.set(1,pos[1] + 1.0);
        res.set(0,pos[0] + 1.0);
    }

//    OUTPUT << "world :"<< res << endl;
    return res;
}

Vector3D FCCLattice::gridToUnscaledWorldPosition(const Cell3DPosition &pos) const {
    return gridToUnscaledWorldPosition_base(pos);
}

Cell3DPosition FCCLattice::unscaledWorldToGridPosition(const Vector3D &pos) const {
    Cell3DPosition res;
    static const double round=0.05;
    double v;

    res.pt[2] = short(floor(pos[2] / (M_SQRT2_2) - 0.5 + round));
    if (IS_EVEN(res[2])) {
        v = pos[0] + 0.5;
        res.pt[0] = v < 0 ? short(v - round) : short(v + round);

        v = pos[1] + 0.5;
        res.pt[1] = v < 0 ? short(v - round) : short(v + round);
    } else {
        v = pos[0];
        res.pt[0] = v < 0 ? short(v - round) : short(v + round);
        v = pos[1];
        res.pt[1] = v < 0 ? short(v - round) : short(v + round);
    }
    return res;
}

Cell3DPosition FCCLattice::worldToGridPosition(const Vector3D &pos) const {
    Cell3DPosition res;
    static const double round=0.05;
    double v;

    res.pt[2] = short(floor(pos[2] / (M_SQRT2_2 * gridScale[2]) - 0.5 + round));
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

/************************************************************
 *   FCCLattice::NeighborDirections
 ************************************************************/
const string FCCLattice::directionName[] =
        {
                "C0East", "C1North", "C2TopNE",
                "C3TopNW", "C4TopSW", "C5TopSE",
                "C6West", "C7South", "C8BottomSW",
                "C9BottomSE", "C10BottomNE", "C11BottomNW"
        };

short FCCLattice::getOppositeDirection(short d) const {
    switch (Direction(d)) {
        case C0East:	return C6West; break;
        case C1North:	return C7South; break;
        case C2TopNE:	return C8BottomSW; break;
        case C3TopNW:	return C9BottomSE; break;
        case C4TopSW:	return C10BottomNE; break;
        case C5TopSE:	return C11BottomNW; break;
        case C6West:	return C0East; break;
        case C7South:	return C1North; break;
        case C8BottomSW:	return C2TopNE; break;
        case C9BottomSE:	return C3TopNW; break;
        case C10BottomNE:	return C4TopSW; break;
        case C11BottomNW:	return C5TopSE; break;
        default:
            ERRPUT << "*** ERROR *** : unknown face: " << d << endl;
            return -1;
            break;
    }
}

string FCCLattice::getDirectionString(short d) const {
    return isInRange(d, 0, this->getMaxNumNeighbors() - 1) ?
           directionName[d] : "undefined";

}

Cell3DPosition FCCLattice::getCellInDirection(const Cell3DPosition &pRef, int direction) const
{
    return pRef + getRelativeConnectivity(pRef)[direction];
}

void FCCLattice::glDraw() const {
    static const float pts[24][3]={{2.928,0,4.996},{0,2.928,4.996},{-2.928,0,4.996},{0,-2.928,4.996},{4.996,2.069,2.069},{2.069,4.996,2.069},{-2.069,4.996,2.069},{-4.996,2.069,2.069},{-4.996,-2.069,2.069},{-2.069,-4.996,2.069},{2.069,-4.996,2.069},{4.996,-2.069,2.069},{4.996,2.069,-2.069},{2.069,4.996,-2.069},{-2.069,4.996,-2.069},{-4.996,2.069,-2.069},{-4.996,-2.069,-2.069},{-2.069,-4.996,-2.069},{2.069,-4.996,-2.069},{4.996,-2.069,-2.069},{2.928,0,-4.996},{0,2.928,-4.996},{-2.928,0,-4.996},{0,-2.928,-4.996}};
    static const uint8_t quads[72]={0,1,2,3,0,4,5,1,1,6,7,2,2,8,9,3,3,10,11,0,4,12,13,5,5,13,14,6,6,14,15,7,7,15,16,8,8,16,17,9,9,17,18,10,10,18,19,11,11,19,12,4,12,20,21,13,14,21,22,15,16,22,23,17,18,23,20,19,23,22,21,20};
    static const uint8_t tris[24]={1,5,6,2,7,8,3,9,10,0,11,4,13,21,14,15,22,16,17,23,18,19,23,12};

    if (!mapHighlightedCells.empty()) {
        Vector3D v;
        int i=72;
        const uint8_t *ptr;
        Color c;
        for (const auto& pair : mapHighlightedCells) {
            glPushMatrix();
            v = gridToWorldPosition(pair.first);
            glTranslatef(v[0],v[1],v[2]);
            /*c.set(pair.second.rgba[0],pair.second.rgba[1],pair.second.rgba[2],0.5f);
            glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE,c.rgba);*/
            pair.second.glMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
            glBegin(GL_QUADS);
            ptr = quads;
            i=18;
            while (i--) {
                glVertex3fv(pts[*ptr++]);
                glVertex3fv(pts[*ptr++]);
                glVertex3fv(pts[*ptr++]);
                glVertex3fv(pts[*ptr++]);
            }
            glEnd();
            glBegin(GL_TRIANGLES);
            ptr = tris;
            i=8;
            while (i--) {
                glVertex3fv(pts[*ptr++]);
                glVertex3fv(pts[*ptr++]);
                glVertex3fv(pts[*ptr++]);
            }
            glEnd();
            glPopMatrix();
        }

    }
}

// === NEIGHBOR RESTRICTIONS ===

void FCCLattice::setPlaneSides(BlockingPositionPlane plane,
                               const Cell3DPosition& pos,
                               Cell3DPosition& sideOne, Cell3DPosition& sideTwo,
                               int d, bool evenZ) const {
    switch(plane) {
        case BlockingPositionPlane::XY:
            sideOne = evenZ ? pos + sideOneEvenXY[d] : pos + sideOneOddXY[d];
            sideTwo = evenZ ? pos + sideTwoEvenXY[d] : pos + sideTwoOddXY[d];
            break;
        case BlockingPositionPlane::YZ:
            sideOne = evenZ ? pos + sideOneEvenYZ[d] : pos + sideOneOddYZ[d];
            sideTwo = evenZ ? pos + sideTwoEvenYZ[d] : pos + sideTwoOddYZ[d];
            break;
        case BlockingPositionPlane::XZ:
            sideOne = evenZ ? pos + sideOneEvenXZ[d] : pos + sideOneOddXZ[d];
            sideTwo = evenZ ? pos + sideTwoEvenXZ[d] : pos + sideTwoOddXZ[d];
            break;
    }
}

bool FCCLattice::isPositionUnblockedSide(const Cell3DPosition &pos) const {
    Cell3DPosition occupiedPosition = pos + xyPos[0];
    Cell3DPosition forbiddenPosition = pos + xyPos[1];

    if (cellHasBlock(occupiedPosition) && cellHasBlock(forbiddenPosition))
        return false;

    occupiedPosition = pos + xyPos[2];
    forbiddenPosition = pos + xyPos[3];

    return !(cellHasBlock(occupiedPosition) && cellHasBlock(forbiddenPosition));
}

bool FCCLattice::isPositionUnblockedSide(const Cell3DPosition &pos,
                                         const Cell3DPosition &ignore) const {
    Cell3DPosition occupiedPosition = pos + xyPos[0];
    Cell3DPosition forbiddenPosition = pos + xyPos[1];

    if ( (cellHasBlock(occupiedPosition) && (occupiedPosition != ignore))
         && (cellHasBlock(forbiddenPosition) && (forbiddenPosition != ignore)) )
        return false;

    occupiedPosition = pos + xyPos[2];
    forbiddenPosition = pos + xyPos[3];

    return !( (cellHasBlock(occupiedPosition) && (occupiedPosition != ignore))
              && (cellHasBlock(forbiddenPosition) && (forbiddenPosition != ignore)) );
}

bool FCCLattice::isPositionUnblocked(const Cell3DPosition &pos,
                                     BlockingPositionPlane plane) const {
    Cell3DPosition sideOne, sideTwo;
    bool isInSide1 = false, isInSide2 = false;
    bool evenZ = !(pos[2]%2);

    for (int i = 0; i < 4; i++) {
        setPlaneSides(plane, pos, sideOne, sideTwo, i, evenZ);

        if (cellHasBlock(sideOne)) isInSide1 = true;
        if (cellHasBlock(sideTwo)) isInSide2 = true;
    }

    return !(isInSide1 && isInSide2);
}

bool FCCLattice::isPositionUnblocked(const Cell3DPosition &pos,
                                     const Cell3DPosition &ignore,
                                     BlockingPositionPlane plane) const {
    Cell3DPosition sideOne, sideTwo;
    bool isInSide1 = false, isInSide2 = false;
    bool evenZ = !(pos[2]%2);

    for (int i = 0; i < 4; i++) {
        setPlaneSides(plane, pos, sideOne, sideTwo, i, evenZ);

        if (cellHasBlock(sideOne) && sideOne != ignore) isInSide1 = true;
        if (cellHasBlock(sideTwo) && sideTwo != ignore) isInSide2 = true;
    }

    return !(isInSide1 && isInSide2);
}

bool FCCLattice::isPositionBlocked(const Cell3DPosition &pos) const {
    return isPositionUnblockedSide(pos)
           && !(isPositionUnblocked(pos, BlockingPositionPlane::XY)
                || isPositionUnblocked(pos, BlockingPositionPlane::YZ)
                || isPositionUnblocked(pos, BlockingPositionPlane::XZ));
}


bool FCCLattice::isPositionBlocked(const Cell3DPosition &pos,
                                   const Cell3DPosition &ignore) const {
    return isPositionUnblockedSide(pos, ignore)
           && !(isPositionUnblocked(pos, ignore, BlockingPositionPlane::XY)
                || isPositionUnblocked(pos, ignore, BlockingPositionPlane::YZ)
                || isPositionUnblocked(pos, ignore, BlockingPositionPlane::XZ));
}

/********************* SkewFCCLattice *********************/
SkewFCCLattice::SkewFCCLattice() : FCCLattice() {}
SkewFCCLattice::SkewFCCLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : FCCLattice(gsz,gsc) {}
SkewFCCLattice::~SkewFCCLattice() {}

unsigned int SkewFCCLattice::getIndex(const Cell3DPosition &p) const {
    int index = (p[0] + p[2] / 2)
                + (p[1] + p[2] / 2 + p[2] * gridSize[1]) * gridSize[0];
#ifdef LATTICE_LOG
    // cerr << "index: " << index << "(/total = " << gridSize[0]*gridSize[1]*gridSize[2] << ")" << endl;
#endif
    return index;
}

Cell3DPosition SkewFCCLattice::getGridLowerBounds(int z) const {
    if (z == -1) return Cell3DPosition(-gridSize[2]/2,-gridSize[2]/2,gridSize[2]);

    return Cell3DPosition(-z/2,-z/2,z);
}

Cell3DPosition SkewFCCLattice::getGridUpperBounds(int z) const {
    return gridSize - Cell3DPosition(z/2, z/2, 0) - Cell3DPosition(1,1,1);
}

vector<Cell3DPosition> SkewFCCLattice::getRelativeConnectivity(const Cell3DPosition &p) const {
    return nCells;
}

Vector3D SkewFCCLattice::gridToUnscaledWorldPosition_base(const Cell3DPosition &pos) {
    Vector3D res(pos[0] + 0.5 + pos[2] / 2.0,
                 pos[1] + 0.5 + pos[2] / 2.0,
                 M_SQRT2_2 * (pos[2] + 0.5),
                 1.0);

    /*res.pt[3] = 1.0;
    res.pt[2] = M_SQRT2_2 * (pos[2] + 0.5);
    res.pt[1] = pos[1] + 0.5 + pos[2] / 2.0;
    res.pt[0] = pos[0] + 0.5 + pos[2] / 2.0;*/

//		OUTPUT << "gridToUnscaledWorldPosition" << pos << " -> " << res << endl;

    return res;
}


Vector3D SkewFCCLattice::gridToUnscaledWorldPosition(const Cell3DPosition &pos) const {
    return gridToUnscaledWorldPosition_base(pos);
}

Cell3DPosition SkewFCCLattice::unscaledWorldToGridPosition(const Vector3D &pos) const {
    Cell3DPosition res;

    res.pt[2] = round((2 * pos[2]) / (M_SQRT2) - 0.5);
    res.pt[1] = round(pos[1] - 0.5 - res.pt[2] / 2.0);
    res.pt[0] = round(pos[0] - 0.5 - res.pt[2] / 2.0);

    return res;
}

Cell3DPosition SkewFCCLattice::worldToGridPosition(const Vector3D &pos) const {
    Cell3DPosition res;

    res.pt[2] = round((2 * pos[2]) / (M_SQRT2 * gridScale[2]) - 0.5);
    res.pt[1] = round(pos[1] / gridScale[1] - 0.5 - res.pt[2] / 2.0);
    res.pt[0] = round(pos[0] / gridScale[0] - 0.5 - res.pt[2] / 2.0);

    // OUTPUT << "worldToGridPosition" << pos << " -> " << res << endl;
    // Vector3D check = gridToWorldPosition(res);
    // OUTPUT << "\tcheck" << res << " -> " << check << endl;
    // assert(check == pos);


    return res;
}

unsigned int
SkewFCCLattice::getCellDistance(const Cell3DPosition &p1, const Cell3DPosition &p2) const {
    if (p1 == p2) return 0;
    if (cellsAreAdjacent(p1,p2)) return 1;
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1]) + abs(p1[2] - p2[2]) ;
}

bool SkewFCCLattice::cellIsBlocked(const Cell3DPosition& pos) const {
    Cell3DPosition p1,p2;
    for (int i = 0; i < 6; i++) {
        p1 = getCellInDirection(pos, i);
        p2 = getCellInDirection(pos, getOppositeDirection(i));

        if (cellHasBlock(p1) and cellHasBlock(p2)) {
            cerr << "cells " << p1 << " and " << p2 << " are blocking " << pos << endl;
            return true;
        }
    }

    return false;
}

/************************************************************
 *   SkewFCCLattice::NeighborDirections
 ************************************************************/

Cell3DPosition SkewFCCLattice::getCellInDirection(const Cell3DPosition &pRef, int direction) const {
    return pRef + getRelativeConnectivity(pRef)[direction];
}

/************************************************************
 *   SkewFCCLattice::glDraw()
 ************************************************************/
void SkewFCCLattice::glDraw() const {
}

/********************* SCLattice *********************/
vector<Cell3DPosition> SCLattice::nCells{
        Cell3DPosition(1,0,0), // SOUTH
        Cell3DPosition(0,1,0), // EAST
        Cell3DPosition(-1,0,0),  // NORTH
        Cell3DPosition(0,-1,0),  // WEST
        Cell3DPosition(0,0,-1),  // BOTTOM
        Cell3DPosition(0,0,1)  // TOP
}; //!< Vector containing relative position of neighboring cells
SCLattice::SCLattice() : Lattice3D() { }
SCLattice::SCLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice3D(gsz,gsc) {}
SCLattice::~SCLattice() { }

vector<Cell3DPosition> SCLattice::getRelativeConnectivity(const Cell3DPosition &p) const {
    return nCells;
}

Vector3D SCLattice::gridToUnscaledWorldPosition_base(const Cell3DPosition &pos) {
    return Vector3D(pos[0], pos[1], pos[2], 0);
}

Vector3D SCLattice::gridToUnscaledWorldPosition(const Cell3DPosition &pos) const {
    return gridToUnscaledWorldPosition_base(pos);
}

Cell3DPosition SCLattice::unscaledWorldToGridPosition(const Vector3D &pos) const {
    return Cell3DPosition(pos[0], pos[1], pos[2]);
}

Cell3DPosition SCLattice::worldToGridPosition(const Vector3D &pos) const {
    return Cell3DPosition(pos[0] / gridScale[0],
                          pos[1] / gridScale[1],
                          pos[2] / gridScale[2]);
}

short SCLattice::getOppositeDirection(short d) const {
    switch (Direction(d)) {
        case North:	return South; break;
        case South:	return North; break;
        case East:	return West; break;
        case West:	return East; break;
        case Top:	return Bottom; break;
        case Bottom:	return Top; break;
        default:
            ERRPUT << "*** ERROR *** : unknown face: " << d << endl;
            return -1;
            break;
    }
}

string SCLattice::getDirectionString(short d) const {
    return isInRange(d, 0, this->getMaxNumNeighbors() - 1) ?
           directionName[d] : "undefined";

}

Cell3DPosition SCLattice::getCellInDirection(const Cell3DPosition &pRef, int direction) const {
    return pRef + nCells[direction];
}

void SCLattice::glDraw() const {
    static const float pts[8][3]={{0.1f,0.1f,0.1f},{0.9f,0.1f,0.1f},{0.9f,0.9f,0.1f},{0.1f,0.9f,0.1f},
                                  {0.1f,0.1f,0.9f},{0.9f,0.1f,0.9f},{0.9f,0.9f,0.9f},{0.1f,0.9f,0.9f}};
    static const uint8_t quads[24]={3,2,1,0,4,5,6,7,2,3,7,6,0,1,5,4,3,0,4,7,1,2,6,5};
    if (!mapHighlightedCells.empty()) {
        glDisable(GL_TEXTURE_2D);
        Vector3D v;
        int i;
        const uint8_t *ptr;
        Color c;
        for (const auto& pair : mapHighlightedCells) {
            glPushMatrix();
            v = gridToWorldPosition(pair.first);
            glTranslatef(v[0],v[1],v[2]);
            glScalef(gridScale[0],gridScale[1],gridScale[2]);

            pair.second.glMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE,0.75);
            glBegin(GL_QUADS);
            ptr = quads;
            i=6;
            while (i--) {
                glVertex3fv(pts[*ptr++]);
                glVertex3fv(pts[*ptr++]);
                glVertex3fv(pts[*ptr++]);
                glVertex3fv(pts[*ptr++]);
            }
            glEnd();
            glPopMatrix();
        }

    }
}


/********************* SCLattice2 *********************/
vector<Cell3DPosition> SCLattice2::nCells2{
        Cell3DPosition(1,0,0), // +X
        Cell3DPosition(0,1,0), // +Y
        Cell3DPosition(0,0,1),  // +Z
        Cell3DPosition(-1,0,0),  // -X
        Cell3DPosition(0,-1,0),  // -Y
        Cell3DPosition(0,0,-1)  // -Z
}; //!< Vector containing relative position of neighboring cells
SCLattice2::SCLattice2() : Lattice3D() { }
SCLattice2::SCLattice2(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice3D(gsz,gsc) {}
SCLattice2::~SCLattice2() { }

short SCLattice2::getOppositeDirection(short d) const {
    return Direction((d+3)%6);
}

string SCLattice2::getDirectionString(short d) const {
    return isInRange(d, 0, this->getMaxNumNeighbors() - 1) ?
           directionName[d] : "undefined";

}

Cell3DPosition SCLattice2::getCellInDirection(const Cell3DPosition &pRef, int direction) const {
    return pRef + nCells2[direction];
}

Vector3D SCLattice2::gridToUnscaledWorldPosition_base(const Cell3DPosition &pos) {
    return Vector3D(pos[0], pos[1], pos[2], 0);
}

Vector3D SCLattice2::gridToUnscaledWorldPosition(const Cell3DPosition &pos) const {
    return gridToUnscaledWorldPosition_base(pos);
}

Cell3DPosition SCLattice2::unscaledWorldToGridPosition(const Vector3D &pos) const {
    return Cell3DPosition(pos[0], pos[1], pos[2]);
}

Cell3DPosition SCLattice2::worldToGridPosition(const Vector3D &pos) const {
    return Cell3DPosition(pos[0] / gridScale[0],
                          pos[1] / gridScale[1],
                          pos[2] / gridScale[2]);
}

vector<Cell3DPosition> SCLattice2::getRelativeConnectivity(const Cell3DPosition &p) const {
    return nCells2;
}
