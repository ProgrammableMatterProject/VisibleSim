#include <climits>

#include "lattice.h"
#include "utils.h"
#include "trace.h"

using namespace BaseSimulator;
using namespace utils;
using namespace std;

// #define LATTICE_LOG 1

/********************* Lattice *********************/

const string Lattice::directionName[] = {};

Lattice::Lattice() {
    grid = NULL;
}

Lattice::Lattice(const Cell3DPosition &gsz, const Vector3D &gsc) {
    gridSize = gsz;
    gridScale = gsc;

    if (gsz[0] <= 0 || gsz[1] <= 0 || gsz[2] <= 0) {
        cerr << "error: Incorrect lattice size: size in any direction cannot be negative or null" << endl;
        throw InvalidDimensionsException();
    }

    grid = new BuildingBlock*[gridSize[0] * gridSize[1] * gridSize[2]];
    // Initializes grid to NULL
    BuildingBlock **ptr = grid;
    int i=gridSize[0] * gridSize[1] * gridSize[2];
    while (i--) {
        *ptr=NULL;
        ptr++;
    }

#ifdef LATTICE_LOG
    cerr << "l.new(gridSize = " << gridSize << ", gridScale = " << gridScale << ")" << endl;
#endif
}

Lattice::~Lattice() {
    delete [] grid;
}

int Lattice::getIndex(const Cell3DPosition &p) const {
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
        throw InvalidInsertionException();
        // exit(EXIT_FAILURE);
    }

#ifdef LATTICE_LOG
    cerr << "l.insert(" << bb->blockId << ") on " << p << " = i:" << getIndex(p) << endl;
#endif
}

void Lattice::remove(const Cell3DPosition &p) {
    grid[getIndex(p)] = NULL;
}

BuildingBlock* Lattice::getBlock(const Cell3DPosition &p) const {
    return isInGrid(p) ? grid[getIndex(p)] : NULL;
}

bool Lattice::isFree(const Cell3DPosition &p) const {
    if (!isInGrid(p))
        return false;
    else
        return (getBlock(p) == NULL);
}

bool Lattice::cellHasBlock(const Cell3DPosition &p) const {
    if (!isInGrid(p)) {
        return false;
    } else {
        return (getBlock(p) != NULL);
    }
}

bool Lattice::isInGrid(const Cell3DPosition &p) const {
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

vector<Cell3DPosition> Lattice::getFreeNeighborCells(const Cell3DPosition &pos) {
    vector<Cell3DPosition> freeNeighborCells;

    for (Cell3DPosition p : getNeighborhood(pos)) { // Check if each neighbor cell has an active node on it
        if (isFree(p)) {
            freeNeighborCells.push_back(p);         // Add its position to the result
        }
    }

    return freeNeighborCells;
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

short Lattice::getDirection(const Cell3DPosition &p, const Cell3DPosition &neighbor) {
    auto neighborhood = getNeighborhood(p);
    for (unsigned short i = 0; i < neighborhood.size(); i++) {
        if (neighbor == neighborhood[i]) return i;
    }

    return -1;
}

bool Lattice::cellsAreAdjacent(const Cell3DPosition &p1, const Cell3DPosition &p2) {
    for (const Cell3DPosition pos : getNeighborhood(p1))
        if (p2 == pos) return true;

    return false;
}


string Lattice::getDirectionString(short d) {
    return isInRange(d, 0, this->getMaxNumNeighbors() - 1) ?
        directionName[d] : "undefined";
}

vector<HighlightedCell>::iterator Lattice::find(const Cell3DPosition& val) {
	vector<HighlightedCell>::iterator first = tabHighlightedCells.begin();
	while (first!=tabHighlightedCells.end()) {
		if ((*first).pos==val) return first;
		++first;
	}
	return tabHighlightedCells.end();
}

void Lattice::highlightCell(const Cell3DPosition& pos, const Color &color) {
	vector<HighlightedCell>::iterator existing = find(pos);
	if (existing!=tabHighlightedCells.end()) {
		(*existing).color=color;
	} else {
		HighlightedCell hc(pos,color);
		tabHighlightedCells.push_back(hc);
	}
}

void Lattice::unhighlightCell(const Cell3DPosition& pos) {
	vector<HighlightedCell>::iterator existing = find(pos);
	if (existing!=tabHighlightedCells.end()) {
		tabHighlightedCells.erase(existing);
	}
}

void Lattice::resetCellHighlights() {
	tabHighlightedCells.clear();
}

/********************* Lattice2D *********************/
Lattice2D::Lattice2D() : Lattice() {}
Lattice2D::Lattice2D(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice(gsz,gsc) {}
Lattice2D::~Lattice2D() {}
const string Lattice2D::directionName[] = {};
/********************* Lattice3D *********************/
Lattice3D::Lattice3D() : Lattice() {}
Lattice3D::Lattice3D(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice(gsz,gsc) {}
Lattice3D::~Lattice3D() {}
const string Lattice3D::directionName[] = {};
/********************* HLattice *********************/
HLattice::HLattice() : Lattice2D() {}
HLattice::HLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice2D(gsz,gsc) {}
HLattice::~HLattice() {}

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
    res.pt[0] = round((pos[0] / gridScale[0] - ((int)res.pt[2] % 2) * 0.5));

    /*
      cout << "------------computation worldToGridPosition--------------" << endl;
      cout << pos << endl;
      cout << res << endl;
      cout << "---------------------------------------------------------" << endl;
    */
    return res;
}

vector<Cell3DPosition> HLattice::getRelativeConnectivity(const Cell3DPosition &p) {
    return IS_EVEN(p[2]) ? nCellsEven : nCellsOdd;
}

Cell3DPosition HLattice::getCellInDirection(const Cell3DPosition &pRef, int direction)
{
    return pRef + getRelativeConnectivity(pRef)[direction];
}


/************************************************************
 *   HLattice::NeighborDirections
 ************************************************************/

const string HLattice::directionName[] = {"Right","TopRight","TopLeft",
                                                             "Left","BottomLeft","BottomRight"};

int HLattice::getOppositeDirection(int d) {
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

string HLattice::getDirectionString(int d) {
    return directionName[d];
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

Cell3DPosition SLattice::getCellInDirection(const Cell3DPosition &pRef, int direction)
{
    return pRef + nCells[direction];
}

/************************************************************
 *   SLattice::NeighborDirections
 ************************************************************/

const string SLattice::directionName[] = {"North","East","South","West"};

int SLattice::getOppositeDirection(int d) {
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

string SLattice::getDirectionString(int d) {
    return directionName[d];
}


/********************* FCCLattice *********************/
FCCLattice::FCCLattice() : Lattice3D() {
}

FCCLattice::FCCLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice3D(gsz,gsc) {
    tabLockedCells = new bool[gridSize[0] * gridSize[1] * gridSize[2]]();
    /*bool *ptr=tabLockedCells;
    int i=gridSize[0] * gridSize[1] * gridSize[2];
    while (--i) {
        *ptr++=false;
    }*/
    // OUTPUT << "init Lattice" << endl;
    // for (int i=0; i<gridSize[0] * gridSize[1] * gridSize[2];i++) { OUTPUT << tabLockedCells[i] << " "; }
    // OUTPUT << endl;

    tabDistances=NULL;
}


FCCLattice::~FCCLattice() {
    delete [] tabLockedCells;
    delete [] tabDistances;
}

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

const string FCCLattice::directionName[] = {"Con0", "Con1", "Con2",
                                            "Con3", "Con4", "Con5",
                                            "Con6", "Con7", "Con8",
                                            "Con9", "Con10", "Con11"};

int FCCLattice::getOppositeDirection(int d) {
    switch (Direction(d)) {
    case Con0:	return Con6; break;
    case Con1:	return Con7; break;
    case Con2:	return Con8; break;
    case Con3:	return Con9; break;
    case Con4:	return Con10; break;
    case Con5:	return Con11; break;
    case Con6:	return Con0; break;
    case Con7:	return Con1; break;
    case Con8:	return Con2; break;
    case Con9:	return Con3; break;
    case Con10:	return Con4; break;
    case Con11:	return Con5; break;
    default:
		ERRPUT << "*** ERROR *** : unknown face: " << d << endl;
		return -1;
		break;
    }
}

string FCCLattice::getDirectionString(int d) {
    return directionName[d];
}

Cell3DPosition FCCLattice::getCellInDirection(const Cell3DPosition &pRef, int direction)
{
    return pRef + getRelativeConnectivity(pRef)[direction];
}

bool FCCLattice::lockCell(const Cell3DPosition &pos) {
    if (!isInGrid(pos)) return true;

    int ind = getIndex(pos);
    OUTPUT << "ind=" << ind << " / pos=" << pos << " / state=" << tabLockedCells[ind] << " / bb=" << (grid[ind]==NULL?0:1) << endl;
    if (tabLockedCells[ind] || grid[ind]!=NULL) {
        return false;
    }
    tabLockedCells[ind] = true;
    return true;
}

bool FCCLattice::unlockCell(const Cell3DPosition &pos) {
    if (!isInGrid(pos)) return true;

    int ind = getIndex(pos);
    bool prev = tabLockedCells[ind];
    tabLockedCells[ind] = false;
    return prev;
}

void FCCLattice::glDraw() {
    
    if (tabDistances) {
		int ix,iy,iz;
		Cell3DPosition gp;
		Vector3D v;
		unsigned short *ptrDistance = tabDistances;
		bool *ptr = tabLockedCells;
		for (iz=0; iz<gridSize[2]; iz++) {
			for (iy=0; iy<gridSize[1]; iy++) {
				for (ix=0; ix<gridSize[0]; ix++) {
					if (*ptr) {
						glPushMatrix();
						gp.set(ix,iy,iz);
						v = gridToWorldPosition(gp);
						glTranslatef(v[0],v[1],v[2]);
						glutSolidSphere(0.065*gridScale[0],6,6);
						glPopMatrix();
					}
					if (*ptrDistance!=USHRT_MAX) {
						glPushMatrix();
						gp.set(ix,iy,iz);
						v = gridToWorldPosition(gp);
						glTranslatef(v[0],v[1],v[2]);

						glMaterialfv(GL_FRONT,GL_DIFFUSE,tabColors[*ptrDistance%12]);
						glutSolidCube(0.2*gridScale[0]);
						glPopMatrix();

					}
					ptr++;
					ptrDistance++;
				}
			}
		}
	}
    if (!tabHighlightedCells.empty()) {
		vector<HighlightedCell>::const_iterator it = tabHighlightedCells.begin();
		Vector3D v;
		while (it!=tabHighlightedCells.end()) {
			glPushMatrix();
			v = gridToWorldPosition((*it).pos);
			glTranslatef(v.pt[0],v.pt[1],v.pt[2]);
			glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE,(*it).color.rgba);
			glutWireSphere(5.0,12,6);
			glPopMatrix();
			++it;
		}
	}
}

void FCCLattice::initTabDistances() {
    if (tabDistances==NULL) {
        int n = gridSize.pt[0]*gridSize.pt[1]*gridSize.pt[2];
        tabDistances = new unsigned short[n];
        // initialisation of tabDistances with value 'd'
        unsigned short *ptr=tabDistances;
        while (n--) {
            *ptr++=USHRT_MAX;
        }

    }
}

unsigned short FCCLattice::getDistance(const Cell3DPosition &pos) {
    if (!isInGrid(pos)) return USHRT_MAX;
    return tabDistances[getIndex(pos)];
}

void FCCLattice::setDistance(const Cell3DPosition &pos,unsigned short d) {
    if (isInGrid(pos)) tabDistances[getIndex(pos)]=d;
}

// === NEIGHBOR RESTRICTIONS ===

void FCCLattice::setPlaneSides(BlockingPositionPlane plane,
                               const Cell3DPosition& pos,
                               Cell3DPosition& sideOne, Cell3DPosition& sideTwo,
                               int d, bool evenZ) {
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

bool FCCLattice::isPositionUnblockedSide(const Cell3DPosition &pos) {
    Cell3DPosition occupiedPosition = pos + xyPos[0];
    Cell3DPosition forbiddenPosition = pos + xyPos[1];

    if (cellHasBlock(occupiedPosition) && cellHasBlock(forbiddenPosition))
        return false;
    
    occupiedPosition = pos + xyPos[2];
    forbiddenPosition = pos + xyPos[3];

    return !(cellHasBlock(occupiedPosition) && cellHasBlock(forbiddenPosition));
}

bool FCCLattice::isPositionUnblockedSide(const Cell3DPosition &pos,
                                         const Cell3DPosition &ignore) {
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
                                     BlockingPositionPlane plane) {
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
                                     BlockingPositionPlane plane) {
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

bool FCCLattice::isPositionBlocked(const Cell3DPosition &pos) {
    return isPositionUnblockedSide(pos)
        && !(isPositionUnblocked(pos, BlockingPositionPlane::XY)
             || isPositionUnblocked(pos, BlockingPositionPlane::YZ)
             || isPositionUnblocked(pos, BlockingPositionPlane::XZ));
}


bool FCCLattice::isPositionBlocked(const Cell3DPosition &pos,
                                   const Cell3DPosition &ignore) {
    return isPositionUnblockedSide(pos, ignore)
        && !(isPositionUnblocked(pos, ignore, BlockingPositionPlane::XY)
             || isPositionUnblocked(pos, ignore, BlockingPositionPlane::YZ)
             || isPositionUnblocked(pos, ignore, BlockingPositionPlane::XZ));
}

// bool FCCLattice::isPositionBlockable(const Cell3DPosition &pos) {
//     Cell3DPosition neighborPos;
//     Catoms3DWorld *world = Catoms3DWorld::getWorld();

//     for (int i = 0; i < 12; i++) {
//         neighborPos = getCellInDirection(pos, i);
 
//         if (isFree(neighborPos) && !isPositionBlocked(neighborPos)) {
//            simulatedBlockPosition = pos; 
//            if (isPositionBlocked(neighborPos))
//                return true;
//            simulatedBlockPosition.set(0,0,0);
//         }
//     }
//     return false;
// }

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

const string SCLattice::directionName[] = {"Bottom", "Back", "Right","Front", "Left", "Top"};

int SCLattice::getOppositeDirection(int d) {
    switch (Direction(d)) {
    case Front:	return Back; break;
    case Back:	return Front; break;
    case Left:	return Right; break;
    case Right:	return Left; break;
    case Top:	return Bottom; break;
    case Bottom:	return Top; break;
    default:
		ERRPUT << "*** ERROR *** : unknown face: " << d << endl;
		return -1;
		break;
    }
}

string SCLattice::getDirectionString(int d) {
    return directionName[d];
}

Cell3DPosition SCLattice::getCellInDirection(const Cell3DPosition &pRef, int direction)
{
    return pRef + nCells[direction];
}

/********************* BCLattice *********************/
BCLattice::BCLattice() : Lattice3D() {}
BCLattice::BCLattice(const Cell3DPosition &gsz, const Vector3D &gsc) : Lattice3D(gsz,gsc) {}
BCLattice::~BCLattice() {}

vector<Cell3DPosition> BCLattice::getRelativeConnectivity(const Cell3DPosition &p) {
    return vector<Cell3DPosition>();
}

Vector3D BCLattice::gridToWorldPosition(const Cell3DPosition &pos) {
    return Vector3D(pos[0] * gridScale[0],
                    pos[1] * gridScale[1],
                    pos[2] * gridScale[2]);
}

Cell3DPosition BCLattice::worldToGridPosition(const Vector3D &pos) {
    return Cell3DPosition(pos[0] / gridScale[0],
                          pos[1] / gridScale[1],
                          pos[2] / gridScale[2]);
}

int BCLattice::getOppositeDirection(int d) {
    return -1;
}

string BCLattice::getDirectionString(int d) {
    return "Wireless";
}
