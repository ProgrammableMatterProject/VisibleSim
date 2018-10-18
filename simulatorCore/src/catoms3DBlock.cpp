/*!
 * \file catoms3DBlock.cpp
 * \brief catoms Block
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#include <iostream>
#include "catoms3DBlock.h"
#include "buildingBlock.h"
#include "catoms3DWorld.h"
#include "catoms3DSimulator.h"
#include "trace.h"
#include "catoms3DMotionEngine.h"

using namespace std;

Catoms3DBlock::Catoms3DBlock(int bId, BlockCodeBuilder bcb)
    : BaseSimulator::BuildingBlock(bId, bcb, FCCLattice::MAX_NB_NEIGHBORS) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "Catoms3DBlock constructor" << endl;
#endif

    orientationCode=0; // connector 0 is along X axis
}

Catoms3DBlock::~Catoms3DBlock() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "Catoms3DBlock destructor " << blockId << endl;
#endif
}

void Catoms3DBlock::setVisible(bool visible) {
    getWorld()->updateGlData(this,visible);
}

Matrix Catoms3DBlock::getMatrixFromPositionAndOrientation(const Cell3DPosition &pos,
                                                          short code) {
    short orientation = code%12;
    short up = code/12;

    Matrix M1,M2,M3,M;
    M1.setRotationZ(tabOrientationAngles[orientation][2]);
    M2.setRotationY(tabOrientationAngles[orientation][1]);
    M3.setRotationX(tabOrientationAngles[orientation][0]+up*180.0);
    M = M2*M1;
    M1 = M3*M;
    M2.setTranslation(getWorld()->lattice->gridToWorldPosition(pos));
    M = M2*M1;
    return M;
}

void Catoms3DBlock::setPosition(const Cell3DPosition &p) {
    setPositionAndOrientation(p, orientationCode);
}

void Catoms3DBlock::setPositionAndOrientation(const Cell3DPosition &pos, short code) {
    orientationCode = code;
    position = pos;

    Matrix M=getMatrixFromPositionAndOrientation(pos,code);
    getWorld()->updateGlData(this,M);
    getWorld()->updateGlData(this,position);
}

short Catoms3DBlock::getOrientationFromMatrix(const Matrix &mat) {
    Vector3D x(1.0,0.0,0.0,0.0); // Vector3D X
    Vector3D v;
    //p = mat*x;
    Matrix mat_1;
    mat.inverse(mat_1);

    short current=-1;
    double psmax=-1;
    for (int i=0; i<12; i++) {
        x.set(tabConnectorPositions[i],3);
        v = mat*x;
        //OUTPUT << "connector #" << i << ":" << v << endl;
        if (v[0]>psmax) {
            current=i;
            psmax=v[0];
        }
    }
    // orientation autour du connecteur
    Matrix M1,M2,M3,M;
    M1.setRotationZ(tabOrientationAngles[current][2]);
    M2.setRotationY(tabOrientationAngles[current][1]);
    M3.setRotationX(tabOrientationAngles[current][0]);
    M = M2*M1;
    M1 = M3*M;
    M1.inverse(M);
    M.m[15]=0;
    /*OUTPUT << "----- ref -----" << endl;
      OUTPUT << M << endl;
      OUTPUT << "----- mat -----" << endl;*/
    M3 = mat;
    //OUTPUT << M3 << endl;

    M2 = mat*M;
    //OUTPUT << M2 << endl;
    // detection of a rotation matrix PI around X axis if M2.m[10]=env.-1
    if (M2.m[10]<0) {
        current = current+12;
    }

    //OUTPUT << "result =" << current << endl;
    return current;
}

int Catoms3DBlock::getDirection(P2PNetworkInterface *given_interface) {
    if( !given_interface) {
        return -1;
    }
    for( int i(0); i < 12; ++i) {
        if(P2PNetworkInterfaces[i] == given_interface) return i;
    }
    return -1;
}

short Catoms3DBlock::getAbsoluteDirection(short connector) {
    Cell3DPosition conPos; // cell adjacent to connector
    bool posIsValid = getNeighborPos(connector, conPos);

    if (!posIsValid) return -1;
    Lattice *lattice = Catoms3DWorld::getWorld()->lattice;
    return lattice->getDirection(position, conPos);
}

short Catoms3DBlock::getAbsoluteDirection(const Cell3DPosition& pos) {
    return getAbsoluteDirection(getConnectorId(pos));
}

short Catoms3DBlock::projectAbsoluteNeighborDirection(const Cell3DPosition& nPos,
                                                      short nDirection) const {
    // cout << "pAND: " << "nPos: " << nPos << "/" << nDirection << endl
    //      << "\tPosition: " << position << endl;

    // Find cell on direction nDirection of neighbor at nPos
    Lattice *lattice = Catoms3DWorld::getWorld()->lattice;
    Cell3DPosition projectedPos = lattice->getCellInDirection(nPos, nDirection);
    // cout << "\tproj: " << projectedPos << endl;

    // No corresponding connector on current module
    if (!lattice->cellsAreAdjacent(position, projectedPos)) return -1;

    // Find connector adjacent to projectedPos on current module
    return getConnectorId(projectedPos);
}


std::ostream& operator<<(std::ostream &stream, Catoms3DBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color;
    return stream;
}

bool Catoms3DBlock::getNeighborPos(short connectorID,Cell3DPosition &pos) const {
    Vector3D realPos;

    Catoms3DWorld *wrl = getWorld();
    const Vector3D bs = wrl->lattice->gridScale;

    realPos.set(tabConnectorPositions[connectorID],3,1);
    realPos.pt[0] *= bs[0];
    realPos.pt[1] *= bs[1];
    realPos.pt[2] *= bs[2];
    realPos = ((Catoms3DGlBlock*)ptrGlBlock)->mat*realPos;
    if (realPos[2]<0) return false;
    pos = wrl->lattice->worldToGridPosition(realPos);
    return wrl->lattice->isInGrid(pos);
}

P2PNetworkInterface *Catoms3DBlock::getInterface(const Cell3DPosition& pos) {
    short conId = getConnectorId(pos);

    return conId >= 0 ? P2PNetworkInterfaces[conId] : NULL;
}

short Catoms3DBlock::getConnectorId(const Cell3DPosition& pos) const {
    Catoms3DWorld *wrl = getWorld();

    if (!wrl->lattice->isInGrid(pos))
        return -1;

    Vector3D realPos = wrl->lattice->gridToWorldPosition(pos);

    Matrix m_1;
    ((Catoms3DGlBlock*)ptrGlBlock)->mat.inverse(m_1);
    realPos = m_1*realPos;

    const Vector3D bs = wrl->lattice->gridScale;
    realPos.pt[0] /= bs[0];
    realPos.pt[1] /= bs[1];
    realPos.pt[2] /= bs[2];

    double x,y,z,d=1;
    int i=0;

    while (i<12 && d>0.1) {
        x = tabConnectorPositions[i][0]-realPos.pt[0];
        y = tabConnectorPositions[i][1]-realPos.pt[1];
        z = tabConnectorPositions[i][2]-realPos.pt[2];
        d=x*x+y*y+z*z;
        i++;
    }
    
    return d > 0.1 ? -1 : i - 1;
}

Catoms3DBlock* Catoms3DBlock::getNeighborOnCell(const Cell3DPosition &pos) const {
    Lattice *lattice = getWorld()->lattice;

    if (!lattice->cellsAreAdjacent(position, pos)) return NULL;

    return static_cast<Catoms3DBlock*>(lattice->getBlock(pos));
}

bool Catoms3DBlock::areOrientationsInverted(short otherOriCode) const {
    return ((orientationCode / 12) + (otherOriCode / 12)) == 1;
}

void Catoms3DBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
		   << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(
		new AddNeighborEvent(getScheduler()->now(), this,
							 getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
}

void Catoms3DBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
		   << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(
		new RemoveNeighborEvent(getScheduler()->now(), this,
								getWorld()->lattice->getOppositeDirection(getDirection(ni))));
}

Catoms3DBlock *Catoms3DBlock::getNeighborBlock(const Cell3DPosition& nPos) const {     
    return static_cast<Catoms3DBlock*>(getWorld()->lattice->getBlock(nPos));
}

Catoms3DBlock *Catoms3DBlock::getNeighborBlock(short conId) const {
    P2PNetworkInterface *itfId = getInterface(conId);

    if (itfId and itfId->isConnected())
        return static_cast<Catoms3DBlock*>(itfId->connectedInterface->hostBlock);

    return NULL;
}

bool Catoms3DBlock::canRotateToPosition(const Cell3DPosition &pos,
                                        RotationLinkType faceReq) const {
    return Catoms3DMotionEngine::findMotionPivot(this, pos, faceReq) != NULL;
}
