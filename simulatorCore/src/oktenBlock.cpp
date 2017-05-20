/*!
 * \file oktenBlock.cpp
 * \brief okten Block
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#include <iostream>
#include "oktenBlock.h"
#include "buildingBlock.h"
#include "oktenWorld.h"
#include "oktenSimulator.h"
#include "trace.h"

using namespace std;

//! \namespace Okten
namespace Okten {

OktenBlock::OktenBlock(int bId, BlockCodeBuilder bcb)
    : BaseSimulator::BuildingBlock(bId, bcb, FCCLattice::MAX_NB_NEIGHBORS) {
    OUTPUT << "OktenBlock constructor" << endl;

    orientationCode=0; // connector 0 is along X axis
}

OktenBlock::~OktenBlock() {
    OUTPUT << "OktenBlock destructor " << blockId << endl;
}

Matrix OktenBlock::getMatrixFromPositionAndOrientation(const Cell3DPosition &pos,short code) {
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

void OktenBlock::setPositionAndOrientation(const Cell3DPosition &pos,short code) {
    orientationCode = code;
    position = pos;

    Matrix M=getMatrixFromPositionAndOrientation(pos,code);
    getWorld()->updateGlData(this,M);
    getWorld()->updateGlData(this,position);
}

short OktenBlock::getOrientationFromMatrix(const Matrix &mat) {
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

int OktenBlock::getDirection(P2PNetworkInterface *given_interface) {
    if( !given_interface) {
        return -1;
    }
    for( int i(0); i < 12; ++i) {
        if(P2PNetworkInterfaces[i] == given_interface) return i;
    }
    return -1;
}

std::ostream& operator<<(std::ostream &stream, OktenBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color;
    return stream;
}

bool OktenBlock::getNeighborPos(short connectorID,Cell3DPosition &pos) const {
    Vector3D realPos;

    OktenWorld *wrl = getWorld();
    const Vector3D bs = wrl->lattice->gridScale;

    realPos.set(tabConnectorPositions[connectorID],3,1);
    realPos.pt[0] *= bs[0];
    realPos.pt[1] *= bs[1];
    realPos.pt[2] *= bs[2];
    realPos = ((OktenGlBlock*)ptrGlBlock)->mat*realPos;
    if (realPos[2]<0) return false;
    pos = wrl->lattice->worldToGridPosition(realPos);
    return wrl->lattice->isInGrid(pos);
}

void OktenBlock::setConnectorLength(short connectorId,float length) {
    OktenWorld *wrl = getWorld();
    wrl->updateGlData(this,connectorId,length);
}

void OktenBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
		   << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
    getScheduler()->schedule(
		new AddNeighborEvent(getScheduler()->now(), this,
							 getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
}

void OktenBlock::removeNeighbor(P2PNetworkInterface *ni) {
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
		   << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
    getScheduler()->schedule(
		new RemoveNeighborEvent(getScheduler()->now(), this,
								getWorld()->lattice->getOppositeDirection(getDirection(ni))));
}

}
