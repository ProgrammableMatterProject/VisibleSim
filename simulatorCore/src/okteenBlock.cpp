/*!
 * \file okteenBlock.cpp
 * \brief okteen Block
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#include <iostream>
#include "okteenBlock.h"
#include "buildingBlock.h"
#include "okteenWorld.h"
#include "okteenSimulator.h"
#include "trace.h"

using namespace std;

//! \namespace Okteen
namespace Okteen {

OkteenBlock::OkteenBlock(int bId, BlockCodeBuilder bcb)
    : BaseSimulator::BuildingBlock(bId, bcb, FCCLattice::MAX_NB_NEIGHBORS) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "OkteenBlock constructor" << endl;
#endif

    orientationCode=0; // connector 0 is along X axis
}

OkteenBlock::~OkteenBlock() {
    OUTPUT << "OkteenBlock destructor " << blockId << endl;
}
/*
Matrix OkteenBlock::getMatrixFromPositionAndOrientation(const Cell3DPosition &pos,short code) {
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

void OkteenBlock::setPositionAndOrientation(const Cell3DPosition &pos,short code) {
    orientationCode = code;
    position = pos;

    Matrix M=getMatrixFromPositionAndOrientation(pos,code);
    getWorld()->updateGlData(this,M);
    getWorld()->updateGlData(this,position);
}

short OkteenBlock::getOrientationFromMatrix(const Matrix &mat) {
    Vector3D x(1.0,0.0,0.0,0.0); // Vector3D X
    Vector3D v;
    Matrix mat_1;
    mat.inverse(mat_1);

    short current=-1;
    double psmax=-1;
    for (int i=0; i<12; i++) {
        x.set(tabConnectorPositions[i],3);
        v = mat*x;
        if (v[0]>psmax) {
            current=i;
            psmax=v[0];
        }
    }
    Matrix M1,M2,M3,M;
    M1.setRotationZ(tabOrientationAngles[current][2]);
    M2.setRotationY(tabOrientationAngles[current][1]);
    M3.setRotationX(tabOrientationAngles[current][0]);
    M = M2*M1;
    M1 = M3*M;
    M1.inverse(M);
    M.m[15]=0;
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
*/
int OkteenBlock::getDirection(P2PNetworkInterface *given_interface) const {
    if( !given_interface) {
        return -1;
    }
    for( int i(0); i < 12; ++i) {
        if(P2PNetworkInterfaces[i] == given_interface) return i;
    }
    return -1;
}

std::ostream& operator<<(std::ostream &stream, OkteenBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color;
    return stream;
}

bool OkteenBlock::getNeighborPos(SCLattice::Direction connectorDir,Cell3DPosition &pos) const {
    OkteenWorld *wrl = getWorld();
    pos = position+((SCLattice*)(wrl->lattice))->getNeighborRelativePos(connectorDir);
    return wrl->lattice->isInGrid(pos);
}

void OkteenBlock::setConnectorLength(short connectorId,float length) {
    OkteenWorld *wrl = getWorld();
    wrl->updateGlData(this,connectorId,length);
}

void OkteenBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
		   << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(new AddNeighborEvent(getScheduler()->now(), this,getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
}

void OkteenBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
		   << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(new RemoveNeighborEvent(getScheduler()->now(), this,getWorld()->lattice->getOppositeDirection(getDirection(ni))));
    getWorld()->updateGlData(this,getDirection(ni),0);
}

}
