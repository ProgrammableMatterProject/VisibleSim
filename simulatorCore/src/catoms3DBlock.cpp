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

using namespace std;

//! \namespace Catoms3D
namespace Catoms3D {

Catoms3DBlock::Catoms3DBlock(int bId, Catoms3DBlockCode *(*catoms3DBlockCodeBuildingFunction)(Catoms3DBlock*)) : BaseSimulator::BuildingBlock(bId) {
	OUTPUT << "Catoms3DBlock constructor" << endl;
	for (int i=0; i<12; i++) {
		tabInterfaces[i] = new P2PNetworkInterface(this);
	}
	buildNewBlockCode = catoms3DBlockCodeBuildingFunction;
	blockCode = (BaseSimulator::BlockCode*)buildNewBlockCode(this);

	orientationCode=0; // connector 0 is along X axis
 }

Catoms3DBlock::~Catoms3DBlock() {
	OUTPUT << "Catoms3DBlock destructor " << blockId << endl;
}

void Catoms3DBlock::setColor(const Color &c) {
	color = c;
	getWorld()->updateGlData(this,c);
}

void Catoms3DBlock::setVisible(bool visible) {
	getWorld()->updateGlData(this,visible);
}

void Catoms3DBlock::setPositionAndOrientation(const Cell3DPosition &p,short code) {
    orientationCode = code;
    position = p;

	Matrice M1,M2,M3,M;
	M1.setRotationZ(tabOrientationAngles[code][0]);
    M2.setRotationY(tabOrientationAngles[code][1]);
    M3.setRotationX(tabOrientationAngles[code][2]);
    M = M2*M1;
    M1 = M3*M;
    M2.setTranslation(getWorld()->gridToWorldPosition(p));
    M = M2*M1;
    OUTPUT << M << endl;
    getWorld()->updateGlData(this,M);
}

short Catoms3DBlock::getOrientationFromMatrix(const Matrice &mat) {
    Vecteur x(1.0,0.0,0.0,0.0); // vecteur X
    Vecteur v;
    //p = mat*x;
    Matrice mat_1;
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
    //OUTPUT << "result =" << current << endl;
    return current;
}

int Catoms3DBlock::getDirection(P2PNetworkInterface *given_interface) {
	if( !given_interface) {
		return -1;
	}
	for( int i(0); i < 12; ++i) {
		if( tabInterfaces[i] == given_interface) return i;
	}
	return -1;
}

std::ostream& operator<<(std::ostream &stream, Catoms3DBlock const& bb) {
  stream << bb.blockId << "\tcolor: " << bb.color;
  return stream;
}

bool Catoms3DBlock::getNeighborPos(short connectorID,Cell3DPosition &pos) {
    Vecteur realPos;

    Catoms3DWorld *wrl = getWorld();
    const float *bs = wrl->getBlocksSize();
    const int *gs = wrl->getGridSize();

    realPos.set(tabConnectorPositions[connectorID],3,1);
    realPos.pt[0]*=bs[0];
    realPos.pt[1]*=bs[1];
    realPos.pt[2]*=bs[2];
    realPos = ptrGlBlock->mat*realPos;
    if (realPos[2]<0) return false;
    pos = wrl->worldToGridPosition(realPos);
    return (pos[0]>=0 && pos[0]<gs[0] &&
            pos[1]>=0 && pos[1]<gs[1] &&
            pos[2]>=0 && pos[2]<gs[2]);
}

P2PNetworkInterface *Catoms3DBlock::getInterface(const Cell3DPosition& pos) {
    Catoms3DWorld *wrl = getWorld();
    Vecteur realPos = wrl->gridToWorldPosition(pos);

    Matrice m_1;
    ptrGlBlock->mat.inverse(m_1);
    realPos = m_1*realPos;

    const float *bs = wrl->getBlocksSize();
    realPos.pt[0]/=bs[0];
    realPos.pt[1]/=bs[1];
    realPos.pt[2]/=bs[2];

    double x,y,z,d=1;
    int i=0;

    while (i<12 && d>0.1) {
        x = tabConnectorPositions[i][0]-realPos.pt[0];
        y = tabConnectorPositions[i][1]-realPos.pt[1];
        z = tabConnectorPositions[i][2]-realPos.pt[2];
        d=x*x+y*y+z*z;
        i++;
    }
    return (d>0.1)?NULL:tabInterfaces[i-1];
}

}
