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

const float tabOrientationAngles[12][3] = { {0,0,0}, {-90.0f,0,90.0f}, {-45.0f,45.0,-90.0f},
{-135.0f,45.0f,90}, {135.0f,45.0f,-90.0f}, {45.0f,45.0f,90.0f},
{180.0f,0,0}, {270.0f,0,0}, {135.0f,-45.0f,90.0f},
{45.0f,-45.0f,-90.0f}, {-45.0f,-45.0f,90.0f}, {-135.0f,-45.0f,-90.0f} };

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

void Catoms3DBlock::setPosition(const Cell3DPosition &p) {
	position=p;
	getWorld()->updateGlData(this,p);
}

void Catoms3DBlock::setColor(const Color &c) {
	color = c;
	getWorld()->updateGlData(this,c);
}

void Catoms3DBlock::setOrientation(short code) {
    orientationCode = code;

    getWorld()->updateGlData(this,tabOrientationAngles[code][0],tabOrientationAngles[code][1],tabOrientationAngles[code][2]);
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

P2PNetworkInterface *Catoms3DBlock::getP2PNetworkInterfaceByRelPos(const PointRel3D &pos) {
	// NOT TESTED. FALSE I THINK, DEPENDS ON THE PARITY OF THE LINE
/*    if (pos.x==-1 && pos.y==0) return tabInterfaces[NeighborDirection::Left];
    else if (pos.x==1 && pos.y==0) return tabInterfaces[NeighborDirection::Right];

    else if (pos.y==-1 && pos.x==1) return tabInterfaces[NeighborDirection::BottomRight];
    else if (pos.y==1 && pos.x==1) return tabInterfaces[NeighborDirection::TopRight];

    else if (pos.z==-1) return tabInterfaces[NeighborDirection::BottomLeft];
    else if (pos.z==1) return tabInterfaces[NeighborDirection::TopLeft];
*/
    return NULL;
}

std::ostream& operator<<(std::ostream &stream, Catoms3DBlock const& bb) {
  stream << bb.blockId << "\tcolor: " << bb.color;
  return stream;
}

}
