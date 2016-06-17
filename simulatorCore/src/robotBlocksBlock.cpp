/*
 * robotBlocksBlock.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "robotBlocksBlock.h"
#include "buildingBlock.h"
#include "robotBlocksWorld.h"
#include "robotBlocksSimulator.h"
#include "trace.h"

using namespace std;

namespace RobotBlocks {

string NeighborDirection::getString(int d) {
	switch(d) {
		case Front:
			return string("Front");
			break;
		case Back:
			return string("Back");
			break;
		case Left:
			return string("Left");
			break;
		case Right:
			return string("Right");
			break;
		case Top:
			return string("Top");
			break;
		case Bottom:
			return string("Bottom");
			break;
		default:
			cerr << "Unknown direction" << endl;
			return string("Unknown");
			break;
	}
}

int NeighborDirection::getOpposite(int d) {
switch (Direction(d)) {
		case Front:
			return Back;
			break;
		case Back:
			return Front;
			break;
		case Left:
			return Right;
			break;
		case Right:
			return Left;
			break;
		case Top:
			return Bottom;
			break;
		case Bottom:
			return Top;
			break;
		default:
			ERRPUT << "*** ERROR *** : unknown face" << endl;
			return -1;
			break;
	}
}

RobotBlocksBlock::RobotBlocksBlock(int bId, RobotBlocksBlockCode *(*robotBlocksBlockCodeBuildingFunction)(RobotBlocksBlock*)) : BaseSimulator::BuildingBlock(bId) {
	OUTPUT << "RobotBlocksBlock constructor" << endl;
	for (int i=0; i<6; i++) {
		tabInterfaces[i] = new P2PNetworkInterface(this);
	}
	buildNewBlockCode = robotBlocksBlockCodeBuildingFunction;
	blockCode = (BaseSimulator::BlockCode*)buildNewBlockCode(this);
 }

RobotBlocksBlock::~RobotBlocksBlock() {
	OUTPUT << "RobotBlocksBlock destructor " << blockId << endl;
}

void RobotBlocksBlock::setPrevNext(int prev,int next) {
	getWorld()->updateGlData(this,prev,next);
}

void RobotBlocksBlock::setPrevNext(const P2PNetworkInterface *prev,const P2PNetworkInterface *next) {
    int prevId=0,nextId=0;
    if (prev) {
	RobotBlocksBlock*rb = (RobotBlocksBlock*)(prev->hostBlock);
	prevId = rb->blockId;
    }
	  if (next) {
	RobotBlocksBlock*rb = (RobotBlocksBlock*)(next->hostBlock);
	nextId = rb->blockId;
    }
    //cout << (prev?prev->hostBlock->blockId:-1) << "," << (next?next->hostBlock->blockId:-1) << endl;
	getWorld()->updateGlData(this,prevId,nextId);
}

NeighborDirection::Direction RobotBlocksBlock::getDirection(P2PNetworkInterface *given_interface) {
	if( !given_interface) {
		return NeighborDirection::Direction(0);
	}
	for( int i(0); i < 6; ++i) {
		if( tabInterfaces[i] == given_interface) return NeighborDirection::Direction(i);
	}
	return NeighborDirection::Direction(0);
}

P2PNetworkInterface *RobotBlocksBlock::getP2PNetworkInterfaceByRelPos(const PointRel3D &pos) {
    if (pos.x==-1) return tabInterfaces[NeighborDirection::Back];
    else if (pos.x==1) return tabInterfaces[NeighborDirection::Front];
    else if (pos.y==-1) return tabInterfaces[NeighborDirection::Left];
    else if (pos.y==1) return tabInterfaces[NeighborDirection::Right];
    else if (pos.z==-1) return tabInterfaces[NeighborDirection::Bottom];
    else if (pos.z==1) return tabInterfaces[NeighborDirection::Top];

    return NULL;
}

std::ostream& operator<<(std::ostream &stream, RobotBlocksBlock const& bb) {
  stream << bb.blockId << "\tcolor: " << bb.color;
  return stream;
}

}
