/*
 * catoms2DBlock.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "catoms2DBlock.h"
#include "buildingBlock.h"
#include "catoms2DWorld.h"
#include "catoms2DSimulator.h"
#include "trace.h"

using namespace std;

namespace Catoms2D {

string NeighborDirection::getString(int d) {
	switch(d) {
		case BottomLeft:
			return string("Bottom-left");
			break;
		case Left:
			return string("Left");
			break;
		case TopLeft:
			return string("Top-left");
			break;
		case BottomRight:
			return string("Bottom-right");
			break;
		case Right:
			return string("Right");
			break;
		case TopRight:
			return string("Top-right");
			break;
		default:
			cerr << "Unknown direction" << endl;
			return string("Unknown");
			break;
	}
}

int NeighborDirection::getOpposite(int d) {
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
			ERRPUT << "*** ERROR *** : unknown face" << endl;
			return -1;
			break;
	}
}

Catoms2DBlock::Catoms2DBlock(int bId, Catoms2DBlockCode *(*catoms2DBlockCodeBuildingFunction)(Catoms2DBlock*)) : BaseSimulator::BuildingBlock(bId) {
	OUTPUT << "Catoms2DBlock constructor" << endl;
	for (int i=0; i<MAX_NB_NEIGHBORS; i++) {
		tabInterfaces[i] = new P2PNetworkInterface(this);
	}
	buildNewBlockCode = catoms2DBlockCodeBuildingFunction;
	blockCode = (BaseSimulator::BlockCode*)buildNewBlockCode(this);
	angle = 0;
 }

Catoms2DBlock::~Catoms2DBlock() {
	OUTPUT << "Catoms2DBlock destructor " << blockId << endl;
}

void Catoms2DBlock::setPosition(const Vecteur &p) {
	position=p;
	getWorld()->updateGlData(this);
}

void Catoms2DBlock::setColor(const Color &c) {
	color = c;
	getWorld()->updateGlData(this,Vecteur(ptrGlBlock->position[0],ptrGlBlock->position[1],ptrGlBlock->position[2]),ptrGlBlock->angle);
}

NeighborDirection::Direction Catoms2DBlock::getDirection(P2PNetworkInterface *given_interface) {
	if( !given_interface) {
		return NeighborDirection::Direction(0);
	}
	for( int i(0); i < MAX_NB_NEIGHBORS; ++i) {
		if( tabInterfaces[i] == given_interface) return NeighborDirection::Direction(i);
	}
	return NeighborDirection::Direction(0);
}

std::ostream& operator<<(std::ostream &stream, Catoms2DBlock const& bb) {
  stream << bb.blockId << "\tcolor: " << bb.color;
  return stream;
}

P2PNetworkInterface *Catoms2DBlock::getInterface(NeighborDirection::Direction d) {
	int alpha = angle;
	int beta = d*60;
	int t = beta-alpha;
	if (t>=0){
		return tabInterfaces[t/60];	
	} else {
		return tabInterfaces[(360+t)/60];
	}
}

int Catoms2DBlock::nbNeighbors() {
	int cnt = 0;
	for (int i = 0; i < MAX_NB_NEIGHBORS; i++) {
		if (tabInterfaces[i]->connectedInterface) 
			cnt++;
	}
	return cnt;
}

int Catoms2DBlock::nbConsecutiveNeighbors() {
	int empty = -1;
	int m = 0;
	int cnt = 0;
	for( int i = 0; i < MAX_NB_NEIGHBORS; i++) {
		if(tabInterfaces[i]->connectedInterface == NULL) {
			empty = i;
			break;
		}
	}
	if (empty == -1) {
		return MAX_NB_NEIGHBORS;
	}
	
	
	for( int i = 0; i < MAX_NB_NEIGHBORS; i++) {
		int j = (empty+i)%MAX_NB_NEIGHBORS;
		if(tabInterfaces[j]->connectedInterface) {
			 cnt++;
		} else {
			m = max(m,cnt);
			cnt = 0;
		}
	}
	m = max(m,cnt);
	return m;
}

// Motion
bool Catoms2DBlock::canMove(Catoms2DMove &m) {  
  // physical moving condition
  // pivot is a neighbor (physically connected)
  // move CW around i connector: i+1 and i+2 should be free
  // move CCW around i connector: i-1 and i-2 should be free
  return true;
}

void Catoms2DBlock::startMove(Catoms2DMove &m, uint64_t t) {
  getScheduler()->schedule(new MotionStartEvent(t,this,m));
}

void Catoms2DBlock::startMove(Catoms2DMove &m) {
  startMove(m,getScheduler()->now());
}

}
