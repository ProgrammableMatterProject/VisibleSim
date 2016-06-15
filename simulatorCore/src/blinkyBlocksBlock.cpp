/*
 * blinkyBlocksBlock.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "blinkyBlocksBlock.h"
#include "buildingBlock.h"
#include "blinkyBlocksWorld.h"
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksEvents.h"
#include "trace.h"
#include "clock.h"
#include "meldProcessEvents.h"
#include "meldInterpretEvents.h"

using namespace std;

#define BLINKYBLOCKS_PACKET_DATASIZE 17
#define BLINKYBLOCKS_TRANSMISSION_MIN_TIME 6.08
#define BLINKYBLOCKS_TRANSMISSION_MAX_TIME 6.11

namespace BlinkyBlocks {
static const GLfloat tabColors[12][4]={{1.0,0.0,0.0,1.0},{1.0,0.647058824,0.0,1.0},{1.0,1.0,0.0,1.0},
                                       {0.0,1.0,0.0,1.0},{0.0,0.0,1.0,1.0},
                                       {0.274509804,0.509803922,0.705882353,1.0},
                                       {0.815686275,0.125490196,0.564705882,1.0},{0.5,0.5,0.5,1.0},
                                       {0.980392157,0.5,0.456,1.0},{0.549019608,0.5,0.5,1.0},
                                       {0.980392157,0.843137255,0.0,1.0},
                                       {0.094117647,0.545098039,0.094117647,1.0}};

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

BlinkyBlocksBlock::BlinkyBlocksBlock(int bId, BlinkyBlocksBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(BlinkyBlocksBlock*)) : BaseSimulator::BuildingBlock(bId) {
	OUTPUT << "BlinkyBlocksBlock constructor" << endl;
	double dataRateMin = ((BLINKYBLOCKS_PACKET_DATASIZE*pow(10,6)*8)/(BLINKYBLOCKS_TRANSMISSION_MAX_TIME*1000));
	double dataRateMax = ((BLINKYBLOCKS_PACKET_DATASIZE*pow(10,6)*8)/(BLINKYBLOCKS_TRANSMISSION_MIN_TIME*1000));

	for (int i=0; i<6; i++) {
		tabInterfaces[i] = new P2PNetworkInterface(this);
		getP2PNetworkInterfaceList().push_back(tabInterfaces[i]);
		tabInterfaces[i]->setDataRate((dataRateMax+dataRateMin)/2);
		tabInterfaces[i]->setDataRateVariability((dataRateMax-dataRateMin)/2);
	}
	clock = new Clock(Clock::XMEGA_RTC_OSC1K_CRC, this);
	buildNewBlockCode = blinkyBlocksBlockCodeBuildingFunction;
	blockCode = (BaseSimulator::BlockCode*)buildNewBlockCode(this);
}

BlinkyBlocksBlock::~BlinkyBlocksBlock() {
	OUTPUT << "BlinkyBlocksBlock destructor " << blockId << endl;
}

void BlinkyBlocksBlock::pauseClock(uint64_t delay, uint64_t start){
	//while(BaseSimulator::getScheduler()->now()<delay+start){

}

void BlinkyBlocksBlock::setPosition(const Vector3D &p) {
	position=p;
	getWorld()->updateGlData(this);
}

void BlinkyBlocksBlock::setColor(const Color &c) {
	lock();
	if (state >= ALIVE) {
		color = c;
	}
	unlock();
	getWorld()->updateGlData(this);
}

void BlinkyBlocksBlock::setColor(int num) {
	const GLfloat *col = tabColors[num%12];
	color.set(col[0],col[1],col[2],col[3]);
	getWorld()->updateGlData(this);
}

NeighborDirection::Direction BlinkyBlocksBlock::getDirection(P2PNetworkInterface *given_interface) {
	if( !given_interface) {
		return NeighborDirection::Direction(0);
	}
	for( int i(0); i < 6; ++i) {
		if( tabInterfaces[i] == given_interface) return NeighborDirection::Direction(i);
	}
	return NeighborDirection::Direction(0);
}

void BlinkyBlocksBlock::accel(uint64_t date, int x, int y, int z) {
	getScheduler()->scheduleLock(new AccelEvent(date, this, x, y, z));
}

void BlinkyBlocksBlock::shake(uint64_t date, int f) {
	getScheduler()->scheduleLock(new ShakeEvent(getScheduler()->now(), this, f));
}

void BlinkyBlocksBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
	OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on " << NeighborDirection::getString(getDirection(ni)) << endl;
	getScheduler()->scheduleLock(new AddNeighborEvent(getScheduler()->now(), this, NeighborDirection::getOpposite(getDirection(ni)), target->blockId));
}

void BlinkyBlocksBlock::removeNeighbor(P2PNetworkInterface *ni) {
	OUTPUT << "Simulator: "<< blockId << " remove neighbor on " << NeighborDirection::getString(getDirection(ni)) << endl;
	getScheduler()->scheduleLock(new RemoveNeighborEvent(getScheduler()->now(), this, NeighborDirection::getOpposite(getDirection(ni))));
}

void BlinkyBlocksBlock::stop(uint64_t date, State s) {
	OUTPUT << "Simulator: stop scheduled" << endl;
	lock();
	state = s;
	if (s == STOPPED) {
		// patch en attendant l'objet 3D qui modelise un BB stopped
		color = Color(0.1, 0.1, 0.1, 0.5);
	}
	unlock();
	getWorld()->updateGlData(this);
	if(BaseSimulator::Simulator::getType() == BaseSimulator::Simulator::MELDPROCESS){
            getScheduler()->scheduleLock(new MeldProcess::VMStopEvent(getScheduler()->now(), this));
	} else if (BaseSimulator::Simulator::getType() == BaseSimulator::Simulator::MELDINTERPRET) {
            getScheduler()->scheduleLock(new MeldInterpret::VMStopEvent(getScheduler()->now(), this));
	}
}

std::ostream& operator<<(std::ostream &stream, BlinkyBlocksBlock const& bb) {
	stream << bb.blockId << "\tcolor: " << bb.color;
	return stream;
}

P2PNetworkInterface* BlinkyBlocksBlock::getInterfaceDestId(int id) {
	for (int i=0; i<6; i++) {
		if (tabInterfaces[i]->connectedInterface != NULL) {
			if (tabInterfaces[i]->connectedInterface->hostBlock->blockId == id) {
				return tabInterfaces[i];
			}
		}
	}
	return NULL;
}

// inline string BlinkyBlocksBlock::xmlBuildingBlock() {       
// 	return "\t\t<block position=" + ConfigUtils::Vector3D3DToXmlString(position)
// 		+ " color=" + ConfigUtils::colorToXmlString(color) + " />\n";
// }
    
    
}
