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

using namespace std;

namespace BlinkyBlocks {
static const GLfloat tabColors[12][4]={{1.0,0.0,0.0,1.0},{1.0,0.647058824,0.0,1.0},{1.0,1.0,0.0,1.0},{0.0,1.0,0.0,1.0},
									{0.0,0.0,1.0,1.0},{0.274509804,0.509803922,0.705882353,1.0},{0.815686275,0.125490196,0.564705882,1.0},{0.5,0.5,0.5,1.0},
{0.980392157,0.5,0.456,1.0},{0.549019608,0.5,0.5,1.0},{0.980392157,0.843137255,0.0,1.0},{0.094117647,0.545098039,0.094117647,1.0}};

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
	for (int i=0; i<6; i++) {
		tabInterfaces[i] = new P2PNetworkInterface(this);
		getP2PNetworkInterfaceList().push_back(tabInterfaces[i]);
	}
	vm = new BlinkyBlocksVM(this);
	buildNewBlockCode = blinkyBlocksBlockCodeBuildingFunction;
	blockCode = (BaseSimulator::BlockCode*)buildNewBlockCode(this);
	clock = new LinearDriftClock(Clock::XMEGA_RTC_OSC1K_CRC, blockId);
}

BlinkyBlocksBlock::~BlinkyBlocksBlock() {
	OUTPUT << "BlinkyBlocksBlock destructor " << blockId << endl;
	killVM();
}

void BlinkyBlocksBlock::setPosition(const Vecteur &p) {
	position=p;
	getWorld()->updateGlData(this);
}

void BlinkyBlocksBlock::setColor(const Vecteur &c) {
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

void BlinkyBlocksBlock::tap(uint64_t date) {
	OUTPUT << "tap scheduled" << endl;
	getScheduler()->scheduleLock(new VMTapEvent(date, this));
}

void BlinkyBlocksBlock::accel(uint64_t date, int x, int y, int z) {
	getScheduler()->scheduleLock(new VMAccelEvent(date, this, x, y, z));
}

void BlinkyBlocksBlock::shake(uint64_t date, int f) {
	getScheduler()->scheduleLock(new VMShakeEvent(getScheduler()->now(), this, f));
}

void BlinkyBlocksBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
	OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on " << NeighborDirection::getString(getDirection(ni)) << endl;
	getScheduler()->scheduleLock(new VMAddNeighborEvent(getScheduler()->now(), this, NeighborDirection::getOpposite(getDirection(ni)), target->blockId));
}

void BlinkyBlocksBlock::removeNeighbor(P2PNetworkInterface *ni) {
	OUTPUT << "Simulator: "<< blockId << " remove neighbor on " << NeighborDirection::getString(getDirection(ni)) << endl;
	getScheduler()->scheduleLock(new VMRemoveNeighborEvent(getScheduler()->now(), this, NeighborDirection::getOpposite(getDirection(ni))));
}

void BlinkyBlocksBlock::stop(uint64_t date, State s) {
	OUTPUT << "Simulator: stop scheduled" << endl;
	lock();
	state = s;
	if (s == STOPPED) {
		// patch en attendant l'objet 3D qui modelise un BB stopped
		color = Vecteur(0.1, 0.1, 0.1, 0.5);
	}
	unlock();
	getWorld()->updateGlData(this);
	getScheduler()->scheduleLock(new VMStopEvent(getScheduler()->now(), this));
}

void BlinkyBlocksBlock::lockVM() {
	if (BlinkyBlocksVM::isInDebuggingMode()) {
		mutex_vm.lock();
	}
}

void BlinkyBlocksBlock::unlockVM() {
	if (BlinkyBlocksVM::isInDebuggingMode()) {
		mutex_vm.unlock();
	}
}

int BlinkyBlocksBlock::sendCommand(VMCommand &c) {
	int ret = 0;
	lockVM();
	if(vm != NULL) {
		if ((state == ALIVE) || (c.getType() == VM_COMMAND_STOP)) {
			ret = vm->sendCommand(c);
		}
	}
	unlockVM();
	return ret;
}

void BlinkyBlocksBlock::killVM() {
	lockVM();
	if(vm != NULL) {
		delete vm;
		vm = NULL;
	}
	unlockVM();
}

std::ostream& operator<<(std::ostream &stream, BlinkyBlocksBlock const& bb) {
  stream << bb.blockId << "\tcolor: " << bb.color;
  return stream;
}

}
