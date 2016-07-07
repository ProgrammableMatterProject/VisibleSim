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
#include "lattice.h"

using namespace std;

#define BLINKYBLOCKS_PACKET_DATASIZE 17
#define BLINKYBLOCKS_TRANSMISSION_MIN_TIME 6.08
#define BLINKYBLOCKS_TRANSMISSION_MAX_TIME 6.11

namespace BlinkyBlocks {

BlinkyBlocksBlock::BlinkyBlocksBlock(int bId, BlockCodeBuilder bcb)
	: BaseSimulator::BuildingBlock(bId, bcb, SCLattice::MAX_NB_NEIGHBORS) {
    OUTPUT << "BlinkyBlocksBlock constructor" << endl;
    double dataRateMin = ((BLINKYBLOCKS_PACKET_DATASIZE*pow(10,6)*8)
						  / (BLINKYBLOCKS_TRANSMISSION_MAX_TIME*1000));
    double dataRateMax = ((BLINKYBLOCKS_PACKET_DATASIZE*pow(10,6)*8)
						  / (BLINKYBLOCKS_TRANSMISSION_MIN_TIME*1000));
	
    for (int i = 0; i < SCLattice::MAX_NB_NEIGHBORS; i++) {
		P2PNetworkInterface *p2p = P2PNetworkInterfaces[i];
		p2p->setDataRate((dataRateMax+dataRateMin)/2);
		p2p->setDataRateVariability((dataRateMax-dataRateMin)/2);
    }

    clock = new Clock(Clock::XMEGA_RTC_OSC1K_CRC, this);
}

BlinkyBlocksBlock::~BlinkyBlocksBlock() {
    OUTPUT << "BlinkyBlocksBlock destructor " << blockId << endl;
}

void BlinkyBlocksBlock::pauseClock(uint64_t delay, uint64_t start) {
    //while(BaseSimulator::getScheduler()->now()<delay+start){

}
    
SCLattice::Direction BlinkyBlocksBlock::getDirection(P2PNetworkInterface *given_interface) {
    if( !given_interface) {
		return SCLattice::Direction(0);
    }

	for(int i(0); i < 6; ++i) {
		if(P2PNetworkInterfaces[i] == given_interface) return SCLattice::Direction(i);
    }

    return SCLattice::Direction(0);
}

void BlinkyBlocksBlock::accel(uint64_t date, int x, int y, int z) {
    getScheduler()->scheduleLock(new AccelEvent(date, this, x, y, z));
}

void BlinkyBlocksBlock::shake(uint64_t date, int f) {
    getScheduler()->scheduleLock(new ShakeEvent(getScheduler()->now(), this, f));
}

void BlinkyBlocksBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
		   << SCLattice::getDirectionString(getDirection(ni)) << endl;
    getScheduler()->scheduleLock(
		new AddNeighborEvent(getScheduler()->now(), this,
							 SCLattice::getOppositeDirection(getDirection(ni)), target->blockId));
}

void BlinkyBlocksBlock::removeNeighbor(P2PNetworkInterface *ni) {
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
		   << SCLattice::getDirectionString(getDirection(ni)) << endl;
    getScheduler()->scheduleLock(
		new RemoveNeighborEvent(getScheduler()->now(), this,
								SCLattice::getOppositeDirection(getDirection(ni))));
}

void BlinkyBlocksBlock::stopBlock(uint64_t date, State s) {
    OUTPUT << "Simulator: stop scheduled" << endl;
    setState(s);
    if (s == STOPPED) {
		// patch en attendant l'objet 3D qui modelise un BB stopped
		color = Color(0.1, 0.1, 0.1, 0.5);
    }

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

}
