/*
 * @file multiRobotsBlock.cpp
 *
 *  Created on: 14/07/2016
 *      Author: pthalamy
 */

#include <iostream>

#include "multiRobotsBlock.h"

#include "multiRobotsWorld.h"
#include "multiRobotsSimulator.h"
#include "translationEvents.h"
#include "meldInterpretEvents.h"
#include "trace.h"
#include "clock.h"

using namespace std;

namespace MultiRobots {

MultiRobotsBlock::MultiRobotsBlock(int bId, BlockCodeBuilder bcb)
	: BaseSimulator::BuildingBlock(bId, bcb, SLattice::MAX_NB_NEIGHBORS) { // PTHY: INCONSISTENCY
    OUTPUT << "MultiRobotsBlock constructor" << endl;
}

MultiRobotsBlock::~MultiRobotsBlock() {
    OUTPUT << "MultiRobotsBlock destructor " << blockId << endl;
}

SLattice::Direction MultiRobotsBlock::getDirection(P2PNetworkInterface *given_interface) {

    return SLattice::Direction(0);// PTHY: INCONSISTENCY
}

void MultiRobotsBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
		   << SCLattice::getDirectionString(getDirection(ni)) << endl;
    // PTHY: INCONSISTENCY
}

void MultiRobotsBlock::removeNeighbor(P2PNetworkInterface *ni) {
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
		   << SCLattice::getDirectionString(getDirection(ni)) << endl;
    // PTHY: INCONSISTENCY
}

void MultiRobotsBlock::stopBlock(uint64_t date, State s) {
    OUTPUT << "Simulator: stop scheduled" << endl;
    setState(s);
    if (s == STOPPED) {
		// patch en attendant l'objet 3D qui modelise un MR stopped
		color = Color(0.1, 0.1, 0.1, 0.5);
    }

	getWorld()->updateGlData(this);

	if (BaseSimulator::Simulator::getType() == BaseSimulator::Simulator::MELDINTERPRET) {
		getScheduler()->scheduleLock(new MeldInterpret::VMStopEvent(getScheduler()->now(), this));
    }
}

std::ostream& operator<<(std::ostream &stream, MultiRobotsBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color;
    return stream;
}

}
