/*
 * blinkyBlocksBlock.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>

#include "utils/tDefs.h"
#include "robots/blinkyBlocks/blinkyBlocksBlock.h"
#include "base/buildingBlock.h"
#include "robots/blinkyBlocks/blinkyBlocksWorld.h"
#include "robots/blinkyBlocks/blinkyBlocksSimulator.h"
#include "robots/blinkyBlocks/blinkyBlocksEvents.h"
#include "utils/trace.h"
#include "clock/clock.h"
#include "meld/meldInterpretEvents.h"
#include "grid/lattice.h"
#include "motion/teleportationEvents.h"

using namespace std;

#define BLINKYBLOCKS_PACKET_DATASIZE 17
#define BLINKYBLOCKS_TRANSMISSION_MIN_TIME 6.08
#define BLINKYBLOCKS_TRANSMISSION_MAX_TIME 6.11

namespace BlinkyBlocks {

  BlinkyBlocksBlock::BlinkyBlocksBlock(int bId, BlockCodeBuilder bcb)
    : BaseSimulator::BuildingBlock(bId, bcb, SCLattice::MAX_NB_NEIGHBORS) {
#ifdef DEBUG_OBJECT_LIFECYCLE
      OUTPUT << "BlinkyBlocksBlock constructor" << endl;
#endif

    double dataRateMin = ((BLINKYBLOCKS_PACKET_DATASIZE*pow(10,6)*8) / (BLINKYBLOCKS_TRANSMISSION_MAX_TIME*1000));
    double dataRateMax = ((BLINKYBLOCKS_PACKET_DATASIZE*pow(10,6)*8)  / (BLINKYBLOCKS_TRANSMISSION_MIN_TIME*1000));

    for (int i = 0; i < SCLattice::MAX_NB_NEIGHBORS; i++) {
      P2PNetworkInterface *p2p = P2PNetworkInterfaces[i];
      doubleRNG g = Random::getUniformDoubleRNG(getRandomUint(),dataRateMin,dataRateMax);
      RandomRate *r = new RandomRate(g);
      p2p->setDataRate(r);
    }
}

BlinkyBlocksBlock::~BlinkyBlocksBlock() {
    OUTPUT << "BlinkyBlocksBlock destructor " << blockId << endl;
}

void BlinkyBlocksBlock::pauseClock(Time delay, Time start) {
    //while(BaseSimulator::getScheduler()->now()<delay+start){

}

int BlinkyBlocksBlock::getDirection(P2PNetworkInterface *given_interface) const {
    if( !given_interface) {
        return SCLattice::Direction(0);
    }

    for(int i(0); i < 6; ++i) {
        if(P2PNetworkInterfaces[i] == given_interface) return SCLattice::Direction(i);
    }

    return SCLattice::Direction(0);
}

void BlinkyBlocksBlock::accel(Time date, int x, int y, int z) {
    getScheduler()->schedule(new AccelEvent(date, this, x, y, z));
}

void BlinkyBlocksBlock::shake(Time date, int f) {
    getScheduler()->schedule(new ShakeEvent(getScheduler()->now(), this, f));
}

void BlinkyBlocksBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(
        new AddNeighborEvent(getScheduler()->now(), this,
                             getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
}

void BlinkyBlocksBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(
        new RemoveNeighborEvent(getScheduler()->now(), this,
                                getWorld()->lattice->getOppositeDirection(getDirection(ni))));
}

void BlinkyBlocksBlock::stopBlock(Time date, State s) {
    OUTPUT << "Simulator: stop scheduled" << endl;
    setState(s);
    if (s == STOPPED) {
        // patch en attendant l'objet 3D qui modelise un BB stopped
        color = Color(0.1, 0.1, 0.1, 0.5);
				getWorld()->updateGlData(this,color);
		}


    if (BaseSimulator::Simulator::getType() == BaseSimulator::Simulator::MELDINTERPRET) {
        getScheduler()->schedule(new MeldInterpret::VMStopEvent(getScheduler()->now(), this));
    }
}

bool BlinkyBlocksBlock::moveTo(const Cell3DPosition& dest)  {
    cerr << "(warning) " << *this << " attempting to move to " << dest
         << " even though BlinkyBlocks has no motion capability. Teleporting... " << endl;

    getScheduler()->schedule(
        new TeleportationStartEvent(getScheduler()->now(), this, dest));

    return true;
}

std::ostream& operator<<(std::ostream &stream, BlinkyBlocksBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color;
    return stream;
}

}
