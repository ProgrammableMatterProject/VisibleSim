/**
 * @file teleportationEvents.h
 * @brief This file implements the realization of a teleportation event for C3D modules.
 *   Its purpose is to serve as a placeholder motion to test algorithm while proper
 *  Catoms3D movement primitives are being developed.
 *
 *  Created on: 17/11/2017
 *      Author: Pierre Thalamy
 */

#include "hexanodeMotionEvents.h"
#include "world.h"
#include "utils.h"
#include "hexanodeBlock.h"

using namespace BaseSimulator::utils;

const int ANIMATION_DELAY=40000;
const int COM_DELAY=0; //

namespace BaseSimulator {

//===========================================================================================================
//
//          HexanodeMotionStartEvent  (class)
//
//===========================================================================================================

HexanodeMotionStartEvent::HexanodeMotionStartEvent(Time t,
                                                   BuildingBlock *block,
                                                   const Cell3DPosition &fpos, HHLattice::Direction forient): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_NODEMOTION_START;
    finalPosition = fpos;
    finalOrientation = forient;
}

HexanodeMotionStartEvent::HexanodeMotionStartEvent(HexanodeMotionStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

HexanodeMotionStartEvent::~HexanodeMotionStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void HexanodeMotionStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    BuildingBlock *bb = concernedBlock;
    World::getWorld()->disconnectBlock(bb);

    Time t = scheduler->now() + ANIMATION_DELAY;
    if (getWorld()->lattice->isInGrid(finalPosition)) {
        bb->blockCode->console << " starting HexanodeMotion to " << finalPosition
                               << " at " << t << "\n";
        scheduler->schedule(new HexanodeMotionStopEvent(t, bb, finalPosition,finalOrientation));
    } else {
        OUTPUT << "ERROR: trying to teleport module to a position outside of lattice"
               << endl;
    }
}

const string HexanodeMotionStartEvent::getEventName() {
    return("HexanodeMotionStart Event");
}

//===========================================================================================================
//
//          HexanodeMotionStopEvent  (class)
//
//===========================================================================================================

HexanodeMotionStopEvent::HexanodeMotionStopEvent(Time t, BuildingBlock *block,const Cell3DPosition &fpos, HHLattice::Direction forient): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_NODEMOTION_STOP;
    finalPosition = fpos;
    finalOrientation = forient;
}

HexanodeMotionStopEvent::~HexanodeMotionStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void HexanodeMotionStopEvent::consume() {
    EVENT_CONSUME_INFO();

    Hexanode::HexanodeBlock *bb = (Hexanode::HexanodeBlock*)concernedBlock;
    World *wrld = getWorld();
    bb->setPositionAndOrientation(finalPosition,finalOrientation);
    wrld->updateGlData(bb);

#ifdef COLOR_MOTION_DEBUG
    bb->setColor(YELLOW);
#endif

    OUTPUT << "connect Block " << bb->blockId << "\n";
    wrld->connectBlock(bb);
    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new HexanodeMotionEndEvent(scheduler->now(), bb));
}

const string HexanodeMotionStopEvent::getEventName() {
    return("HexanodeMotionStop Event");
}

//===========================================================================================================
//
//          HexanodeMotionEndEvent  (class)
//
//===========================================================================================================

HexanodeMotionEndEvent::HexanodeMotionEndEvent(Time t, BuildingBlock *block): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_NODEMOTION_END;
}

HexanodeMotionEndEvent::HexanodeMotionEndEvent(HexanodeMotionEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

HexanodeMotionEndEvent::~HexanodeMotionEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void HexanodeMotionEndEvent::consume() {
    EVENT_CONSUME_INFO();
    Hexanode::HexanodeBlock *bb = (Hexanode::HexanodeBlock*)concernedBlock;
    bb->blockCode->console << " finished HexanodeMotion to " << bb->position << "," << bb->orientationCode
                           << " at " << date + COM_DELAY << "\n";
    concernedBlock->blockCode->processLocalEvent(
        EventPtr(new HexanodeMotionEndEvent(date + COM_DELAY,bb))
        );
    StatsCollector::getInstance().incMotionCount();
    StatsIndividual::incMotionCount(bb->stats);
}

const string HexanodeMotionEndEvent::getEventName() {
    return("HexanodeMotionEnd Event");
}


} // Building namespace
