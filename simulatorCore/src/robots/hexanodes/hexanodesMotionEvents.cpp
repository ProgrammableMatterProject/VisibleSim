/**
 * @file teleportationEvents.h
 * @brief This file implements the realization of a teleportation event for C3D modules.
 *   Its purpose is to serve as a placeholder motion to test algorithm while proper
 *  Catoms3D movement primitives are being developed.
 *
 *  Created on: 17/11/2017
 *      Author: Pierre Thalamy
 */

#include "hexanodesMotionEvents.h"
#include "../../base/world.h"
#include "../../utils/utils.h"
#include "hexanodesBlock.h"

using namespace BaseSimulator::utils;

const int ANIMATION_DELAY=40000;
const int COM_DELAY=0; //

namespace BaseSimulator {

//===========================================================================================================
//
//          HexanodesMotionStartEvent  (class)
//
//===========================================================================================================

HexanodesMotionStartEvent::HexanodesMotionStartEvent(Time t,
                                                   BuildingBlock *block,
                                                   const Cell3DPosition &fpos, HHLattice::Direction forient): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_HEXANODESMOTION_START;
    finalPosition = fpos;
    finalOrientation = forient;
}

HexanodesMotionStartEvent::HexanodesMotionStartEvent(HexanodesMotionStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

HexanodesMotionStartEvent::~HexanodesMotionStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void HexanodesMotionStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    BuildingBlock *bb = concernedBlock;
    World::getWorld()->disconnectBlock(bb);

    Time t = scheduler->now() + ANIMATION_DELAY;
    if (getWorld()->lattice->isInGrid(finalPosition)) {
        bb->blockCode->console << " starting HexanodesMotion to " << finalPosition
                               << " at " << t << "\n";
        scheduler->schedule(new HexanodesMotionStopEvent(t, bb, finalPosition,finalOrientation));
    } else {
        OUTPUT << "ERROR: trying to teleport module to a position outside of lattice"
               << endl;
    }
}

const string HexanodesMotionStartEvent::getEventName() {
    return("HexanodesMotionStart Event");
}

//===========================================================================================================
//
//          HexanodesMotionStopEvent  (class)
//
//===========================================================================================================

HexanodesMotionStopEvent::HexanodesMotionStopEvent(Time t, BuildingBlock *block,const Cell3DPosition &fpos, HHLattice::Direction forient): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_HEXANODESMOTION_STOP;
    finalPosition = fpos;
    finalOrientation = forient;
}

HexanodesMotionStopEvent::~HexanodesMotionStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void HexanodesMotionStopEvent::consume() {
    EVENT_CONSUME_INFO();

    Hexanodes::HexanodesBlock *bb = (Hexanodes::HexanodesBlock*)concernedBlock;
    World *wrld = getWorld();
    bb->setPositionAndOrientation(finalPosition,finalOrientation);
    wrld->updateGlData(bb,bb->position);

#ifdef COLOR_MOTION_DEBUG
    bb->setColor(YELLOW);
#endif

    OUTPUT << "connect Block " << bb->blockId << "\n";
    wrld->connectBlock(bb);
    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new HexanodesMotionEndEvent(scheduler->now(), bb));
}

const string HexanodesMotionStopEvent::getEventName() {
    return("HexanodesMotionStop Event");
}

//===========================================================================================================
//
//          HexanodesMotionEndEvent  (class)
//
//===========================================================================================================

HexanodesMotionEndEvent::HexanodesMotionEndEvent(Time t, BuildingBlock *block): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_HEXANODESMOTION_END;
}

HexanodesMotionEndEvent::HexanodesMotionEndEvent(HexanodesMotionEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

HexanodesMotionEndEvent::~HexanodesMotionEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void HexanodesMotionEndEvent::consume() {
    EVENT_CONSUME_INFO();
    Hexanodes::HexanodesBlock *bb = (Hexanodes::HexanodesBlock*)concernedBlock;
    bb->blockCode->console << " finished HexanodesMotion to " << bb->position << "," << bb->orientationCode
                           << " at " << date + COM_DELAY << "\n";
    concernedBlock->blockCode->processLocalEvent(
        EventPtr(new HexanodesMotionEndEvent(date + COM_DELAY,bb))
        );
    StatsCollector::getInstance().incMotionCount();
    StatsIndividual::incMotionCount(bb->stats);
}

const string HexanodesMotionEndEvent::getEventName() {
    return("HexanodesMotionEnd Event");
}


} // Building namespace
