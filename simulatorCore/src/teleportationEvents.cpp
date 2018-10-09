/**
 * @file teleportationEvents.h
 * @brief This file implements the realization of a teleportation event for C3D modules.
 *   Its purpose is to serve as a placeholder motion to test algorithm while proper
 *  Catoms3D movement primitives are being developed.
 *
 *  Created on: 17/11/2017
 *      Author: Pierre Thalamy
 */

#include "teleportationEvents.h"
#include "world.h"
#include "catoms3DWorld.h"
#include "utils.h"

using namespace BaseSimulator::utils;

const int ANIMATION_DELAY=40000;
const int COM_DELAY=2000;

namespace BaseSimulator {

//===========================================================================================================
//
//          TeleportationStartEvent  (class)
//
//===========================================================================================================

// TeleportationStartEvent::TeleportationStartEvent(Time t,
//                                                  BuildingBlock *block,
//                                                  BuildingBlock *pivot,
//                                                  MovementDirection mDir): BlockEvent(t,block) {
TeleportationStartEvent::TeleportationStartEvent(Time t,
                                                 BuildingBlock *block,
                                                 const Cell3DPosition &fpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_TELEPORTATION_START;
    finalPosition = fpos;
}

TeleportationStartEvent::TeleportationStartEvent(TeleportationStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

TeleportationStartEvent::~TeleportationStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void TeleportationStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    BuildingBlock *bb = concernedBlock;
    World::getWorld()->disconnectBlock(bb);

    Time t = scheduler->now() + ANIMATION_DELAY;
    if (getWorld()->lattice->isInGrid(finalPosition)) {
        scheduler->schedule(new TeleportationStopEvent(t, bb, finalPosition));
    } else {
        OUTPUT << "ERROR: trying to teleport module to a position outside of lattice"
               << endl;
    }
}

const string TeleportationStartEvent::getEventName() {
    return("TeleportationStart Event");
}

//===========================================================================================================
//
//          TeleportationStopEvent  (class)
//
//===========================================================================================================

TeleportationStopEvent::TeleportationStopEvent(Time t, BuildingBlock *block,const Cell3DPosition &fpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_TELEPORTATION_STOP;
    finalPosition = fpos;
}

TeleportationStopEvent::~TeleportationStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void TeleportationStopEvent::consume() {
    EVENT_CONSUME_INFO();

    BuildingBlock *bb = concernedBlock;
    World *wrld = getWorld();
    bb->setPosition(finalPosition);
    wrld->updateGlData(bb);

#ifdef COLOR_MOTION_DEBUG
    bb->setColor(YELLOW);
#endif

    OUTPUT << "connect Block " << bb->blockId << "\n";
    wrld->connectBlock(bb);
    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new TeleportationEndEvent(scheduler->now() + ANIMATION_DELAY, bb));
}

const string TeleportationStopEvent::getEventName() {
    return("TeleportationStop Event");
}

//===========================================================================================================
//
//          TeleportationEndEvent  (class)
//
//===========================================================================================================

TeleportationEndEvent::TeleportationEndEvent(Time t, BuildingBlock *block): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_TELEPORTATION_END;
}

TeleportationEndEvent::TeleportationEndEvent(TeleportationEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

TeleportationEndEvent::~TeleportationEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void TeleportationEndEvent::consume() {
    EVENT_CONSUME_INFO();
    BuildingBlock *bb = concernedBlock;
    concernedBlock->blockCode->processLocalEvent(
        EventPtr(new TeleportationEndEvent(date + COM_DELAY,bb))
        );
    StatsCollector::getInstance().incMotionCount();
    StatsIndividual::incMotionCount(bb->stats);
}

const string TeleportationEndEvent::getEventName() {
    return("TeleportationEnd Event");
}


} // Building namespace
