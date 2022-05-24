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
#include "../base/world.h"
//#include "../robots/catoms3D/catoms3DWorld.h"
#include "../utils/utils.h"
#include "../replay/replayExporter.h"

using namespace BaseSimulator::utils;

const int ANIMATION_DELAY=40000;
const int COM_DELAY=0; //

namespace BaseSimulator {

//===========================================================================================================
//
//          TeleportationStartEvent  (class)
//
//===========================================================================================================

    TeleportationStartEvent::TeleportationStartEvent(Time t,
                                                     BuildingBlock *block,
                                                     const Cell3DPosition &fpos): BlockEvent(t,block) {
        EVENT_CONSTRUCTOR_INFO();
        eventType = EVENT_TELEPORTATION_START;
        finalPosition = fpos;
        finalOrientation = 0;
    }

    TeleportationStartEvent::TeleportationStartEvent(Time t,
                                                     BuildingBlock *block,
                                                     const Cell3DPosition &fpos,
                                                     uint8_t orient): BlockEvent(t,block) {
        EVENT_CONSTRUCTOR_INFO();
        eventType = EVENT_TELEPORTATION_START;
        finalPosition = fpos;
        finalOrientation = orient;
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
    World::getWorld()->disconnectBlock(bb, false);

    Time t = scheduler->now() + ANIMATION_DELAY;
    if (getWorld()->lattice->isInGrid(finalPosition)) {
        bb->blockCode->console << " starting Teleportation from " << bb->position << " to " << finalPosition
                               << " at " << t << "\n";
        scheduler->schedule(new TeleportationStopEvent(t, bb, finalPosition,finalOrientation));
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

TeleportationStopEvent::TeleportationStopEvent(Time t, BuildingBlock *block,const Cell3DPosition &fpos,uint8_t orient): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_TELEPORTATION_STOP;
    finalPosition = fpos;
    finalOrientation = orient;
}

TeleportationStopEvent::~TeleportationStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void TeleportationStopEvent::consume() {
    EVENT_CONSUME_INFO();

    BuildingBlock *bb = concernedBlock;
    World *wrld = getWorld();
    bb->setPositionAndOrientation(finalPosition,finalOrientation);
    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writePositionUpdate(getScheduler()->now(),
                                                           bb->blockId, bb->position, bb->orientationCode);
    wrld->updateGlData(bb,bb->position);

#ifdef COLOR_MOTION_DEBUG
    bb->setColor(YELLOW);
#endif

    OUTPUT << "connect Block " << bb->blockId << "\n";
    wrld->connectBlock(bb, false);
    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new TeleportationEndEvent(scheduler->now(), bb));
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
    bb->blockCode->console << " finished Teleportation to " << bb->position
                           << " at " << date + COM_DELAY << "\n";
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
