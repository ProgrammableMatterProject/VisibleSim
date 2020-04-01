/**
 * @file teleportationEvents.h
 * @brief This file implements the realization of a teleportation event for C3D modules.
 *   Its purpose is to serve as a placeholder motion to test algorithm while proper
 *  Catoms3D movement primitives are being developed.
 *
 *  Created on: 17/11/2017
 *      Author: Pierre Thalamy
 */

#include "robots/nodes2D/nodes2DMotionEvents.h"
#include "base/world.h"
#include "utils/utils.h"
#include "robots/nodes2D/nodes2DBlock.h"

using namespace BaseSimulator::utils;

const int ANIMATION_DELAY=40000;
const int COM_DELAY=0; //

namespace BaseSimulator {

//===========================================================================================================
//
//          Nodes2DMotionStartEvent  (class)
//
//===========================================================================================================

Nodes2DMotionStartEvent::Nodes2DMotionStartEvent(Time t,
                                                                                     BuildingBlock *block,
                                                                                     const Cell3DPosition &fpos, SLattice::Direction forient): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_NODES2DMOTION_START;
    finalPosition = fpos;
        finalOrientation = forient;
}

Nodes2DMotionStartEvent::Nodes2DMotionStartEvent(Nodes2DMotionStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Nodes2DMotionStartEvent::~Nodes2DMotionStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Nodes2DMotionStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    BuildingBlock *bb = concernedBlock;
    World::getWorld()->disconnectBlock(bb);

    Time t = scheduler->now() + ANIMATION_DELAY;
    if (getWorld()->lattice->isInGrid(finalPosition)) {
        bb->blockCode->console << " starting Nodes2DMotion to " << finalPosition
                               << " at " << t << "\n";
        scheduler->schedule(new Nodes2DMotionStopEvent(t, bb, finalPosition,finalOrientation));
    } else {
        OUTPUT << "ERROR: trying to teleport module to a position outside of lattice"
               << endl;
    }
}

const string Nodes2DMotionStartEvent::getEventName() {
    return("Nodes2DMotionStart Event");
}

//===========================================================================================================
//
//          Nodes2DMotionStopEvent  (class)
//
//===========================================================================================================

Nodes2DMotionStopEvent::Nodes2DMotionStopEvent(Time t, BuildingBlock *block,const Cell3DPosition &fpos, SLattice::Direction forient): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_NODES2DMOTION_STOP;
    finalPosition = fpos;
    finalOrientation = forient;
}

Nodes2DMotionStopEvent::~Nodes2DMotionStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Nodes2DMotionStopEvent::consume() {
    EVENT_CONSUME_INFO();

    Nodes2D::Nodes2DBlock *bb = (Nodes2D::Nodes2DBlock*)concernedBlock;
    World *wrld = getWorld();
    bb->setPositionAndOrientation(finalPosition,finalOrientation);
    wrld->updateGlData(bb);

#ifdef COLOR_MOTION_DEBUG
    bb->setColor(YELLOW);
#endif

    OUTPUT << "connect Block " << bb->blockId << "\n";
    wrld->connectBlock(bb);
    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new Nodes2DMotionEndEvent(scheduler->now(), bb));
}

const string Nodes2DMotionStopEvent::getEventName() {
    return("Nodes2DMotionStop Event");
}

//===========================================================================================================
//
//          Nodes2DMotionEndEvent  (class)
//
//===========================================================================================================

Nodes2DMotionEndEvent::Nodes2DMotionEndEvent(Time t, BuildingBlock *block): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_NODES2DMOTION_END;
}

Nodes2DMotionEndEvent::Nodes2DMotionEndEvent(Nodes2DMotionEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Nodes2DMotionEndEvent::~Nodes2DMotionEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Nodes2DMotionEndEvent::consume() {
    EVENT_CONSUME_INFO();
    Nodes2D::Nodes2DBlock *bb = (Nodes2D::Nodes2DBlock*)concernedBlock;
    bb->blockCode->console << " finished Nodes2DMotion to " << bb->position << "," << bb->orientationCode
                                                    << " at " << date + COM_DELAY << "\n";
    concernedBlock->blockCode->processLocalEvent(
            EventPtr(new Nodes2DMotionEndEvent(date + COM_DELAY,bb))
            );
    StatsCollector::getInstance().incMotionCount();
    StatsIndividual::incMotionCount(bb->stats);
}

const string Nodes2DMotionEndEvent::getEventName() {
    return("Nodes2DMotionEnd Event");
}


} // Building namespace
