/**
 * @file teleportationEvents.h
 * @brief This file implements the realization of a teleportation event for C3D modules.
 *   Its purpose is to serve as a placeholder motion to test algorithm while proper
 *  Catoms3D movement primitives are being developed.
 *
 *  Created on: 17/11/2017
 *      Author: Pierre Thalamy
 */

#include "nodeMotionEvents.h"
#include "world.h"
#include "utils.h"
#include "nodeBlock.h"

using namespace BaseSimulator::utils;

const int ANIMATION_DELAY=40000;
const int COM_DELAY=0; //

namespace BaseSimulator {

//===========================================================================================================
//
//          NodeMotionStartEvent  (class)
//
//===========================================================================================================

NodeMotionStartEvent::NodeMotionStartEvent(Time t,
																					 BuildingBlock *block,
																					 const Cell3DPosition &fpos, SLattice::Direction forient): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_NODEMOTION_START;
    finalPosition = fpos;
		finalOrientation = forient;
}

NodeMotionStartEvent::NodeMotionStartEvent(NodeMotionStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

NodeMotionStartEvent::~NodeMotionStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void NodeMotionStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    BuildingBlock *bb = concernedBlock;
    World::getWorld()->disconnectBlock(bb);

    Time t = scheduler->now() + ANIMATION_DELAY;
    if (getWorld()->lattice->isInGrid(finalPosition)) {
        bb->blockCode->console << " starting NodeMotion to " << finalPosition
                               << " at " << t << "\n";
        scheduler->schedule(new NodeMotionStopEvent(t, bb, finalPosition,finalOrientation));
    } else {
        OUTPUT << "ERROR: trying to teleport module to a position outside of lattice"
               << endl;
    }
}

const string NodeMotionStartEvent::getEventName() {
    return("NodeMotionStart Event");
}

//===========================================================================================================
//
//          NodeMotionStopEvent  (class)
//
//===========================================================================================================

NodeMotionStopEvent::NodeMotionStopEvent(Time t, BuildingBlock *block,const Cell3DPosition &fpos, SLattice::Direction forient): BlockEvent(t,block) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_NODEMOTION_STOP;
	finalPosition = fpos;
	finalOrientation = forient;
}

NodeMotionStopEvent::~NodeMotionStopEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void NodeMotionStopEvent::consume() {
	EVENT_CONSUME_INFO();

	Node::NodeBlock *bb = (Node::NodeBlock*)concernedBlock;
	World *wrld = getWorld();
	bb->setPositionAndOrientation(finalPosition,finalOrientation);
	wrld->updateGlData(bb);

#ifdef COLOR_MOTION_DEBUG
	bb->setColor(YELLOW);
#endif

	OUTPUT << "connect Block " << bb->blockId << "\n";
	wrld->connectBlock(bb);
	Scheduler *scheduler = getScheduler();
	scheduler->schedule(new NodeMotionEndEvent(scheduler->now(), bb));
}

const string NodeMotionStopEvent::getEventName() {
	return("NodeMotionStop Event");
}

//===========================================================================================================
//
//          NodeMotionEndEvent  (class)
//
//===========================================================================================================

NodeMotionEndEvent::NodeMotionEndEvent(Time t, BuildingBlock *block): BlockEvent(t,block) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_NODEMOTION_END;
}

NodeMotionEndEvent::NodeMotionEndEvent(NodeMotionEndEvent *ev) : BlockEvent(ev) {
	EVENT_CONSTRUCTOR_INFO();
}

NodeMotionEndEvent::~NodeMotionEndEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void NodeMotionEndEvent::consume() {
	EVENT_CONSUME_INFO();
	Node::NodeBlock *bb = (Node::NodeBlock*)concernedBlock;
	bb->blockCode->console << " finished NodeMotion to " << bb->position << "," << bb->orientationCode
													<< " at " << date + COM_DELAY << "\n";
	concernedBlock->blockCode->processLocalEvent(
			EventPtr(new NodeMotionEndEvent(date + COM_DELAY,bb))
			);
	StatsCollector::getInstance().incMotionCount();
	StatsIndividual::incMotionCount(bb->stats);
}

const string NodeMotionEndEvent::getEventName() {
	return("NodeMotionEnd Event");
}


} // Building namespace
