/*
 * robotBlocksEvents.cpp
 *
 *  Created on: 2014 fevrary 1st
 *      Author: Benoît
 */

#include "robotBlocksEvents.h"
#include "robotBlocksScheduler.h"
#include "robotBlocksWorld.h"

const int ANIMATION_DELAY=40000;
const int COM_DELAY=2000;

const double EPS=1E-5;
namespace RobotBlocks {

//===========================================================================================================
//
//          MotionStartEvent  (class)
//
//===========================================================================================================

MotionStartEvent::MotionStartEvent(uint64_t t, RobotBlocksBlock *block,const Vector3D &fpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_MOTION_START;
    finalPosition = fpos;
}

MotionStartEvent::MotionStartEvent(MotionStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

MotionStartEvent::~MotionStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void MotionStartEvent::consume() {
    EVENT_CONSUME_INFO();
    RobotBlocksScheduler *scheduler = RobotBlocks::getScheduler();
    RobotBlocksBlock *rb = (RobotBlocksBlock *)concernedBlock;
    RobotBlocksWorld::getWorld()->disconnectBlock(rb);
    rb->setColor(DARKGREY);
    Vector3D motionPosition = rb->getPositionVector();
    Vector3D motionStep =  finalPosition - motionPosition;
    motionStep.setLength(0.1);
    scheduler->schedule(new MotionStepEvent(scheduler->now() + ANIMATION_DELAY, rb,
					    finalPosition, motionStep, motionPosition));
}

const string MotionStartEvent::getEventName() {
    return("MotionStart Event");
}

//===========================================================================================================
//
//          MotionStepEvent  (class)
//
//===========================================================================================================

MotionStepEvent::MotionStepEvent(uint64_t t, RobotBlocksBlock *block,
				 const Vector3D &fpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_MOTION_STEP;
    finalPosition = fpos;
    motionStep = finalPosition - motionPosition;
    motionStep.setLength(0.1);
}

MotionStepEvent::MotionStepEvent(uint64_t t, RobotBlocksBlock *block,const Vector3D &fpos,
				 const Vector3D &step, const Vector3D &mpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_MOTION_STEP;
    concernedBlock = block;
    finalPosition = fpos;
    motionStep = step;
    motionPosition = mpos;
}

MotionStepEvent::MotionStepEvent(MotionStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

MotionStepEvent::~MotionStepEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void MotionStepEvent::consume() {
    EVENT_CONSUME_INFO();
    RobotBlocksBlock *rb = (RobotBlocksBlock*)concernedBlock;
    motionPosition += motionStep;
    World::getWorld()->updateGlData(rb, motionPosition);
    RobotBlocksScheduler *scheduler = RobotBlocks::getScheduler();

    double v = (finalPosition - motionPosition) * motionStep;
    if (v<EPS) {
	scheduler->schedule(new MotionStopEvent(scheduler->now() + ANIMATION_DELAY,
						rb, finalPosition));
    } else {
	scheduler->schedule(new MotionStepEvent(scheduler->now() + ANIMATION_DELAY, rb,
						finalPosition, motionStep, motionPosition));
    }
}

const string MotionStepEvent::getEventName() {
    return("MotionStep Event");
}

//===========================================================================================================
//
//          MotionStepEvent  (class)
//
//===========================================================================================================

MotionStopEvent::MotionStopEvent(uint64_t t, RobotBlocksBlock *block,const Vector3D &fpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_MOTION_STOP;
    finalPosition = fpos;
}

MotionStopEvent::MotionStopEvent(MotionStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

MotionStopEvent::~MotionStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void MotionStopEvent::consume() {
    EVENT_CONSUME_INFO();
    RobotBlocksBlock *rb = (RobotBlocksBlock*)concernedBlock;
    rb->setPosition(finalPosition);
    RobotBlocksWorld::getWorld()->updateGlData(rb);
    rb->setColor(YELLOW);
    RobotBlocksWorld *wrld=RobotBlocksWorld::getWorld();
    int ix = rb->position.pt[0],
	iy = rb->position.pt[1],
	iz = rb->position.pt[2];
    wrld->setGridPtr(ix,iy,iz,rb);
    stringstream info;
    info.str("");
    info << "connect Block " << rb->blockId;
    getScheduler()->trace(info.str(),rb->blockId,LIGHTBLUE);
    wrld->connectBlock(rb);
    RobotBlocksScheduler *scheduler = RobotBlocks::getScheduler();
    scheduler->schedule(new MotionEndEvent(scheduler->now() + ANIMATION_DELAY, rb));
}

const string MotionStopEvent::getEventName() {
    return("MotionStop Event");
}

//===========================================================================================================
//
//          MotionEndEvent  (class)
//
//===========================================================================================================

MotionEndEvent::MotionEndEvent(uint64_t t, RobotBlocksBlock *block): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_MOTION_END;
}

MotionEndEvent::MotionEndEvent(MotionEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

MotionEndEvent::~MotionEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void MotionEndEvent::consume() {
    EVENT_CONSUME_INFO();
    RobotBlocksBlock *rb = (RobotBlocksBlock*)concernedBlock;
    concernedBlock->blockCode->processLocalEvent(EventPtr(new MotionEndEvent(date+COM_DELAY,rb)));
}

const string MotionEndEvent::getEventName() {
    return("MotionEnd Event");
}


} // RobotBlocks namespace
