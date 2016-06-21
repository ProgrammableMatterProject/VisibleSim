/*
 * robotBlocksEvents.cpp
 *
 *  Created on: 2014 fevrary 1st
 *      Author: Benoît
 */

#include "smartBlocksEvents.h"
#include "smartBlocksScheduler.h"
#include "smartBlocksWorld.h"

//const int ANIMATION_DELAY=20000; // 20ms x 5
const int ANIMATION_DELAY=100000; // 100ms
const int COM_DELAY=2000; // 2ms

const double EPS=1E-5;
namespace SmartBlocks {

//===========================================================================================================
//
//          MotionStartEvent  (class)
//
//===========================================================================================================

MotionStartEvent::MotionStartEvent(uint64_t t, SmartBlocksBlock *block,const Vector3D &fpos): BlockEvent(t,block) {
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
    SmartBlocksScheduler *scheduler = SmartBlocks::getScheduler();
    SmartBlocksBlock *sb = (SmartBlocksBlock *)concernedBlock;
    SmartBlocksWorld::getWorld()->disconnectBlock(sb);
    sb->setColor(DARKGREY);
    uint64_t t=scheduler->now() + ANIMATION_DELAY;
    Vector3D motionPosition = sb->getPositionVector();
    Vector3D motionStep = finalPosition - motionPosition;
    motionStep.setLength(0.2);
    scheduler->schedule(new MotionStepEvent(t, sb,finalPosition, motionStep, motionPosition));
//	OUTPUT << "Schedule MotionStepEvent(" << concernedBlock->blockId << "," << t <<"," << finalPosition << ")" << endl;
}

const string MotionStartEvent::getEventName() {
    return("MotionStart Event");
}

//===========================================================================================================
//
//          MotionStepEvent  (class)
//
//===========================================================================================================

MotionStepEvent::MotionStepEvent(uint64_t t, SmartBlocksBlock *block,
				 const Vector3D &fpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_MOTION_STEP;
    finalPosition = fpos;
    motionStep = finalPosition - motionPosition;
    motionStep.setLength(0.2);
}

MotionStepEvent::MotionStepEvent(uint64_t t, SmartBlocksBlock *block, const Vector3D &fpos,
				 const Vector3D &step, const Vector3D &mpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_MOTION_STEP;
    concernedBlock = block;
    finalPosition = fpos;
    motionPosition = mpos;
    motionStep = step;
}

MotionStepEvent::MotionStepEvent(MotionStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

MotionStepEvent::~MotionStepEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void MotionStepEvent::consume() {
    EVENT_CONSUME_INFO();
    SmartBlocksBlock *rb = (SmartBlocksBlock*)concernedBlock;
    motionPosition += motionStep;
    World::getWorld()->updateGlData(rb, motionPosition);
    SmartBlocksScheduler *scheduler = SmartBlocks::getScheduler();

    // OUTPUT << rb->blockId << ":" << scheduler->now()<< endl;
    double v = (finalPosition - motionPosition) * motionStep;
    if (v<EPS) {
	scheduler->schedule(new MotionStopEvent(scheduler->now() + ANIMATION_DELAY, rb,finalPosition));
    } else {
	scheduler->schedule(new MotionStepEvent(scheduler->now() + ANIMATION_DELAY, rb,
						finalPosition,motionStep,motionPosition));
    }
//	OUTPUT << "MotionStepEvent(" << concernedBlock->blockId << "," << rb->position << ")" << endl;
}

const string MotionStepEvent::getEventName() {
    return("MotionStep Event");
}

//===========================================================================================================
//
//          MotionStepEvent  (class)
//
//===========================================================================================================

MotionStopEvent::MotionStopEvent(uint64_t t, SmartBlocksBlock *block,const Vector3D &fpos): BlockEvent(t,block) {
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
    SmartBlocksBlock *rb = (SmartBlocksBlock*)concernedBlock;
    rb->setPosition(finalPosition);
    World::getWorld()->updateGlData(rb);
    rb->setColor(YELLOW);
    SmartBlocksWorld *wrld=SmartBlocksWorld::getWorld();
/*
  stringstream info;
  info.str("");
  info << "connect Block " << rb->blockId;
  getScheduler()->trace(info.str(),rb->blockId,LIGHTBLUE);*/
    wrld->connectBlock(rb);
    SmartBlocksScheduler *scheduler = SmartBlocks::getScheduler();
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

MotionEndEvent::MotionEndEvent(uint64_t t, SmartBlocksBlock *block): BlockEvent(t,block) {
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
    SmartBlocksBlock *rb = (SmartBlocksBlock*)concernedBlock;
    concernedBlock->blockCode->processLocalEvent(EventPtr(new MotionEndEvent(date+COM_DELAY,rb)));
}

const string MotionEndEvent::getEventName() {
    return("MotionEnd Event");
}

} // SmartBlocks namespace
