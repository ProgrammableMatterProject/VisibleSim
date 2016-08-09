/**
 * @file translationEvents.cpp
 * @brief This file implements the realization of a translation motion events
 *  (created from refactoring of smartBlocksEvents and robotBlocksEvents)
 *
 *  Created on: 07/07/16
 *      Author: Pierre
 */

#include "translationEvents.h"
#include "world.h"
#include "utils.h"

using namespace BaseSimulator::utils;

const int ANIMATION_DELAY=40000;
const int COM_DELAY=2000;

namespace BaseSimulator {

//===========================================================================================================
//
//          TranslationStartEvent  (class)
//
//===========================================================================================================

TranslationStartEvent::TranslationStartEvent(Time t, BuildingBlock *block,const Vector3D &fpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_TRANSLATION_START;
    finalPosition = fpos;
}

TranslationStartEvent::TranslationStartEvent(TranslationStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

TranslationStartEvent::~TranslationStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void TranslationStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    BuildingBlock *bb = concernedBlock;
    World::getWorld()->disconnectBlock(bb);
    bb->setColor(DARKGREY);

    Time t = scheduler->now() + ANIMATION_DELAY;
    Vector3D motionPosition = bb->getPositionVector();
    Vector3D motionStep =  finalPosition - motionPosition;
    motionStep.setLength(0.1);
    scheduler->schedule(new TranslationStepEvent(t, bb, finalPosition, motionStep, motionPosition));
}

const string TranslationStartEvent::getEventName() {
    return("TranslationStart Event");
}

//===========================================================================================================
//
//          TranslationStepEvent  (class)
//
//===========================================================================================================

TranslationStepEvent::TranslationStepEvent(Time t, BuildingBlock *block,
				 const Vector3D &fpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_TRANSLATION_STEP;
    finalPosition = fpos;
    motionStep = finalPosition - motionPosition;
    motionStep.setLength(0.1);
}

TranslationStepEvent::TranslationStepEvent(Time t, BuildingBlock *block,const Vector3D &fpos,
				 const Vector3D &step, const Vector3D &mpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_TRANSLATION_STEP;
    concernedBlock = block;
    finalPosition = fpos;
    motionStep = step;
    motionPosition = mpos;
}

TranslationStepEvent::TranslationStepEvent(TranslationStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

TranslationStepEvent::~TranslationStepEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void TranslationStepEvent::consume() {
    EVENT_CONSUME_INFO();
    World *wrl = World::getWorld();
    Vector3D gridScale = wrl->lattice->gridScale;
    BuildingBlock *bb = (BuildingBlock*)concernedBlock;
    motionPosition += motionStep;
    Vector3D motionGlPos(motionPosition[0] * gridScale[0],
			 motionPosition[1] * gridScale[1],
			 motionPosition[2] * gridScale[2]);
    wrl->updateGlData(bb, motionGlPos);
    Scheduler *scheduler = getScheduler();

    double v = (finalPosition - motionPosition) * motionStep;
    if (v<EPS) {
        scheduler->schedule(new TranslationStopEvent(scheduler->now() + ANIMATION_DELAY,
                                                bb, finalPosition));
    } else {
        scheduler->schedule(new TranslationStepEvent(scheduler->now() + ANIMATION_DELAY, bb,
                                                finalPosition, motionStep, motionPosition));
    }
}

const string TranslationStepEvent::getEventName() {
    return("TranslationStep Event");
}

//===========================================================================================================
//
//          TranslationStepEvent  (class)
//
//===========================================================================================================

TranslationStopEvent::TranslationStopEvent(Time t, BuildingBlock *block,const Vector3D &fpos): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_TRANSLATION_STOP;
    finalPosition = fpos;
}

TranslationStopEvent::TranslationStopEvent(TranslationStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

TranslationStopEvent::~TranslationStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void TranslationStopEvent::consume() {
    EVENT_CONSUME_INFO();


    BuildingBlock *bb = concernedBlock;
    World *wrld = getWorld();
    bb->setPosition(Cell3DPosition(finalPosition.pt[0], finalPosition.pt[1], finalPosition.pt[2]));
    wrld->updateGlData(bb);
    bb->setColor(YELLOW);

    stringstream info;
    info.str("");
    info << "connect Block " << bb->blockId;
    getScheduler()->trace(info.str(),bb->blockId,LIGHTBLUE);
    wrld->connectBlock(bb);
    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new TranslationEndEvent(scheduler->now() + ANIMATION_DELAY, bb));
}

const string TranslationStopEvent::getEventName() {
    return("TranslationStop Event");
}

//===========================================================================================================
//
//          TranslationEndEvent  (class)
//
//===========================================================================================================

TranslationEndEvent::TranslationEndEvent(Time t, BuildingBlock *block): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_TRANSLATION_END;
}

TranslationEndEvent::TranslationEndEvent(TranslationEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

TranslationEndEvent::~TranslationEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void TranslationEndEvent::consume() {
    EVENT_CONSUME_INFO();
    BuildingBlock *bb = concernedBlock;
    concernedBlock->blockCode->processLocalEvent(EventPtr(new TranslationEndEvent(date + COM_DELAY,bb)));
    StatsCollector::getInstance().incMotionCount();
}

const string TranslationEndEvent::getEventName() {
    return("TranslationEnd Event");
}


} // Building namespace
