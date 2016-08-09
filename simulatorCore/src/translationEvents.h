/**
 * @file translationEvents.h
 * @brief This file implements the realization of a translation motion events
 *  (created from refactoring of smartBlocksEvents and robotBlocksEvents)
 *
 *  Created on: 07/07/16
 *      Author: Pierre
 */

#ifndef TRANSLATIONEVENTS_H_
#define TRANSLATIONEVENTS_H_

#include "buildingBlock.h"
#include "events.h"

namespace BaseSimulator {

//===========================================================================================================
//
//          TranslationStartEvent  (class)
//
//===========================================================================================================

class TranslationStartEvent : public BlockEvent {
    Vector3D finalPosition;
public:
    TranslationStartEvent(Time, BuildingBlock *block,const Vector3D &fpos);
    TranslationStartEvent(TranslationStartEvent *ev);
    ~TranslationStartEvent();
    void consumeBlockEvent() {};
    void consume();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          TranslationStepEvent  (class)
//
//===========================================================================================================

class TranslationStepEvent : public BlockEvent {
    Vector3D finalPosition;	//!< Target position of the block
    Vector3D motionStep;		//!< Translation of the block during this step
    Vector3D motionPosition;		//!< Actual position on the grid of the block in motion
public:
    TranslationStepEvent(Time, BuildingBlock *block,const Vector3D &fpos);
    TranslationStepEvent(Time, BuildingBlock *block,const Vector3D &fpos,
		    const Vector3D &step, const Vector3D &mpos);
    TranslationStepEvent(TranslationStepEvent *ev);
    ~TranslationStepEvent();
    void consumeBlockEvent() {};
    void consume();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          TranslationStopEvent  (class)
//
//===========================================================================================================

class TranslationStopEvent : public BlockEvent {
    Vector3D finalPosition;
public:
    TranslationStopEvent(Time, BuildingBlock *block,const Vector3D &fpos);
    TranslationStopEvent(TranslationStepEvent *ev);
    ~TranslationStopEvent();
    void consumeBlockEvent() {};
    void consume();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          TranslationEndEvent  (class)
//
//===========================================================================================================

class TranslationEndEvent : public BlockEvent {
public:
    TranslationEndEvent(Time, BuildingBlock *block);
    TranslationEndEvent(TranslationEndEvent *ev);
    ~TranslationEndEvent();
    void consumeBlockEvent() {};
    void consume();
    const virtual string getEventName();
};

}
#endif /* TRANSLATIONEVENTS_H_ */
