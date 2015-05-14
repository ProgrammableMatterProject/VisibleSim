/*!
 * \file catoms2DEvents.h
 * \brief Motion events for 2D Catoms
 * \date 15/02/2015
 * \author Benoît Piranda
 */

#ifndef CATOMS2DEVENTS_H_
#define CATOMS2DEVENTS_H_

#include "catoms2DBlock.h"
#include "events.h"

namespace Catoms2D {

#define ROTATE_LEFT 1
#define ROTATE_RIGHT -1
//===========================================================================================================
//
//          MotionStartEvent  (class)
//
//===========================================================================================================

class MotionStartEvent : public BlockEvent {
    Vecteur pivot;
    double angle;
    int sens;
public:
	MotionStartEvent(uint64_t, Catoms2DBlock *block,const Catoms2DBlock *pivotBlock,int sens);
	MotionStartEvent(MotionStartEvent *ev);
	~MotionStartEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          MotionStepEvent  (class)
//
//===========================================================================================================

class MotionStepEvent : public BlockEvent {
    Vecteur pivot;
    double angle;
    int sens;
public:
	MotionStepEvent(uint64_t, Catoms2DBlock *block,const Vecteur &pivot,double angle2goal,int s);
	MotionStepEvent(MotionStepEvent *ev);
	~MotionStepEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          MotionStopEvent  (class)
//
//===========================================================================================================

class MotionStopEvent : public BlockEvent {
    Vecteur finalPosition;
public:
	MotionStopEvent(uint64_t, Catoms2DBlock *block);
	MotionStopEvent(MotionStepEvent *ev);
	~MotionStopEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          MotionEndEvent  (class)
//
//===========================================================================================================

class MotionEndEvent : public BlockEvent {
public:
	MotionEndEvent(uint64_t, Catoms2DBlock *block);
	MotionEndEvent(MotionEndEvent *ev);
	~MotionEndEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

}
#endif /* CATOMS2DEVENTS_H_ */
