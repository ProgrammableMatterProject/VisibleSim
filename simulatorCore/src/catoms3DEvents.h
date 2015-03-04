/*!
 * \file catoms3DEvents.h
 * \brief Motion events for 2D Catoms
 * \date 15/02/2015
 * \author Benoît Piranda
 */

#ifndef CATOMS3DEVENTS_H_
#define CATOMS3DEVENTS_H_

#include "catoms3DBlock.h"
#include "events.h"

namespace Catoms3D {

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
	MotionStartEvent(uint64_t, Catoms3DBlock *block,const Catoms3DBlock *pivotBlock,int sens);
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
	MotionStepEvent(uint64_t, Catoms3DBlock *block,const Vecteur &pivot,double angle2goal,int s);
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
	MotionStopEvent(uint64_t, Catoms3DBlock *block);
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
	MotionEndEvent(uint64_t, Catoms3DBlock *block);
	MotionEndEvent(MotionEndEvent *ev);
	~MotionEndEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

}
#endif /* CATOMS3DEVENTS_H_ */
