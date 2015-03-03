/*
 * catoms2DEvents.cpp
 *
 *  Created on: 2014 fevrary 1st
 *      Author: Benoît
 */

#include "catoms2DEvents.h"
#include "catoms2DScheduler.h"
#include "catoms2DWorld.h"

const int ANIMATION_DELAY=40000;
const int COM_DELAY=2000;

const double EPS=1E-5;
namespace Catoms2D {

//===========================================================================================================
//
//          MotionStartEvent  (class)
//
//===========================================================================================================

MotionStartEvent::MotionStartEvent(uint64_t t, Catoms2DBlock *block,const Catoms2DBlock *pivotBlock, int sens): BlockEvent(t,block) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_MOTION_START;
	pivot.set(pivotBlock->ptrGlBlock->position[0],pivotBlock->ptrGlBlock->position[1],pivotBlock->ptrGlBlock->position[2]);
	angle = sens*60;
}

MotionStartEvent::MotionStartEvent(MotionStartEvent *ev) : BlockEvent(ev) {
	EVENT_CONSTRUCTOR_INFO();
}

MotionStartEvent::~MotionStartEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void MotionStartEvent::consume() {
	EVENT_CONSUME_INFO();
	Catoms2DScheduler *scheduler = Catoms2D::getScheduler();
	Catoms2DBlock *rb = (Catoms2DBlock *)concernedBlock;
    Catoms2DWorld::getWorld()->disconnectBlock(rb);
    rb->setColor(DARKGREY);
	scheduler->schedule(new MotionStepEvent(scheduler->now() + ANIMATION_DELAY, rb,pivot,angle));
}

const string MotionStartEvent::getEventName() {
	return("MotionStart Event");
}

//===========================================================================================================
//
//          MotionStepEvent  (class)
//
//===========================================================================================================

MotionStepEvent::MotionStepEvent(uint64_t t, Catoms2DBlock *block,const Vecteur &p,double angle2goal): BlockEvent(t,block) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_MOTION_STEP;

    pivot = p;
    angle = angle2goal;
}

MotionStepEvent::MotionStepEvent(MotionStepEvent *ev) : BlockEvent(ev) {
	EVENT_CONSTRUCTOR_INFO();
}

MotionStepEvent::~MotionStepEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void MotionStepEvent::consume() {
	EVENT_CONSUME_INFO();
	Catoms2DBlock *rb = (Catoms2DBlock*)concernedBlock;

	Catoms2DScheduler *scheduler = Catoms2D::getScheduler();


    Matrice roty;
    if (angle<12) {
        roty.setRotationY(angle);
        Vecteur BA(rb->ptrGlBlock->position[0] - pivot[0],rb->ptrGlBlock->position[1] - pivot[1],rb->ptrGlBlock->position[2] - pivot[2]);
        Vecteur BC = roty*BA;
        Vecteur pos = pivot+BC;
        Catoms2DWorld::getWorld()->updateGlData(rb,pos);
        scheduler->schedule(new MotionStopEvent(scheduler->now() + ANIMATION_DELAY, rb));
	} else {
        roty.setRotationY(12);
        Vecteur BA(rb->ptrGlBlock->position[0] - pivot[0],rb->ptrGlBlock->position[1] - pivot[1],rb->ptrGlBlock->position[2] - pivot[2]);
        Vecteur BC = roty*BA;
        Vecteur pos = pivot+BC;
        Catoms2DWorld::getWorld()->updateGlData(rb,pos);
        scheduler->schedule(new MotionStepEvent(scheduler->now() + ANIMATION_DELAY,rb, pivot,angle-12));
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

MotionStopEvent::MotionStopEvent(uint64_t t, Catoms2DBlock *block): BlockEvent(t,block) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_MOTION_STOP;
}

MotionStopEvent::MotionStopEvent(MotionStepEvent *ev) : BlockEvent(ev) {
	EVENT_CONSTRUCTOR_INFO();
}

MotionStopEvent::~MotionStopEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void MotionStopEvent::consume() {
	EVENT_CONSUME_INFO();
	Catoms2DBlock *rb = (Catoms2DBlock*)concernedBlock;
    rb->setColor(YELLOW);

/* Transformer les coordonnées GL en coordonnées grille*/

    Catoms2DWorld *wrld=Catoms2DWorld::getWorld();
    Vecteur worldPos = Vecteur(rb->ptrGlBlock->position[0],rb->ptrGlBlock->position[1],rb->ptrGlBlock->position[2]);
    Vecteur gridPos = wrld->worldToGridPosition(worldPos);
    cout << "---------------motion end-----------------"<<endl;
    cout << worldPos << endl;
    cout << gridPos << endl;
    cout << "------------------------------------------"<<endl;
    rb->setPosition(gridPos);
	wrld->setGridPtr(gridPos.pt[0],gridPos.pt[1],gridPos.pt[2],rb);

	stringstream info;
    info.str("");
    info << "connect Block " << rb->blockId;
    getScheduler()->trace(info.str(),rb->blockId,LIGHTBLUE);
	wrld->connectBlock(rb);
    Catoms2DScheduler *scheduler = Catoms2D::getScheduler();
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

MotionEndEvent::MotionEndEvent(uint64_t t, Catoms2DBlock *block): BlockEvent(t,block) {
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
	Catoms2DBlock *rb = (Catoms2DBlock*)concernedBlock;
	concernedBlock->blockCode->processLocalEvent(EventPtr(new MotionEndEvent(date+COM_DELAY,rb)));
}

const string MotionEndEvent::getEventName() {
	return("MotionEnd Event");
}


} // Catoms2D namespace
