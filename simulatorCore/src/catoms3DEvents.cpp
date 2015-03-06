/*
 * catoms3DEvents.cpp
 *
 *  Created on: 2014 fevrary 1st
 *      Author: Benoît
 */

#include "catoms3DEvents.h"
#include "catoms3DScheduler.h"
#include "catoms3DWorld.h"

const int ANIMATION_DELAY=40000;
const int COM_DELAY=2000;
const int ANGULAR_STEP=12;

const double EPS=1E-5;
namespace Catoms3D {

//===========================================================================================================
//
//          MotionStartEvent  (class)
//
//===========================================================================================================

MotionStartEvent::MotionStartEvent(uint64_t t, Catoms3DBlock *block,const Catoms3DBlock *pivotBlock, int s): BlockEvent(t,block) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_MOTION_START;
	pivot.set(pivotBlock->ptrGlBlock->position[0],pivotBlock->ptrGlBlock->position[1],pivotBlock->ptrGlBlock->position[2]);
	angle = 60;
    sens = s;
}

MotionStartEvent::MotionStartEvent(MotionStartEvent *ev) : BlockEvent(ev) {
	EVENT_CONSTRUCTOR_INFO();
}

MotionStartEvent::~MotionStartEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void MotionStartEvent::consume() {
	EVENT_CONSUME_INFO();
	Catoms3DScheduler *scheduler = Catoms3D::getScheduler();
	Catoms3DBlock *rb = (Catoms3DBlock *)concernedBlock;
    Catoms3DWorld::getWorld()->disconnectBlock(rb);
    rb->setColor(DARKGREY);
	scheduler->schedule(new MotionStepEvent(scheduler->now() + ANIMATION_DELAY, rb,pivot,angle,sens));
}

const string MotionStartEvent::getEventName() {
	return("MotionStart Event");
}

//===========================================================================================================
//
//          MotionStepEvent  (class)
//
//===========================================================================================================

MotionStepEvent::MotionStepEvent(uint64_t t, Catoms3DBlock *block,const Vecteur &p,double angle2goal,int s): BlockEvent(t,block) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_MOTION_STEP;

    pivot = p;
    angle = angle2goal;
    sens = s;
}

MotionStepEvent::MotionStepEvent(MotionStepEvent *ev) : BlockEvent(ev) {
	EVENT_CONSTRUCTOR_INFO();
}

MotionStepEvent::~MotionStepEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void MotionStepEvent::consume() {
	EVENT_CONSUME_INFO();
	Catoms3DBlock *rb = (Catoms3DBlock*)concernedBlock;

	Catoms3DScheduler *scheduler = Catoms3D::getScheduler();

    Matrice roty;
    if (angle<ANGULAR_STEP) {
        roty.setRotationY(-sens*angle);
        Vecteur BA(rb->ptrGlBlock->position[0] - pivot[0],rb->ptrGlBlock->position[1] - pivot[1],rb->ptrGlBlock->position[2] - pivot[2]);
        Vecteur BC = roty*BA;
        Vecteur pos = pivot+BC;
        Catoms3DWorld::getWorld()->updateGlData(rb,pos,rb->ptrGlBlock->angle-angle*sens);
        scheduler->schedule(new MotionStopEvent(scheduler->now() + ANIMATION_DELAY, rb));
	} else {
        roty.setRotationY(-sens*ANGULAR_STEP);
        Vecteur BA(rb->ptrGlBlock->position[0] - pivot[0],rb->ptrGlBlock->position[1] - pivot[1],rb->ptrGlBlock->position[2] - pivot[2]);
        Vecteur BC = roty*BA;
        Vecteur pos = pivot+BC;
        Catoms3DWorld::getWorld()->updateGlData(rb,pos,rb->ptrGlBlock->angle-ANGULAR_STEP*sens);
        scheduler->schedule(new MotionStepEvent(scheduler->now() + ANIMATION_DELAY,rb, pivot,angle-ANGULAR_STEP,sens));
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

MotionStopEvent::MotionStopEvent(uint64_t t, Catoms3DBlock *block): BlockEvent(t,block) {
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
	Catoms3DBlock *rb = (Catoms3DBlock*)concernedBlock;
    rb->setColor(YELLOW);

/* Transformer les coordonnées GL en coordonnées grille*/

    Catoms3DWorld *wrld=Catoms3DWorld::getWorld();
    Vecteur worldPos = Vecteur(rb->ptrGlBlock->position[0],rb->ptrGlBlock->position[1],rb->ptrGlBlock->position[2]);
    Cell3DPosition gridPos = wrld->worldToGridPosition(worldPos);
    rb->setPosition(gridPos);
	wrld->setGridPtr(gridPos.pt[0],gridPos.pt[1],gridPos.pt[2],rb);

	stringstream info;
    info.str("");
    info << "connect Block " << rb->blockId;
    getScheduler()->trace(info.str(),rb->blockId,LIGHTBLUE);
	wrld->connectBlock(rb);
    Catoms3DScheduler *scheduler = Catoms3D::getScheduler();
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

MotionEndEvent::MotionEndEvent(uint64_t t, Catoms3DBlock *block): BlockEvent(t,block) {
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
	Catoms3DBlock *rb = (Catoms3DBlock*)concernedBlock;
	concernedBlock->blockCode->processLocalEvent(EventPtr(new MotionEndEvent(date+COM_DELAY,rb)));
}

const string MotionEndEvent::getEventName() {
	return("MotionEnd Event");
}


} // Catoms3D namespace
