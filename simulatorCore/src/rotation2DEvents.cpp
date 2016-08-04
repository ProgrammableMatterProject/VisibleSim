/*
 * @file rotation2DEvents.cpp
 *
 * Formerly catoms2DEvents.cpp
 *  Created on: 2014 fevrary 1st, 18/07/16
 *      Author: Benoît, Pierre
 */

#include "rotation2DEvents.h"
#include "catoms2DWorld.h"
#include "utils.h"

using namespace BaseSimulator::utils;

const int ANIMATION_DELAY=40000;
const int COM_DELAY=2000;
const int ANGULAR_STEP=12;

//===========================================================================================================
//
//          Rotation2DMove  (class)
//
//===========================================================================================================

Rotation2DMove::Rotation2DMove(Catoms2DBlock *p, RelativeDirection::Direction d) {
    pivot = p;
    direction = d;
}

Rotation2DMove::Rotation2DMove(const Rotation2DMove &m) {
    pivot = m.pivot;
    direction = m.direction;
}

Rotation2DMove::~Rotation2DMove() { }

RelativeDirection::Direction Rotation2DMove::getDirection() {
    return direction;
}

Catoms2DBlock* Rotation2DMove::getPivot() {
    return pivot;
}

//===========================================================================================================
//
//          Rotation2DStartEvent  (class)
//
//===========================================================================================================

Rotation2DStartEvent::Rotation2DStartEvent(uint64_t t, Catoms2DBlock *block, Rotation2DMove &m): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION2D_START;
    
    pivot.set(m.getPivot()->ptrGlBlock->position[0],m.getPivot()->ptrGlBlock->position[1],m.getPivot()->ptrGlBlock->position[2]);
    angle = 60;
    sens = m.getDirection();
}

Rotation2DStartEvent::Rotation2DStartEvent(Rotation2DStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Rotation2DStartEvent::~Rotation2DStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Rotation2DStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    Catoms2DBlock *rb = (Catoms2DBlock *)concernedBlock;
    Catoms2DWorld::getWorld()->disconnectBlock(rb);
    rb->setColor(DARKGREY);
    scheduler->schedule(new Rotation2DStepEvent(scheduler->now() + ANIMATION_DELAY, rb,pivot,angle,sens));
}

const string Rotation2DStartEvent::getEventName() {
    return("Rotation2DStart Event");
}

//===========================================================================================================
//
//          Rotation2DStepEvent  (class)
//
//===========================================================================================================

Rotation2DStepEvent::Rotation2DStepEvent(uint64_t t, Catoms2DBlock *block,const Vector3D &p,double angle2goal,int s): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION2D_STEP;

    pivot = p;
    angle = angle2goal;
    sens = s;
}

Rotation2DStepEvent::Rotation2DStepEvent(Rotation2DStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Rotation2DStepEvent::~Rotation2DStepEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Rotation2DStepEvent::consume() {
    EVENT_CONSUME_INFO();
    Catoms2DBlock *rb = (Catoms2DBlock*)concernedBlock;

    Scheduler *scheduler = getScheduler();

    Matrix roty;
    if (angle<ANGULAR_STEP) {
        roty.setRotationY(-sens*angle);
        Vector3D BA(rb->ptrGlBlock->position[0] - pivot[0],
                    rb->ptrGlBlock->position[1] - pivot[1],
                    rb->ptrGlBlock->position[2] - pivot[2]);
        Vector3D BC = roty*BA;
        Vector3D pos = pivot+BC;
        rb->angle += angle*sens;
        Catoms2DWorld::getWorld()->updateGlData(rb,pos,((Catoms2DGlBlock*)rb->ptrGlBlock)->angle+angle*sens);
        scheduler->schedule(new Rotation2DStopEvent(scheduler->now() + ANIMATION_DELAY, rb));
    } else {
        roty.setRotationY(-sens*ANGULAR_STEP);
        Vector3D BA(rb->ptrGlBlock->position[0] - pivot[0],
                    rb->ptrGlBlock->position[1] - pivot[1],
                    rb->ptrGlBlock->position[2] - pivot[2]);
        Vector3D BC = roty*BA;
        Vector3D pos = pivot+BC;
        rb->angle += ANGULAR_STEP*sens;
        Catoms2DWorld::getWorld()->updateGlData(rb,pos,
                                                ((Catoms2DGlBlock*)rb->ptrGlBlock)->angle+ANGULAR_STEP*sens);
        scheduler->schedule(new Rotation2DStepEvent(scheduler->now() + ANIMATION_DELAY,rb,
                                                pivot,angle-ANGULAR_STEP,sens));
    }
}

const string Rotation2DStepEvent::getEventName() {
    return("Rotation2DStep Event");
}

//===========================================================================================================
//
//          Rotation2DStepEvent  (class)
//
//===========================================================================================================

Rotation2DStopEvent::Rotation2DStopEvent(uint64_t t, Catoms2DBlock *block): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION2D_STOP;
}

Rotation2DStopEvent::Rotation2DStopEvent(Rotation2DStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Rotation2DStopEvent::~Rotation2DStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Rotation2DStopEvent::consume() {
    EVENT_CONSUME_INFO();
    Catoms2DBlock *rb = (Catoms2DBlock*)concernedBlock;

    /* Transformer les coordonnées GL en coordonnées grille*/

    Catoms2DWorld *wrld=Catoms2DWorld::getWorld();
    Vector3D worldPos = Vector3D(rb->ptrGlBlock->position[0],
                                 rb->ptrGlBlock->position[1],
                                 rb->ptrGlBlock->position[2]);
    Cell3DPosition gridPos = wrld->lattice->worldToGridPosition(worldPos);
    /*
    cout << "---------------motion end-----------------"<<endl;
    cout << worldPos << endl;
    cout << gridPos << endl;
    cout << "------------------------------------------"<<endl;
    */
    rb->setPosition(gridPos);    
    rb->setColor(YELLOW);
    
    stringstream info;
    info.str("");
    info << "connect Block " << rb->blockId;
    getScheduler()->trace(info.str(),rb->blockId,LIGHTBLUE);
    wrld->connectBlock(rb);
    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new Rotation2DEndEvent(scheduler->now() + ANIMATION_DELAY, rb));
}

const string Rotation2DStopEvent::getEventName() {
    return("Rotation2DStop Event");
}

//===========================================================================================================
//
//          Rotation2DEndEvent  (class)
//
//===========================================================================================================

Rotation2DEndEvent::Rotation2DEndEvent(uint64_t t, Catoms2DBlock *block): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION2D_END;
}

Rotation2DEndEvent::Rotation2DEndEvent(Rotation2DEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Rotation2DEndEvent::~Rotation2DEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Rotation2DEndEvent::consume() {
    EVENT_CONSUME_INFO();
    Catoms2DBlock *rb = (Catoms2DBlock*)concernedBlock;
    concernedBlock->blockCode->processLocalEvent(EventPtr(new Rotation2DEndEvent(date+COM_DELAY,rb)));
    StatsCollector::getInstance().incMotionCount();
}

const string Rotation2DEndEvent::getEventName() {
    return("Rotation2DEnd Event");
}
