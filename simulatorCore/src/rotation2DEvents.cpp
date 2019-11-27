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

//#define POSITION_MOTION_DEBUG
//#define DURATION_MOTION_DEBUG
//#define COLOR_MOTION_DEBUG

#define REMAINING_STEPS angle/ANGULAR_STEP

//const int ANIMATION_DELAY=40000;
const int COM_DELAY=2000;
const int ANGLE = 60;
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

RelativeDirection::Direction Rotation2DMove::getDirection() const {
    return direction;
}

Catoms2DBlock* Rotation2DMove::getPivot() const {
    return pivot;
}

//===========================================================================================================
//
//          Rotation2DStartEvent  (class)
//
//===========================================================================================================

Rotation2DStartEvent::Rotation2DStartEvent(Time t, Catoms2DBlock *block, const Rotation2DMove &m): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION2D_START;

    pivot.set(m.getPivot()->ptrGlBlock->position[0],m.getPivot()->ptrGlBlock->position[1],m.getPivot()->ptrGlBlock->position[2]);
    angle = ANGLE;
    sens = m.getDirection();
    Distance r = defaultBlockSize[0]/2.0; // radius
    Distance d =  r*M_PI/3.0;
    duration = block->motionEngine->getDuration(d);
#ifdef DURATION_MOTION_DEBUG
    cerr << "Total motion duration (us): " << duration << endl;
    cerr << "Total motion distance (mm): " << d << endl;
#endif
}

Rotation2DStartEvent::Rotation2DStartEvent(Rotation2DStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    pivot = ev->pivot;
    angle = ev->angle;
    sens = ev->sens;
    duration = ev->duration;
}

Rotation2DStartEvent::~Rotation2DStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Rotation2DStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    Catoms2DBlock *rb = (Catoms2DBlock *)concernedBlock;
    Catoms2DWorld::getWorld()->disconnectBlock(rb);
#ifdef COLOR_MOTION_DEBUG
    rb->setColor(DARKGREY);
#endif
    unsigned int steps = REMAINING_STEPS;
    Time stepDuration = (long double)duration/(long double)steps;
    Time remaining = duration - stepDuration;
#ifdef DURATION_MOTION_DEBUG
    cerr << "----------" << endl;
    cerr << "@" << concernedBlock->blockId << endl;
    cerr << "Duration: " << duration << endl;
    cerr << "Step duration: " << stepDuration << endl;
    cerr << "Motion start at " << getScheduler()->now() << endl;
    cerr << "Motion should end at " << getScheduler()->now()+duration << endl;
    cerr << "----------" << endl;
#endif
    scheduler->schedule(new Rotation2DStepEvent(scheduler->now() + stepDuration, rb,pivot,angle,sens,remaining));
}

const string Rotation2DStartEvent::getEventName() {
    return("Rotation2DStart Event");
}

//===========================================================================================================
//
//          Rotation2DStepEvent  (class)
//
//===========================================================================================================

Rotation2DStepEvent::Rotation2DStepEvent(Time t, Catoms2DBlock *block,const Vector3D &p,double angle2goal,int s,Time d): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION2D_STEP;
    pivot = p;
    angle = angle2goal;
    sens = s;
    duration = d;
}

Rotation2DStepEvent::Rotation2DStepEvent(Rotation2DStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    pivot = ev->pivot;
    angle = ev->angle;
    sens = ev->sens;
    duration = ev->duration;
}

Rotation2DStepEvent::~Rotation2DStepEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Rotation2DStepEvent::consume() {
    EVENT_CONSUME_INFO();
    Catoms2DBlock *rb = (Catoms2DBlock*)concernedBlock;

    Scheduler *scheduler = getScheduler();
#ifdef DURATION_MOTION_DEBUG
    cerr << "@" << rb->blockId << " motion step, angle " << angle << " at " << getScheduler()->now() << " (" << date << ")" << endl;
#endif
    Matrix roty;

    unsigned int steps = REMAINING_STEPS;
    Time stepDuration = (long double)duration/(long double)steps;
    Time remaining = duration - stepDuration;

    if (angle < ANGULAR_STEP) {
        scheduler->schedule(new Rotation2DStopEvent(scheduler->now() + duration,rb,duration));
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
        scheduler->schedule(new Rotation2DStepEvent(scheduler->now() + stepDuration,rb,
                            pivot,angle-ANGULAR_STEP,sens,remaining));
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

Rotation2DStopEvent::Rotation2DStopEvent(Time t, Catoms2DBlock *block, Time d): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION2D_STOP;
    duration = d;
}

Rotation2DStopEvent::Rotation2DStopEvent(Rotation2DStopEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    duration = ev->duration;
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

#ifdef POSITION_MOTION_DEBUG
    cerr << "---------------motion end-----------------"<<endl;
    cerr << worldPos << endl;
    cerr << gridPos << endl;
    cerr << "------------------------------------------"<<endl;
#endif

    rb->setPosition(gridPos);

    rb->angle = rb->angle%360;
#ifdef COLOR_MOTION_DEBUG
    rb->setColor(YELLOW);
#endif

#ifdef DURATION_MOTION_DEBUG
    cerr << "----------" << endl;
    cerr << "@" << concernedBlock->blockId << endl;
    cerr << "Now: " << getScheduler()->now() << endl;
    cerr << "Motion end at " << date << endl;
    cerr << "Communication should be re-established at " << date + COM_DELAY << endl;
    cerr << "----------" << endl;
#endif

    // stringstream info;
    // info.str("");
    // info << "connect Block " << rb->blockId;
    // getScheduler()->trace(info.str(),rb->blockId,LIGHTBLUE);
    wrld->connectBlock(rb);

    StatsCollector::getInstance().incMotionCount();
    StatsIndividual::incMotionCount(rb->stats);

    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new Rotation2DEndEvent(scheduler->now(), rb));
    //concernedBlock->blockCode->processLocalEvent(EventPtr(new Rotation2DEndEvent(scheduler->now()+COM_DELAY,rb)));
}

const string Rotation2DStopEvent::getEventName() {
    return("Rotation2DStop Event");
}

//===========================================================================================================
//
//          Rotation2DEndEvent  (class)
//
//===========================================================================================================

Rotation2DEndEvent::Rotation2DEndEvent(Time t, Catoms2DBlock *block): BlockEvent(t,block) {
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
    concernedBlock->scheduleLocalEvent(EventPtr(new Rotation2DEndEvent(getScheduler()->now()+COM_DELAY,(Catoms2DBlock*)concernedBlock)));
}

const string Rotation2DEndEvent::getEventName() {
    return("Rotation2DEnd Event");
}
