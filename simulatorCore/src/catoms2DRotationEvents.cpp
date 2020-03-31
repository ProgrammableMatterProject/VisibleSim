/*
 * @file catoms2DRotationEvents.cpp
 *
 * Formerly catoms2DEvents.cpp
 *  Created on: 2014 fevrary 1st, 18/07/16
 *      Author: Benoît, Pierre
 */

#include "catoms2DRotationEvents.h"
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
//          Catoms2DRotationMove  (class)
//
//===========================================================================================================

Catoms2DRotationMove::Catoms2DRotationMove(Catoms2DBlock *p, RelativeDirection::Direction d) {
    pivot = p;
    direction = d;
}

Catoms2DRotationMove::Catoms2DRotationMove(const Catoms2DRotationMove &m) {
    pivot = m.pivot;
    direction = m.direction;
}

Catoms2DRotationMove::~Catoms2DRotationMove() { }

RelativeDirection::Direction Catoms2DRotationMove::getDirection() const {
    return direction;
}

Catoms2DBlock* Catoms2DRotationMove::getPivot() const {
    return pivot;
}

//===========================================================================================================
//
//          Catoms2DRotationStartEvent  (class)
//
//===========================================================================================================

Catoms2DRotationStartEvent::Catoms2DRotationStartEvent(Time t, Catoms2DBlock *block, const Catoms2DRotationMove &m): BlockEvent(t,block) {
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

Catoms2DRotationStartEvent::Catoms2DRotationStartEvent(Catoms2DRotationStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    pivot = ev->pivot;
    angle = ev->angle;
    sens = ev->sens;
    duration = ev->duration;
}

Catoms2DRotationStartEvent::~Catoms2DRotationStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Catoms2DRotationStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    Catoms2DBlock *rb = (Catoms2DBlock *)concernedBlock;
    Catoms2DWorld::getWorld()->disconnectBlock(rb, false);
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
    scheduler->schedule(new Catoms2DRotationStepEvent(scheduler->now() + stepDuration, rb,pivot,angle,sens,remaining));
}

const string Catoms2DRotationStartEvent::getEventName() {
    return("Catoms2DRotationStart Event");
}

//===========================================================================================================
//
//          Catoms2DRotationStepEvent  (class)
//
//===========================================================================================================

Catoms2DRotationStepEvent::Catoms2DRotationStepEvent(Time t, Catoms2DBlock *block,const Vector3D &p,double angle2goal,int s,Time d): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION2D_STEP;
    pivot = p;
    angle = angle2goal;
    sens = s;
    duration = d;
}

Catoms2DRotationStepEvent::Catoms2DRotationStepEvent(Catoms2DRotationStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    pivot = ev->pivot;
    angle = ev->angle;
    sens = ev->sens;
    duration = ev->duration;
}

Catoms2DRotationStepEvent::~Catoms2DRotationStepEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Catoms2DRotationStepEvent::consume() {
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
        scheduler->schedule(new Catoms2DRotationStopEvent(scheduler->now() + duration,rb,duration));
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
        scheduler->schedule(new Catoms2DRotationStepEvent(scheduler->now() + stepDuration,rb,
                            pivot,angle-ANGULAR_STEP,sens,remaining));
    }
}

const string Catoms2DRotationStepEvent::getEventName() {
    return("Catoms2DRotationStep Event");
}

//===========================================================================================================
//
//          Catoms2DRotationStepEvent  (class)
//
//===========================================================================================================

Catoms2DRotationStopEvent::Catoms2DRotationStopEvent(Time t, Catoms2DBlock *block, Time d): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION2D_STOP;
    duration = d;
}

Catoms2DRotationStopEvent::Catoms2DRotationStopEvent(Catoms2DRotationStopEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    duration = ev->duration;
}

Catoms2DRotationStopEvent::~Catoms2DRotationStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Catoms2DRotationStopEvent::consume() {
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
    wrld->connectBlock(rb, false);

    StatsCollector::getInstance().incMotionCount();
    StatsIndividual::incMotionCount(rb->stats);

    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new Catoms2DRotationEndEvent(scheduler->now(), rb));
}

const string Catoms2DRotationStopEvent::getEventName() {
    return("Catoms2DRotationStop Event");
}

//===========================================================================================================
//
//          Catoms2DRotationEndEvent  (class)
//
//===========================================================================================================

Catoms2DRotationEndEvent::Catoms2DRotationEndEvent(Time t, Catoms2DBlock *block): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION2D_END;
}

Catoms2DRotationEndEvent::Catoms2DRotationEndEvent(Catoms2DRotationEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Catoms2DRotationEndEvent::~Catoms2DRotationEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Catoms2DRotationEndEvent::consume() {
    EVENT_CONSUME_INFO();

    Catoms2DBlock *rb = (Catoms2DBlock*)concernedBlock;
    concernedBlock->blockCode->processLocalEvent(EventPtr(new Catoms2DRotationEndEvent(date+COM_DELAY,rb)));
    StatsCollector::getInstance().incMotionCount();
    StatsIndividual::incMotionCount(rb->stats);
}

const string Catoms2DRotationEndEvent::getEventName() {
    return("Catoms2DRotationEnd Event");
}
