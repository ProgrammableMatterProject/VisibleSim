/*!
 * \file rotation2DEvents.h
 * \brief Motion events for 2D Catoms
 * \date 15/02/2015
 * \author Benoît Piranda, Pierre Thalamy
 *
 * For now, only Catoms2D implement this kind of movements, if other blocks are added, we may consider creating subclasses of BuildingBlocks depending on the type of motion they implement.
 *
 */

#ifndef ROTATION2DEVENTS_H_
#define ROTATION2DEVENTS_H_

#include "events.h"
#include "catoms2DBlock.h"

using namespace Catoms2D;

//===========================================================================================================
//
//          Rotation2DMove  (class)
//
//===========================================================================================================

class Rotation2DMove {
public:
    Catoms2DBlock *pivot;
    RelativeDirection::Direction direction;

    Rotation2DMove(Catoms2DBlock *p, RelativeDirection::Direction d);
    Rotation2DMove(const Rotation2DMove &m);
    ~Rotation2DMove();

    RelativeDirection::Direction getDirection();
    Catoms2DBlock* getPivot();

};


//===========================================================================================================
//
//          Rotation2DStartEvent  (class)
//
//===========================================================================================================

class Rotation2DStartEvent : public BlockEvent {
    Vector3D pivot;
    double angle;
    int sens;
public:
    Rotation2DStartEvent(uint64_t, Catoms2DBlock *block, Rotation2DMove &m);
    Rotation2DStartEvent(Rotation2DStartEvent *ev);
    ~Rotation2DStartEvent();
    void consumeBlockEvent() {};
    void consume();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          Rotation2DStepEvent  (class)
//
//===========================================================================================================

class Rotation2DStepEvent : public BlockEvent {
    Vector3D pivot;
    double angle;
    int sens;
public:
    Rotation2DStepEvent(uint64_t, Catoms2DBlock *block,const Vector3D &pivot,double angle2goal,int s);
    Rotation2DStepEvent(Rotation2DStepEvent *ev);
    ~Rotation2DStepEvent();
    void consumeBlockEvent() {};
    void consume();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          Rotation2DStopEvent  (class)
//
//===========================================================================================================

class Rotation2DStopEvent : public BlockEvent {
    Vector3D finalPosition;
public:
    Rotation2DStopEvent(uint64_t, Catoms2DBlock *block);
    Rotation2DStopEvent(Rotation2DStepEvent *ev);
    ~Rotation2DStopEvent();
    void consumeBlockEvent() {};
    void consume();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          Rotation2DEndEvent  (class)
//
//===========================================================================================================

class Rotation2DEndEvent : public BlockEvent {
public:
    Rotation2DEndEvent(uint64_t, Catoms2DBlock *block);
    Rotation2DEndEvent(Rotation2DEndEvent *ev);
    ~Rotation2DEndEvent();
    void consumeBlockEvent() {};
    void consume();
    const virtual string getEventName();
};

#endif
