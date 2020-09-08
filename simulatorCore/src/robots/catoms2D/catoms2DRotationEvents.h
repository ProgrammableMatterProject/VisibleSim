/*!
 * \file catoms2DRotationEvents.h
 * \brief Motion events for 2D Catoms
 * \date 15/02/2015
 * \author Benoît Piranda, Pierre Thalamy
 *
 * For now, only Catoms2D implement this kind of movements, if other blocks are added, we may consider creating subclasses of BuildingBlocks depending on the type of motion they implement.
 *
 */

#ifndef ROTATION2DEVENTS_H_
#define ROTATION2DEVENTS_H_

#include "../../events/events.h"
#include "catoms2DBlock.h"

using namespace Catoms2D;

//===========================================================================================================
//
//          Catoms2DRotationMove  (class)
//
//===========================================================================================================

class Catoms2DRotationMove {
public:
    Catoms2DBlock *pivot;
    RelativeDirection::Direction direction;

    Catoms2DRotationMove(Catoms2DBlock *p, RelativeDirection::Direction d);
    Catoms2DRotationMove(const Catoms2DRotationMove &m);
    ~Catoms2DRotationMove();

    RelativeDirection::Direction getDirection() const;
    Catoms2DBlock* getPivot() const;

};


//===========================================================================================================
//
//          Catoms2DRotationStartEvent  (class)
//
//===========================================================================================================

class Catoms2DRotationStartEvent : public BlockEvent {
protected:
  Vector3D pivot;
  double angle;
  int sens;
  Time duration;
public:
    Catoms2DRotationStartEvent(Time, Catoms2DBlock *block, const Catoms2DRotationMove &m);
    Catoms2DRotationStartEvent(Catoms2DRotationStartEvent *ev);
    ~Catoms2DRotationStartEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          Catoms2DRotationStepEvent  (class)
//
//===========================================================================================================

class Catoms2DRotationStepEvent : public BlockEvent {
protected:
    Vector3D pivot;
    double angle;
    int sens;
    Time duration;
public:
    Catoms2DRotationStepEvent(Time, Catoms2DBlock *block,const Vector3D &pivot,double angle2goal,int s, Time d);
    Catoms2DRotationStepEvent(Catoms2DRotationStepEvent *ev);
    ~Catoms2DRotationStepEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          Catoms2DRotationStopEvent  (class)
//
//===========================================================================================================

class Catoms2DRotationStopEvent : public BlockEvent {
protected:
  Time duration;
public:
    Catoms2DRotationStopEvent(Time, Catoms2DBlock *block, Time d);
    Catoms2DRotationStopEvent(Catoms2DRotationStopEvent *ev);
    ~Catoms2DRotationStopEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          Catoms2DRotationEndEvent  (class)
//
//===========================================================================================================

class Catoms2DRotationEndEvent : public BlockEvent {
public:
    Catoms2DRotationEndEvent(Time, Catoms2DBlock *block);
    Catoms2DRotationEndEvent(Catoms2DRotationEndEvent *ev);
    ~Catoms2DRotationEndEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

#endif
