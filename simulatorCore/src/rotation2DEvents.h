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

    RelativeDirection::Direction getDirection() const;
    Catoms2DBlock* getPivot() const;

};


//===========================================================================================================
//
//          Rotation2DStartEvent  (class)
//
//===========================================================================================================

class Rotation2DStartEvent : public BlockEvent {
protected:
  Vector3D pivot;
  double angle;
  int sens;
  Time duration;
public:
    Rotation2DStartEvent(Time, Catoms2DBlock *block, const Rotation2DMove &m);
    Rotation2DStartEvent(Rotation2DStartEvent *ev);
    ~Rotation2DStartEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          Rotation2DStepEvent  (class)
//
//===========================================================================================================

class Rotation2DStepEvent : public BlockEvent {
protected:
    Vector3D pivot;
    double angle;
    int sens;
    Time duration;
public:
    Rotation2DStepEvent(Time, Catoms2DBlock *block,const Vector3D &pivot,double angle2goal,int s, Time d);
    Rotation2DStepEvent(Rotation2DStepEvent *ev);
    ~Rotation2DStepEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          Rotation2DStopEvent  (class)
//
//===========================================================================================================

class Rotation2DStopEvent : public BlockEvent {
protected:
  Time duration;
public:
    Rotation2DStopEvent(Time, Catoms2DBlock *block, Time d);
    Rotation2DStopEvent(Rotation2DStopEvent *ev);
    ~Rotation2DStopEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          Rotation2DEndEvent  (class)
//
//===========================================================================================================

class Rotation2DEndEvent : public BlockEvent {
public:
    Rotation2DEndEvent(Time, Catoms2DBlock *block);
    Rotation2DEndEvent(Rotation2DEndEvent *ev);
    ~Rotation2DEndEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

#endif
