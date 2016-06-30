/*!
 * \file catoms2DEvents.h
 * \brief Motion events for 2D Catoms
 * \date 15/02/2015
 * \author Benoît Piranda
 */

#ifndef CATOMS2DEVENTS_H_
#define CATOMS2DEVENTS_H_

#include "events.h"
#include "catoms2DBlock.h"
#include "catoms2DMove.h"

namespace Catoms2D {

class Catoms2DMove;

  //===========================================================================================================
  //
  //          MotionStartEvent  (class)
  //
  //===========================================================================================================

  class MotionStartEvent : public BlockEvent {
    Vector3D pivot;
    double angle;
    int sens;
  public:
    MotionStartEvent(uint64_t, Catoms2DBlock *block, Catoms2DMove &m);
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
    Vector3D pivot;
    double angle;
    int sens;
  public:
    MotionStepEvent(uint64_t, Catoms2DBlock *block,const Vector3D &pivot,double angle2goal,int s);
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
    Vector3D finalPosition;
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
