/**
 * @file scenario.h
 *
 *  Created on: 12/07/16
 *      Author: pthalamy
 */

#ifndef SCENARIO_H_
#define SCENARIO_H_

#include "math/Cell3DPosition.h"
#include "base/world.h"
#include "events/scheduler.h"

class ScenarioEvent : public Event {
protected :
public :
    ScenarioEvent(Time t):eventTime(t) {};
    virtual ~ScenarioEvent() {};

    inline Time getEventTime() { return eventTime; };
    virtual void exportEventToScheduler() = 0;
};

class ScenarioTapEvent:public ScenarioEvent {
    bID blockId;
public:
    ScenarioTapEvent(Time t,bID id):ScenarioEvent(t),blockId(id) {};
    ~ScenarioTapEvent() {};
    virtual void exportEventToScheduler() {
        BaseSimulator::getWorld()->tapBlock(eventTime * 1000000
                                            + BaseSimulator::getScheduler()->now(),blockId);
    };
};

class ScenarioDebugEvent:public ScenarioEvent {
    bool open;
public:
    ScenarioDebugEvent(Time t,bool op):ScenarioEvent(t),open(op) {};
    ~ScenarioDebugEvent() {};
    virtual void exportEventToScheduler() {
        (void)open;         // Suppress unused member warnings
    };
};

class ScenarioSelectBlockEvent:public ScenarioEvent {
    bID blockId;
public:
    ScenarioSelectBlockEvent(Time t,bID id):ScenarioEvent(t),blockId(id) {};
    ~ScenarioSelectBlockEvent() {};
    virtual void exportEventToScheduler() {
        (void)blockId; // Suppress unused member warnings
    };
};

class ScenarioAddBlockEvent:public ScenarioEvent {
    Vector3D position;
public:
    ScenarioAddBlockEvent(Time t,const Vector3D &pos):ScenarioEvent(t),position(pos) {};
    ~ScenarioAddBlockEvent() {};
    virtual void exportEventToScheduler() {
        BaseSimulator::getWorld()->addBlock(eventTime * 1000000
                                             + BaseSimulator::getScheduler()->now(),blockId);
    };
};

class ScenarioRemoveBlockEvent:public ScenarioEvent {
    Vector3D position;
public:
    ScenarioRemoveBlockEvent(Time t,const Vector3D &pos):ScenarioEvent(t),position(pos) {};
    ~ScenarioRemoveBlockEvent() {};
    virtual void exportEventToScheduler() {};
};


#endif /* SCENARIO_H_ */
