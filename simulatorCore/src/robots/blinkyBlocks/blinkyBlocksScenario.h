/*
 * blinkyBlocksScenario.h
 *
 *  Created on: 16 août 2013
 *      Author: bpiranda
 */

#ifndef BLINKYBLOCKSSCENARIO_H_
#define BLINKYBLOCKSSCENARIO_H_

#include "math/vector3D.h"
#include "robots/blinkyBlocks/blinkyBlocksWorld.h"
#include "base/world.h"
#include "events/scheduler.h"

class ScenarioEvent {
protected :
    float eventTime;
public :
    ScenarioEvent(float t):eventTime(t) {};
    virtual ~ScenarioEvent() {};

    inline float getEventTime() { return eventTime; };
    virtual void exportEventToScheduler()=0;
};

class ScenarioTapEvent:public ScenarioEvent {
    bID blockId;
public:
    ScenarioTapEvent(float t, bID id):ScenarioEvent(t),blockId(id) {};
    ~ScenarioTapEvent() {};
    virtual void exportEventToScheduler() {
        BaseSimulator::getWorld()->tapBlock(eventTime * 1000000
                                            + BaseSimulator::getScheduler()->now(),blockId);
    };
};

class ScenarioDebugEvent:public ScenarioEvent {
    bool open;
public:
    ScenarioDebugEvent(float t,bool op):ScenarioEvent(t),open(op) {};
    ~ScenarioDebugEvent() {};
    virtual void exportEventToScheduler() {
            (void)open;         // Suppress unused member warnings
        };
};

class ScenarioSelectBlockEvent:public ScenarioEvent {
    bID blockId;
public:
    ScenarioSelectBlockEvent(float t,bID id):ScenarioEvent(t),blockId(id) {};
    ~ScenarioSelectBlockEvent() {};
        virtual void exportEventToScheduler() {
            (void)blockId; // Suppress unused member warnings
        };
};

class ScenarioAddBlockEvent:public ScenarioEvent {
    Vector3D position;
public:
    ScenarioAddBlockEvent(float t,const Vector3D &pos):ScenarioEvent(t),position(pos) {};
    ~ScenarioAddBlockEvent() {};
    virtual void exportEventToScheduler() {};
};

class ScenarioRemoveBlockEvent:public ScenarioEvent {
    Vector3D position;
public:
    ScenarioRemoveBlockEvent(float t,const Vector3D &pos):ScenarioEvent(t),position(pos) {};
    ~ScenarioRemoveBlockEvent() {};
    virtual void exportEventToScheduler() {};
};


#endif /* BLINKYBLOCKSSCENARIO_H_ */
