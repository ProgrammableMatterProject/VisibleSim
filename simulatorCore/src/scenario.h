/**
 * @file scenario.h
 *
 *  Created on: 12/07/16
 *      Author: pthalamy
 */

#ifndef SCENARIO_H_
#define SCENARIO_H_

#include "Cell3DPosition.h"
#include "world.h"
#include "scheduler.h"

class ScenarioEvent : public Event {
protected :
public :
	ScenarioEvent(uint64_t t):eventTime(t) {};
	virtual ~ScenarioEvent() {};
	
	inline uint64_t getEventTime() { return eventTime; };
	virtual void exportEventToScheduler() = 0;
};

class ScenarioTapEvent:public ScenarioEvent {
	int blockId;
public:
	ScenarioTapEvent(uint64_t t,int id):ScenarioEvent(t),blockId(id) {};
	~ScenarioTapEvent() {};
    virtual void exportEventToScheduler() {
        BaseSimulator::getWorld()->tapBlock(eventTime * 1000000
                                            + BaseSimulator::getScheduler()->now(),blockId);
    };
};

class ScenarioDebugEvent:public ScenarioEvent {
	bool open;
public:
	ScenarioDebugEvent(uint64_t t,bool op):ScenarioEvent(t),open(op) {};
	~ScenarioDebugEvent() {};
	virtual void exportEventToScheduler() {
		(void)open;         // Suppress unused member warnings
	};
};

class ScenarioSelectBlockEvent:public ScenarioEvent {
	int blockId;
public:
	ScenarioSelectBlockEvent(uint64_t t,int id):ScenarioEvent(t),blockId(id) {};
	~ScenarioSelectBlockEvent() {};
	virtual void exportEventToScheduler() {
		(void)blockId; // Suppress unused member warnings
	};
};

class ScenarioAddBlockEvent:public ScenarioEvent {
	Vector3D position;
public:
	ScenarioAddBlockEvent(uint64_t t,const Vector3D &pos):ScenarioEvent(t),position(pos) {};
	~ScenarioAddBlockEvent() {};
	virtual void exportEventToScheduler() {
        BaseSimulator::getWorld()->addBlock(eventTime * 1000000
											 + BaseSimulator::getScheduler()->now(),blockId);		
	};
};

class ScenarioRemoveBlockEvent:public ScenarioEvent {
	Vector3D position;
public:
	ScenarioRemoveBlockEvent(uint64_t t,const Vector3D &pos):ScenarioEvent(t),position(pos) {};
	~ScenarioRemoveBlockEvent() {};
	virtual void exportEventToScheduler() {};
};


#endif /* SCENARIO_H_ */
