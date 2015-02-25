/*
 * blinkyBlocksScenario.h
 *
 *  Created on: 16 août 2013
 *      Author: bpiranda
 */

#ifndef BLINKYBLOCKSSCENARIO_H_
#define BLINKYBLOCKSSCENARIO_H_
#include "vecteur.h"
#include "blinkyBlocksWorld.h"
#include "scheduler.h"

class ScenarioEvent {
protected :
	float eventTime;
public :
	ScenarioEvent(float t):eventTime(t) {};

	inline float getEventTime() { return eventTime; };
	virtual void exportEventToScheduler()=0;
};

class ScenarioTappEvent:public ScenarioEvent {
	int blockId;
public:
	ScenarioTappEvent(float t,int id):ScenarioEvent(t),blockId(id) {};
	virtual void exportEventToScheduler() { BaseSimulator::getWorld()->tapBlock(eventTime*1000000+BaseSimulator::getScheduler()->now(),blockId); };
};

class ScenarioDebugEvent:public ScenarioEvent {
	bool open;
public:
	ScenarioDebugEvent(float t,bool op):ScenarioEvent(t),open(op) {};
	virtual void exportEventToScheduler() {};
};

class ScenarioSelectBlockEvent:public ScenarioEvent {
	int blockId;
public:
	ScenarioSelectBlockEvent(float t,int id):ScenarioEvent(t),blockId(id) {};
	virtual void exportEventToScheduler() {};
};

class ScenarioAddBlockEvent:public ScenarioEvent {
	Vecteur position;
public:
	ScenarioAddBlockEvent(float t,const Vecteur &pos):ScenarioEvent(t),position(pos) {};
	virtual void exportEventToScheduler() {};
};


#endif /* BLINKYBLOCKSSCENARIO_H_ */
