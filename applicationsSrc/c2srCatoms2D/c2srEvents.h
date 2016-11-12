/*
 * C2SREvents.h
 *
 *  Created on: 25 Nov 2015
 *      Author: Andre Naz
 */

#ifndef C2SREVENTS_H_
#define C2SREVENTS_H_

#define EVENT_START_C2SR 15000

#include "events.h"
#include <memory>

class StartC2SREvent : public BlockEvent {
 public:

 StartC2SREvent(Time t, BaseSimulator::BuildingBlock *conBlock): BlockEvent(t, conBlock) {
    eventType = EVENT_START_C2SR;
    randomNumber = conBlock->getRandomUint();
  }
	
 StartC2SREvent(StartC2SREvent *ev) : BlockEvent(ev) {
    randomNumber = ev->randomNumber;
  }
	
  ~StartC2SREvent() {};
	
  void consumeBlockEvent() {
    concernedBlock->scheduleLocalEvent(EventPtr(new StartC2SREvent(this)));
  }
	
  const string getEventName() { return "START_C2SR EVENT"; }
};

#endif // RECONFCATOMS2DEVENTS_H_
