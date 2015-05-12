/*
 * reconfCatoms2DEvents.h
 *
 *  Created on: 04/05/2015
 *      Author: andre
 */

#ifndef RECONFCATOMS2DEVENTS_H_
#define RECONFCATOMS2DEVENTS_H_

#define EVENT_TRY_TO_MOVE 15000

#include "events.h"
#include <boost/shared_ptr.hpp>

class TryToMoveEvent : public BlockEvent {
 public:

 TryToMoveEvent(uint64_t t, BaseSimulator::BuildingBlock *conBlock): BlockEvent(t, conBlock) {
    eventType = EVENT_TRY_TO_MOVE;
    randomNumber = conBlock->getNextRandomNumber();
  }
	
 TryToMoveEvent(TryToMoveEvent *ev) : BlockEvent(ev) {
    randomNumber = ev->randomNumber;
  }
	
  ~TryToMoveEvent() {};
	
  void consumeBlockEvent() {
    concernedBlock->scheduleLocalEvent(EventPtr(new TryToMoveEvent(this)));
  }
	
  const string getEventName() { return "TRY_TO_MOVE EVENT"; }
};

#endif // RECONFCATOMS2DEVENTS_H_
