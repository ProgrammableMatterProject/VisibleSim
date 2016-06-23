/*
 * reconfCatoms2DEvents.h
 *
 *  Created on: 04/05/2015
 *      Author: andre
 */

#ifndef RECONFCATOMS2DEVENTS_H_
#define RECONFCATOMS2DEVENTS_H_

#define EVENT_TUPLE_QUERY_RESPONSE 15001
#define EVENT_TRY_TO_MOVE 15000

#include "events.h"
#include <memory>
#include "contextTuple.hpp"

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

class TupleQueryResponseEvent : public BlockEvent {
 public:
  ContextTuple *tuple;
  
 TupleQueryResponseEvent(uint64_t t, BaseSimulator::BuildingBlock *conBlock, ContextTuple *tu): BlockEvent(t, conBlock) {
    eventType =  EVENT_TUPLE_QUERY_RESPONSE;
    randomNumber = conBlock->getNextRandomNumber();
    tuple = tu;
  }
	
 TupleQueryResponseEvent(TupleQueryResponseEvent *ev) : BlockEvent(ev) {
    randomNumber = ev->randomNumber;
    tuple = ev->tuple;
  }
	
  ~TupleQueryResponseEvent() {};
	
  void consumeBlockEvent() {
    concernedBlock->scheduleLocalEvent(EventPtr(new TupleQueryResponseEvent(this)));
  }

  ContextTuple *getTuple() {return tuple;}

  const string getEventName() { return "TUPLE_QUERY_RESPONSE EVENT"; }
};

#endif // RECONFCATOMS2DEVENTS_H_
