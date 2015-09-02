/*
 * multiCoresEvent.cpp
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#include "multiCoresEvents.h"

namespace MultiCores {

//===========================================================================================================
//
//          VMStartComputationEvent  (class)
//
//===========================================================================================================

VMStartComputationEvent::VMStartComputationEvent(uint64_t t, MultiCoresBlock *conBlock, uint64_t dur):Event(t) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_VM_START_COMPUTATION;
	concernedBlock = conBlock;
	duration = dur;
}

VMStartComputationEvent::VMStartComputationEvent(VMStartComputationEvent *ev) : Event(ev) {
	EVENT_CONSTRUCTOR_INFO();
	concernedBlock = ev->concernedBlock;
	eventType = ev->eventType;
	duration = ev->duration;
}

VMStartComputationEvent::~VMStartComputationEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void VMStartComputationEvent::consume() {
	EVENT_CONSUME_INFO();
	concernedBlock->scheduleLocalEvent(EventPtr(new VMStartComputationEvent(this)));
}

const string VMStartComputationEvent::getEventName() {
	return("VMStartComputation Event");
}

//===========================================================================================================
//
//          VMEndComputationEvent  (class)
//
//===========================================================================================================

VMEndComputationEvent::VMEndComputationEvent(uint64_t t, MultiCoresBlock *block) : Event(t) {
	EVENT_CONSTRUCTOR_INFO();
	concernedBlock = block;
	eventType = EVENT_VM_END_COMPUTATION;
}

VMEndComputationEvent::VMEndComputationEvent(VMEndComputationEvent *ev) : Event(ev) {
	EVENT_CONSTRUCTOR_INFO();
	concernedBlock = ev->concernedBlock;
	eventType = ev->eventType;
}

VMEndComputationEvent::~VMEndComputationEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void VMEndComputationEvent::consume() {
	EVENT_CONSUME_INFO();
	concernedBlock->scheduleLocalEvent(EventPtr(new VMEndComputationEvent(this)));
	return;
}

const string VMEndComputationEvent::getEventName() {
	return("VMEndComputation Event");
}

//===========================================================================================================
//
//          VMStartTransmissionEvent  (class)
//
//===========================================================================================================

VMStartTransmissionEvent::VMStartTransmissionEvent(uint64_t t, MultiCoresBlock *conBlock, int dbId, unsigned int mSize):Event(t) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_VM_START_TRANSMISSION;
	concernedBlock = conBlock;
	messageSize = mSize;
	destBlockId = dbId;
}

VMStartTransmissionEvent::VMStartTransmissionEvent(VMStartTransmissionEvent *ev) : Event(ev) {
	EVENT_CONSTRUCTOR_INFO();
	concernedBlock = ev->concernedBlock;
	eventType = ev->eventType;
	messageSize = ev->messageSize;
	destBlockId = ev->destBlockId;
}

VMStartTransmissionEvent::~VMStartTransmissionEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void VMStartTransmissionEvent::consume() {
	EVENT_CONSUME_INFO();
	concernedBlock->scheduleLocalEvent(EventPtr(new VMStartTransmissionEvent(this)));
}

const string VMStartTransmissionEvent::getEventName() {
	return("VMStartTransmission Event");
}

} // MultiCores namespace
