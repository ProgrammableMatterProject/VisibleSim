/*
 * multiCoresEvents.h
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#ifndef MULTICORESEVENTS_H_
#define MULTICORESEVENTS_H_

#include "multiCoresBlock.h"
#include "events.h"
#include "uniqueEventsId.h"

namespace MultiCores {

//===========================================================================================================
//
//          VMStartComputationEvent  (class)
//
//===========================================================================================================

class VMStartComputationEvent : public Event {
public:
	MultiCoresBlock *concernedBlock;
	uint64_t duration;

	VMStartComputationEvent(uint64_t, MultiCoresBlock *conBlock, uint64_t dur);
	VMStartComputationEvent(VMStartComputationEvent *ev);
	~VMStartComputationEvent();
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          VMEndComputationEvent  (class)
//
//===========================================================================================================

class VMEndComputationEvent : public Event {
public:
	MultiCoresBlock *concernedBlock;
	uint64_t duration;

	VMEndComputationEvent(uint64_t, MultiCoresBlock *conBlock);
	VMEndComputationEvent(VMEndComputationEvent *ev);
	~VMEndComputationEvent();
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          VMStartTransmissionEvent  (class)
//
//===========================================================================================================

class VMStartTransmissionEvent : public Event {
public:
	MultiCoresBlock *concernedBlock;
	unsigned int messageSize;
	int destBlockId;

	VMStartTransmissionEvent(uint64_t, MultiCoresBlock *conBlock, int dbId, unsigned int mSize);
	VMStartTransmissionEvent(VMStartTransmissionEvent *ev);
	~VMStartTransmissionEvent();
	void consume();
	const virtual string getEventName();
};

} // MultiCores namespace


#endif /* MULTICORESEVENTS_H_ */
