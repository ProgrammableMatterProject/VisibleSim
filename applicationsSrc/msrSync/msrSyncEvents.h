/*
 * messages.h
 *
 *  Created on: 20 avril 2013
 *      Author: nico
 */

#ifndef MSRSYNCEVENTS_H_
#define MSRSYNCEVENTS_H_

#define EVENT_MSRSYNC	15000

#include "events.h"
#include <memory>

class MsrSyncEvent : public BlockEvent {
public:

	MsrSyncEvent(uint64_t t, BaseSimulator::BuildingBlock *conBlock): BlockEvent(t, conBlock) {
		eventType = EVENT_MSRSYNC;
		randomNumber = conBlock->getNextRandomNumber();
	}
	
	MsrSyncEvent(MsrSyncEvent *ev) : BlockEvent(ev) {
		randomNumber = ev->randomNumber;
	}
	
	~MsrSyncEvent() {};
	
	void consumeBlockEvent() {
		concernedBlock->scheduleLocalEvent(EventPtr(new MsrSyncEvent(this)));
	}
	
	const string getEventName() { return "MSR SYNC EVENTS"; }
};

#endif // MSRSYNCMESSAGES_H_
