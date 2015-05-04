/*
 * reconfCatoms2DEvents.h
 *
 *  Created on: 04/05/2015
 *      Author: andre
 */

#ifndef RECONFCATOMS2DEVENTS_H_
#define RECONFCATOMS2DEVENTS_H_

#define EVENT_MSRSYNC	15000

#include "events.h"
#include <boost/shared_ptr.hpp>

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

#endif // RECONFCATOMS2DEVENTS_H_
