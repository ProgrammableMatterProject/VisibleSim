/*
 *  bbCycleEvents.h
 *
 *  Created on: 10 avril 2015
 *      Author: etienne
 */

#ifndef MSRSYNCEVENTS_H_
#define MSRSYNCEVENTS_H_

#define EVENT_SYNC   15001
#define EVENT_SPREAD_SONG 15015

#include "events.h"
#include <boost/shared_ptr.hpp>

class SynchronizeEvent : public BlockEvent {
public:

        SynchronizeEvent(uint64_t t, BaseSimulator::BuildingBlock *conBlock): BlockEvent(t, conBlock) {
                eventType = EVENT_SYNC;
                randomNumber = conBlock->getNextRandomNumber();
        }

        SynchronizeEvent(SynchronizeEvent *ev) : BlockEvent(ev) {
                randomNumber = ev->randomNumber;
        }

        ~SynchronizeEvent() {};

        void consumeBlockEvent() {
                concernedBlock->scheduleLocalEvent(EventPtr(new SynchronizeEvent(this)));
        }

        const string getEventName() { return "SYNC EVENT"; }
};

class SpreadSongEvent : public BlockEvent {
public:
	
	SpreadSongEvent(uint64_t t, BaseSimulator::BuildingBlock *conBlock): BlockEvent(t,conBlock) {
		eventType=EVENT_SPREAD_SONG;
		randomNumber = conBlock->getNextRandomNumber();
	}

	SpreadSongEvent(SpreadSongEvent *ev) : BlockEvent(ev) {
		randomNumber = ev->randomNumber;
	}
	
	~SpreadSongEvent() {};
	
	void consumeBlockEvent() {
		concernedBlock->scheduleLocalEvent(EventPtr(new SpreadSongEvent(this)));
	}

	const string getEventName() { return "SPREAD SONG EVENT"; }
};

#endif // MSRSYNCMESSAGES_H_
