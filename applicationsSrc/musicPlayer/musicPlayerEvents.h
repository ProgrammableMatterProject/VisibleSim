/*
 *  bbCycleEvents.h
 *
 *  Created on: 10 avril 2015
 *      Author: etienne
 */

#ifndef MSRSYNCEVENTS_H_
#define MSRSYNCEVENTS_H_

#define EVENT_SYNC   15001
#define EVENT_PLAY_NOTE 15015

#include "events.h"
#include <memory>

class SynchronizeEvent : public BlockEvent {
public:

        SynchronizeEvent(Time t, BaseSimulator::BuildingBlock *conBlock): BlockEvent(t, conBlock) {
                eventType = EVENT_SYNC;
                randomNumber = conBlock->getRandomUint();
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


class PlayNoteEvent : public BlockEvent {
public:
	
	PlayNoteEvent(Time t, BaseSimulator::BuildingBlock *conBlock): BlockEvent(t,conBlock) {
		eventType=EVENT_PLAY_NOTE;
		randomNumber = conBlock->getRandomUint();
	}

	PlayNoteEvent(PlayNoteEvent *ev) : BlockEvent(ev) {
		randomNumber = ev->randomNumber;
	}
	
	~PlayNoteEvent() {};
	
	void consumeBlockEvent() {
		concernedBlock->scheduleLocalEvent(EventPtr(new PlayNoteEvent(this)));
	}

	const string getEventName() { return "PLAY NOTE EVENT"; }
};

#endif // MSRSYNCMESSAGES_H_
