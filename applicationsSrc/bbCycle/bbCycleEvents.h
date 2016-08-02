/*
 *  bbCycleEvents.h
 *
 *  Created on: 10 avril 2015
 *      Author: etienne
 */

#ifndef MSRSYNCEVENTS_H_
#define MSRSYNCEVENTS_H_

#define EVENT_SYNC   15001

#include "events.h"
#include <memory>

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

#endif // MSRSYNCMESSAGES_H_
