#ifndef LcmEvent_H_
#define LcmEvent_H_

#include "compressFullRangeBlockCode.hpp"
//using namespace SlidingCubes;
const int EVENT_LOOK    = 2001;
const int EVENT_COMPUTE = 2002;
const int EVENT_MOVE    = 2003;
const int EVENT_CHANGE_LIGHT = 2004;
class LookEvent : public InterruptionEvent<int> {
public:
	LookEvent(Time t, SlidingCubesBlock* conBlock)
	    : InterruptionEvent<int>(t, conBlock, 0) {
		EVENT_CONSTRUCTOR_INFO();
		eventType = EVENT_LOOK;
	}
	LookEvent(LookEvent* ev) : InterruptionEvent<int>(ev) {
		EVENT_CONSTRUCTOR_INFO();
	}
	~LookEvent() { EVENT_DESTRUCTOR_INFO(); };

	void consumeBlockEvent() override {
		EVENT_CONSUME_INFO();
		concernedBlock->scheduleLocalEvent(EventPtr(new LookEvent(this)));
	}
	const virtual string getEventName() override { return ("LookEvent"); };
};

class ComputeEvent : public InterruptionEvent<int> {
public:
	ComputeEvent(Time t, SlidingCubesBlock* conBlock)
	    : InterruptionEvent<int>(t, conBlock, 0) {
		EVENT_CONSTRUCTOR_INFO();
		eventType = EVENT_COMPUTE;
	}
	ComputeEvent(ComputeEvent* ev) : InterruptionEvent<int>(ev) {
		EVENT_CONSTRUCTOR_INFO();
	}
	~ComputeEvent() { EVENT_DESTRUCTOR_INFO(); };
	void consumeBlockEvent() override {
		EVENT_CONSUME_INFO();
		concernedBlock->scheduleLocalEvent(EventPtr(new ComputeEvent(this)));
	}
	const virtual string getEventName() override { return ("ComputeEvent"); };
};

class MoveEvent : public InterruptionEvent<int> {
public:
	MoveEvent(Time t, SlidingCubesBlock* conBlock)
	    : InterruptionEvent<int>(t, conBlock, 0) {
		EVENT_CONSTRUCTOR_INFO();
		eventType = EVENT_MOVE;
	}
	MoveEvent(MoveEvent* ev) : InterruptionEvent<int>(ev) {
		EVENT_CONSTRUCTOR_INFO();
	}
	~MoveEvent() { EVENT_DESTRUCTOR_INFO(); };
	void consumeBlockEvent() override {
		EVENT_CONSUME_INFO();
		concernedBlock->scheduleLocalEvent(EventPtr(new MoveEvent(this)));
	}
	const virtual string getEventName() override { return ("MoveEvent"); };
};

class ChangeLightEvent : public InterruptionEvent<int> {
public:
	ChangeLightEvent(Time t, SlidingCubesBlock* conBlock)
	    : InterruptionEvent<int>(t, conBlock, 0) {
		EVENT_CONSTRUCTOR_INFO();
		eventType = EVENT_CHANGE_LIGHT;
	}
	ChangeLightEvent(ChangeLightEvent* ev) : InterruptionEvent<int>(ev) {
		EVENT_CONSTRUCTOR_INFO();
	}
	~ChangeLightEvent() { EVENT_DESTRUCTOR_INFO(); };
	void consumeBlockEvent() override {
		EVENT_CONSUME_INFO();
		concernedBlock->scheduleLocalEvent(EventPtr(new ChangeLightEvent(this)));
	}
	const virtual string getEventName() override { return ("ChangeLightEvent"); };
};

#endif  // LcmEvent_H_