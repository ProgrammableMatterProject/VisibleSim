#ifndef MELDINTERPEVENTS_H_
#define MELDINTERPEVENTS_H_

#include "buildingBlock.h"
#include "meldInterpretVMCore.h"
#include "events.h"
#include "network.h"

//We'll see later for a debugger
//#include "meldProcessDebugger.h"

namespace MeldInterpret {

//===========================================================================================================
//
//          VMSetIdEvent  (class)
//
//===========================================================================================================

class VMSetIdEvent : public BlockEvent {
public:

	VMSetIdEvent(uint64_t, BaseSimulator::BuildingBlock *conBlock);
	VMSetIdEvent(VMSetIdEvent *ev);
	~VMSetIdEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          VMStopEvent  (class)
//
//===========================================================================================================

class VMStopEvent : public BlockEvent {
public:

	VMStopEvent(uint64_t, BaseSimulator::BuildingBlock *conBlock);
	VMStopEvent(VMStopEvent *ev);
	~VMStopEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};


//===========================================================================================================
//
//          VMSendMessageEvent  (class)
//
//===========================================================================================================

class VMSendMessageEvent : public BlockEvent {
public:
	MessagePtr message;
	P2PNetworkInterface *sourceInterface;

	VMSendMessageEvent(uint64_t, BaseSimulator::BuildingBlock *conBlock, MessagePtr mes, P2PNetworkInterface *ni);
	VMSendMessageEvent(VMSendMessageEvent *ev);
	~VMSendMessageEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          VMHandleDebugCommandEvent  (class)
//
//===========================================================================================================

class VMHandleDebugCommandEvent : public BlockEvent {

public:

	VMHandleDebugCommandEvent(uint64_t, BaseSimulator::BuildingBlock *conBlock);
	VMHandleDebugCommandEvent(VMHandleDebugCommandEvent *ev);
	~VMHandleDebugCommandEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          VMDebugMessageEvent  (class)
//
//===========================================================================================================

class VMDebugPauseSimEvent : public Event {

public:

	VMDebugPauseSimEvent(uint64_t);
	VMDebugPauseSimEvent(VMDebugPauseSimEvent *ev);
	~VMDebugPauseSimEvent();
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          VMEndPollEvent  (class)
//
//===========================================================================================================

class VMEndPollEvent : public BlockEvent {
public:

	VMEndPollEvent(uint64_t, BaseSimulator::BuildingBlock *conBlock);
	VMEndPollEvent(VMEndPollEvent *ev);
	~VMEndPollEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};


//===========================================================================================================
//
//          ComputePredicateEvent  (class)
//
//===========================================================================================================

class ComputePredicateEvent : public BlockEvent {
public:
	//No idea for members for the moment

	ComputePredicateEvent(uint64_t, BaseSimulator::BuildingBlock *conBlock);
	ComputePredicateEvent(ComputePredicateEvent *ev);
	~ComputePredicateEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          AddTupleEvent  (class)
//
//===========================================================================================================

class AddTupleEvent : public BlockEvent {
public:
	tuple_t tuple;
	byte interface;

	AddTupleEvent(uint64_t, BaseSimulator::BuildingBlock *conBlock, tuple_t tpl, byte ni);
	AddTupleEvent(AddTupleEvent *ev);
	~AddTupleEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};


//===========================================================================================================
//
//          RemoveTupleEvent  (class)
//
//===========================================================================================================

class RemoveTupleEvent : public BlockEvent {
public:
	tuple_t tuple;
	byte interface;

	RemoveTupleEvent(uint64_t, BaseSimulator::BuildingBlock *conBlock, tuple_t tpl, byte ni);
	RemoveTupleEvent(RemoveTupleEvent *ev);
	~RemoveTupleEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};


} // MeldInterpret namespace


#endif /* MELDINTERPEVENTS_H_ */
