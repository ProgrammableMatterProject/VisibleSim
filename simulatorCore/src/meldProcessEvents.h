/*
 * meldProcessEvents.h
 *
 *  Created on: 28 March 2015
 *      Author: andre
 */

#ifndef MELDPROCESSEVENTS_H_
#define MELDPROCESSEVENTS_H_

#include "buildingBlock.h"
#include "events.h"
#include "network.h"
#include "meldProcessDebugger.h"

namespace MeldProcess {

//===========================================================================================================
//
//          VMSetIdEvent  (class)
//
//===========================================================================================================

class VMSetIdEvent : public BlockEvent {
public:

	VMSetIdEvent(Time, BuildingBlock *conBlock);
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

	VMStopEvent(Time, BuildingBlock *conBlock);
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

	VMSendMessageEvent(Time, BuildingBlock *conBlock, Message *mes, P2PNetworkInterface *ni);
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
	DebbuggerVMCommand *command;
	
	VMHandleDebugCommandEvent(Time, BuildingBlock *conBlock, DebbuggerVMCommand *c);
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
	
	VMDebugPauseSimEvent(Time);
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

	VMEndPollEvent(Time, BuildingBlock *conBlock);
	VMEndPollEvent(VMEndPollEvent *ev);
	~VMEndPollEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};

} // MeldProcess namespace


#endif /* MELDPROCESSEVENTS_H_ */
