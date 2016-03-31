/*
 * events.h
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#ifndef EVENTS_H_
#define EVENTS_H_

//class BuildingBlock;

#include <inttypes.h>
#include <string>
#include <boost/shared_ptr.hpp>
#include "buildingBlock.h"
#include "uniqueEventsId.h"
#include "network.h"

using namespace std;

class Event;

typedef boost::shared_ptr<Event> EventPtr;

#ifdef DEBUG_EVENTS
#define EVENT_CONSTRUCTOR_INFO()			(OUTPUT << getEventName() << " constructor (" << id << ")" << endl)
#define EVENT_DESTRUCTOR_INFO()				(OUTPUT << getEventName() << " destructor (" << id << ")" << endl)
#else
#define EVENT_CONSTRUCTOR_INFO()
#define EVENT_DESTRUCTOR_INFO()
#endif

#ifdef DEBUG_CONSUME_EVENTS
#define EVENT_CONSUME_INFO()				({ stringstream debuginfo1; debuginfo1 << "consuming event " << id << " (" << getEventName() << ")"; Scheduler::trace(debuginfo1.str());})
#else
#define EVENT_CONSUME_INFO()
#endif

//===========================================================================================================
//
//          Event  (class)
//
//===========================================================================================================

class Event {
protected:
	static int nextId;
	static unsigned int nbLivingEvents;

public:
	int id;				// unique ID of the event (mainly for debugging purpose)
	uint64_t date;		// time at which the event will be processed. 0 means simulation start
	int eventType;		// see the various types at the beginning of this file
	int randomNumber;

	Event(uint64_t t);
	Event(Event *ev);
	virtual ~Event();

	virtual void consume() = 0;
	virtual const string getEventName();

	static unsigned int getNextId();
	static unsigned int getNbLivingEvents();
	virtual BaseSimulator::BuildingBlock* getConcernedBlock() { return NULL; };
};

//===========================================================================================================
//
//          BlockEvent  (class)
//
//===========================================================================================================

class BlockEvent : public Event {

protected:
	BaseSimulator::BuildingBlock *concernedBlock;
	BlockEvent(uint64_t t, BaseSimulator::BuildingBlock *conBlock);
	BlockEvent(BlockEvent *ev);
	virtual ~BlockEvent();
	virtual const string getEventName();

public:
	BaseSimulator::BuildingBlock* getConcernedBlock() {return concernedBlock;};
	virtual void consumeBlockEvent() = 0;
	virtual void consume() {
		if (concernedBlock->state >= BaseSimulator::BuildingBlock::ALIVE) {
			this->consumeBlockEvent();
		}
	};
};

//===========================================================================================================
//
//          CodeStartEvent  (class)
//
//===========================================================================================================

class CodeStartEvent : public BlockEvent {
public:

	CodeStartEvent(uint64_t, BaseSimulator::BuildingBlock *conBlock);
	~CodeStartEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          CodeEndSimulationEvent  (class)
//
//===========================================================================================================

class CodeEndSimulationEvent : public Event {
public:

	CodeEndSimulationEvent(uint64_t);
	~CodeEndSimulationEvent();
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          ProcessLocalEvent  (class)
//
//===========================================================================================================

class ProcessLocalEvent : public BlockEvent {
public:

	ProcessLocalEvent(uint64_t, BaseSimulator::BuildingBlock *conBlock);
	~ProcessLocalEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          NetworkInterfaceStartTransmittingEvent  (class)
//
//===========================================================================================================

class NetworkInterfaceStartTransmittingEvent : public Event {
public:
	P2PNetworkInterface *interface;

	NetworkInterfaceStartTransmittingEvent(uint64_t, P2PNetworkInterface *ni);
	~NetworkInterfaceStartTransmittingEvent();
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          NetworkInterfaceStopTransmittingEvent  (class)
//
//===========================================================================================================

class NetworkInterfaceStopTransmittingEvent : public Event {
public:
	P2PNetworkInterface *interface;

	NetworkInterfaceStopTransmittingEvent(uint64_t, P2PNetworkInterface *ni);
	~NetworkInterfaceStopTransmittingEvent();
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          NetworkInterfaceReceiveEvent  (class)
//
//===========================================================================================================

class NetworkInterfaceReceiveEvent : public Event {
public:
	P2PNetworkInterface *interface;
	MessagePtr message;
	NetworkInterfaceReceiveEvent(uint64_t,P2PNetworkInterface *ni, MessagePtr mes);
	~NetworkInterfaceReceiveEvent();
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          NetworkInterfaceEnqueueOutgoingEvent  (class)
//
//===========================================================================================================

class NetworkInterfaceEnqueueOutgoingEvent : public Event {
public:
	MessagePtr message;
	P2PNetworkInterface *sourceInterface;

	NetworkInterfaceEnqueueOutgoingEvent(uint64_t, Message *mes, P2PNetworkInterface *ni);
	NetworkInterfaceEnqueueOutgoingEvent(uint64_t, MessagePtr mes, P2PNetworkInterface *ni);
	~NetworkInterfaceEnqueueOutgoingEvent();
	void consume();
	const virtual string getEventName();
};

#endif /* EVENTS_H_ */
