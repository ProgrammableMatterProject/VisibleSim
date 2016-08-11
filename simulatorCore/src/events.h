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
#include "buildingBlock.h"
#include "uniqueEventsId.h"
#include "network.h"
#include "color.h"
#include "tDefs.h"

using namespace std;

class Event;

typedef std::shared_ptr<Event> EventPtr;

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
	int id;				//!< unique ID of the event (mainly for debugging purpose)
	Time date;		//!< time at which the event will be processed. 0 means simulation start
	int eventType;		//!< see the various types at the beginning of this file
        ruint randomNumber;

	Event(Time t);
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
	BlockEvent(Time t, BaseSimulator::BuildingBlock *conBlock);
	BlockEvent(BlockEvent *ev);
	virtual ~BlockEvent();
	virtual const string getEventName();

public:
	BaseSimulator::BuildingBlock* getConcernedBlock() {return concernedBlock;};
	virtual void consumeBlockEvent() = 0;
	virtual void consume() {
		if (concernedBlock->getState() >= BaseSimulator::BuildingBlock::ALIVE) {
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

	CodeStartEvent(Time, BaseSimulator::BuildingBlock *conBlock);
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

	CodeEndSimulationEvent(Time);
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

	ProcessLocalEvent(Time, BaseSimulator::BuildingBlock *conBlock);
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

	NetworkInterfaceStartTransmittingEvent(Time, P2PNetworkInterface *ni);
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

	NetworkInterfaceStopTransmittingEvent(Time, P2PNetworkInterface *ni);
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
	NetworkInterfaceReceiveEvent(Time,P2PNetworkInterface *ni, MessagePtr mes);
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

	NetworkInterfaceEnqueueOutgoingEvent(Time, Message *mes, P2PNetworkInterface *ni);
	NetworkInterfaceEnqueueOutgoingEvent(Time, MessagePtr mes, P2PNetworkInterface *ni);
	~NetworkInterfaceEnqueueOutgoingEvent();
	void consume();
	const virtual string getEventName();
};


//===========================================================================================================
//
//          SetColorEvent  (class)
//
//===========================================================================================================

class SetColorEvent : public BlockEvent {
public:
	Color color;

	SetColorEvent(Time, BaseSimulator::BuildingBlock *conBlock, float r, float g, float b, float a);
	SetColorEvent(Time, BaseSimulator::BuildingBlock *conBlock, Color &c);
	SetColorEvent(SetColorEvent *ev);
	~SetColorEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};


//===========================================================================================================
//
//          AddNeighborEvent  (class)
//
//===========================================================================================================

class AddNeighborEvent : public BlockEvent {
public:
	uint64_t face;
	uint64_t target;

	AddNeighborEvent(Time, BaseSimulator::BuildingBlock *conBlock, uint64_t f, uint64_t ta);
	AddNeighborEvent(AddNeighborEvent *ev);
	~AddNeighborEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          RemoveNeighborEvent  (class)
//
//===========================================================================================================

class RemoveNeighborEvent : public BlockEvent {
public:
	uint64_t face;

	RemoveNeighborEvent(Time, BaseSimulator::BuildingBlock *conBlock, uint64_t f);
	RemoveNeighborEvent(RemoveNeighborEvent *ev);
	~RemoveNeighborEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          TapEvent  (class)
//
//===========================================================================================================

class TapEvent : public BlockEvent {
public:
	const int tappedFace;

	TapEvent(Time, BaseSimulator::BuildingBlock *conBlock, const int face);
	TapEvent(TapEvent *ev);
	~TapEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};


//===========================================================================================================
//
//          AccelEvent  (class)
//
//===========================================================================================================

class AccelEvent : public BlockEvent {
public:
	uint64_t x;
	uint64_t y;
	uint64_t z;

	AccelEvent(Time, BaseSimulator::BuildingBlock *conBlock, uint64_t xx, uint64_t yy, uint64_t zz);
	AccelEvent(AccelEvent *ev);
	~AccelEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          ShakeEvent  (class)
//
//===========================================================================================================

class ShakeEvent : public BlockEvent {
public:
	uint64_t force;

	ShakeEvent(Time, BaseSimulator::BuildingBlock *conBlock, uint64_t f);
	ShakeEvent(ShakeEvent *ev);
	~ShakeEvent();
	void consumeBlockEvent();
	const virtual string getEventName();
};



#endif /* EVENTS_H_ */
