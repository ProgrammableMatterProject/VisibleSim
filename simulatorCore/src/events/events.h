/*
 * events.h
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#ifndef EVENTS_H_
#define EVENTS_H_


#include <cinttypes>
#include <string>
#include "../base/buildingBlock.h"
#include "uniqueEventsId.h"
#include "../comm/network.h"
#include "../utils/color.h"
#include "../utils/tDefs.h"
#include "../utils/random.h"

using namespace std;
using namespace BaseSimulator;

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
#define EVENT_CONSUME_INFO()				({ stringstream debuginfo1; debuginfo1 << "consuming event " << id << " (" << getEventName() << ")"; Scheduler::trace(debuginfo1.str()); })
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
    BaseSimulator::ruint randomNumber;

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
    virtual const string getEventName() override;

public:
    BaseSimulator::BuildingBlock* getConcernedBlock() override { return concernedBlock;};
    virtual void consumeBlockEvent() = 0;
    virtual void consume() override {
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
    void consumeBlockEvent() override;
    const virtual string getEventName() override;
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
    void consume() override;
    const virtual string getEventName() override;
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
    void consumeBlockEvent() override;
    const virtual string getEventName() override;
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
    void consume() override;
    const virtual string getEventName() override;
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
    void consume() override;
    const virtual string getEventName() override;
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
    void consume() override;
    const virtual string getEventName() override;
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
    void consume() override;
    const virtual string getEventName() override;
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
    void consumeBlockEvent() override;
    const virtual string getEventName() override;
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
    void consumeBlockEvent() override;
    const virtual string getEventName() override;
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
    void consumeBlockEvent() override;
    const virtual string getEventName() override;
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
    void consumeBlockEvent() override;
    const virtual string getEventName() override;
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
    void consumeBlockEvent() override;
    const virtual string getEventName() override;
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
    void consumeBlockEvent() override;
    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          InterruptionEvent  (class)
//
//===========================================================================================================

/** InterruptionEvent is meant to be used as an interruption triggered by one module
 * it is meant to model this behavior to enable module actions such as periodically
 * checking some sensor information or updating its state based on local information **/
template <class T>
class InterruptionEvent : public BlockEvent {
public:
    T data; //!< A used-defined identifier for non general-purpose interruptions

    InterruptionEvent(Time t, BaseSimulator::BuildingBlock *conBlock, T c_data): BlockEvent(t, conBlock),data(c_data) {
        EVENT_CONSTRUCTOR_INFO();
        eventType = EVENT_INTERRUPTION;
    }
    InterruptionEvent(InterruptionEvent *ev) : BlockEvent(ev) {
        EVENT_CONSTRUCTOR_INFO();
        data = ev->data;
    }
    ~InterruptionEvent() {EVENT_DESTRUCTOR_INFO();}
    void consumeBlockEvent() override {
        EVENT_CONSUME_INFO();
        concernedBlock->scheduleLocalEvent(EventPtr(new InterruptionEvent(this)));
    }
    const virtual string getEventName() override { return("InterruptionEvent"); };
};

#endif /* EVENTS_H_ */
