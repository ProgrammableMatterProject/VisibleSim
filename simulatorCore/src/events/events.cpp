/*
 * events.cpp
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */


#include <iostream>
#include <sstream>
#include "events.h"
#include "scheduler.h"
#include "../base/blockCode.h"
#include "../stats/statsIndividual.h"

int Event::nextId = 0;
unsigned int Event::nbLivingEvents = 0;

using namespace std;
using namespace BaseSimulator;

//===========================================================================================================
//
//          Event  (class)
//
//===========================================================================================================

Event::Event(Time t) {
    id = nextId;
    nextId++;
    nbLivingEvents++;
    date = t;
    eventType = EVENT_GENERIC;
    randomNumber = 0;
    EVENT_CONSTRUCTOR_INFO();
}

Event::Event(Event *ev) {
    id = nextId;
    nextId++;
    nbLivingEvents++;
    date = ev->date;
    eventType = ev->eventType;
    randomNumber = 0;
    EVENT_CONSTRUCTOR_INFO();
}

Event::~Event() {
    EVENT_DESTRUCTOR_INFO();
    nbLivingEvents--;
}

const string Event::getEventName() {
    return("Generic Event");
}

unsigned int Event::getNextId() {
    return(nextId);
}

unsigned int Event::getNbLivingEvents() {
    return(nbLivingEvents);
}

//===========================================================================================================
//
//          BlockEvent  (class)
//
//===========================================================================================================

BlockEvent::BlockEvent(Time t, BaseSimulator::BuildingBlock *conBlock) : Event(t) {
    EVENT_CONSTRUCTOR_INFO();
    concernedBlock = conBlock;
    eventType = BLOCKEVENT_GENERIC;
}

BlockEvent::BlockEvent(BlockEvent *ev) : Event(ev) {
    EVENT_CONSTRUCTOR_INFO();
    concernedBlock = ev->concernedBlock;
}

BlockEvent::~BlockEvent() {
    EVENT_DESTRUCTOR_INFO();
}

const string BlockEvent::getEventName() {
    return("Generic BlockEvent");
}

//===========================================================================================================
//
//          CodeStartEvent  (class)
//
//===========================================================================================================

CodeStartEvent::CodeStartEvent(Time t, BaseSimulator::BuildingBlock *conBlock): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_CODE_START;
}
CodeStartEvent::~CodeStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void CodeStartEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->blockCode->startup();
}

const string CodeStartEvent::getEventName() {
    return("CodeStart Event");
}

//===========================================================================================================
//
//          CodeEndSimulationEvent  (class)
//
//===========================================================================================================

CodeEndSimulationEvent::CodeEndSimulationEvent(Time t): Event(t) {
    eventType = EVENT_END_SIMULATION;
    EVENT_CONSTRUCTOR_INFO();
}

CodeEndSimulationEvent::~CodeEndSimulationEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void CodeEndSimulationEvent::consume() {
    EVENT_CONSUME_INFO();
    BaseSimulator::getScheduler()->setState(BaseSimulator::Scheduler::ENDED);
}

const string CodeEndSimulationEvent::getEventName() {
    return("CodeEndSimulation Event");
}


//===========================================================================================================
//
//          ProcessLocalEvent  (class)
//
//===========================================================================================================

ProcessLocalEvent::ProcessLocalEvent(Time t, BaseSimulator::BuildingBlock *conBlock): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_PROCESS_LOCAL_EVENT;
}
ProcessLocalEvent::~ProcessLocalEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void ProcessLocalEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->processLocalEvent();
}

const string ProcessLocalEvent::getEventName() {
    return("ProcessLocal Event");
}

//===========================================================================================================
//
//          NetworkInterfaceStartTransmittingEvent  (class)
//
//===========================================================================================================

NetworkInterfaceStartTransmittingEvent::NetworkInterfaceStartTransmittingEvent(Time t, P2PNetworkInterface *ni):Event(t) {
    eventType = EVENT_NI_START_TRANSMITTING;
    interface = ni;
    EVENT_CONSTRUCTOR_INFO();
}
NetworkInterfaceStartTransmittingEvent::~NetworkInterfaceStartTransmittingEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void NetworkInterfaceStartTransmittingEvent::consume() {
    EVENT_CONSUME_INFO();
    interface->send();
}

const string NetworkInterfaceStartTransmittingEvent::getEventName() {
    return("NetworkInterfaceStartTransmitting Event");
}

//===========================================================================================================
//
//          NetworkInterfaceStopTransmittingEvent  (class)
//
//===========================================================================================================

NetworkInterfaceStopTransmittingEvent::NetworkInterfaceStopTransmittingEvent(Time t, P2PNetworkInterface *ni):Event(t) {
    eventType = EVENT_NI_STOP_TRANSMITTING;
    interface = ni;
    EVENT_CONSTRUCTOR_INFO();
}
NetworkInterfaceStopTransmittingEvent::~NetworkInterfaceStopTransmittingEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void NetworkInterfaceStopTransmittingEvent::consume() {
    EVENT_CONSUME_INFO();
    if (!interface->connectedInterface) {
      ERRPUT << "Warning: connection loss, untransmitted message!" << endl;
    } else {
      BaseSimulator::BuildingBlock *receivingBlock = interface->connectedInterface->hostBlock;
      receivingBlock->scheduleLocalEvent(EventPtr(new NetworkInterfaceReceiveEvent(BaseSimulator::getScheduler()->now(), interface->connectedInterface, interface->messageBeingTransmitted)));
      BaseSimulator::utils::StatsIndividual::incReceivedMessageCount(receivingBlock->stats);
      BaseSimulator::utils::StatsIndividual::incIncommingMessageQueueSize(receivingBlock->stats);
    }

    interface->messageBeingTransmitted.reset();
    interface->availabilityDate = BaseSimulator::getScheduler()->now();

    if (interface->outgoingQueue.size() > 0) {
        //cout << "one more to send !!" << endl;
        interface->send();
    }
}

const string NetworkInterfaceStopTransmittingEvent::getEventName() {
    return("NetworkInterfaceStopTransmitting Event");
}

//===========================================================================================================
//
//          NetworkInterfaceReceiveEvent  (class)
//
//===========================================================================================================

NetworkInterfaceReceiveEvent::NetworkInterfaceReceiveEvent(Time t, P2PNetworkInterface *ni, MessagePtr mes):Event(t) {
    eventType = EVENT_NI_RECEIVE;
    interface = ni;
    message = mes;
    EVENT_CONSTRUCTOR_INFO();
}

NetworkInterfaceReceiveEvent::~NetworkInterfaceReceiveEvent() {
    message.reset();
    EVENT_DESTRUCTOR_INFO();
}

void NetworkInterfaceReceiveEvent::consume() {
    EVENT_CONSUME_INFO();
}

const string NetworkInterfaceReceiveEvent::getEventName() {
    return("NetworkInterfaceReceiveEvent Event");
}

//===========================================================================================================
//
//          NetworkInterfaceEnqueueOutgoingEvent  (class)
//
//===========================================================================================================

NetworkInterfaceEnqueueOutgoingEvent::NetworkInterfaceEnqueueOutgoingEvent(Time t, Message *mes, P2PNetworkInterface *ni):Event(t) {
    eventType = EVENT_NI_ENQUEUE_OUTGOING_MESSAGE;
    message = MessagePtr(mes);
    sourceInterface = ni;
    EVENT_CONSTRUCTOR_INFO();
}

NetworkInterfaceEnqueueOutgoingEvent::NetworkInterfaceEnqueueOutgoingEvent(Time t, MessagePtr mes, P2PNetworkInterface *ni):Event(t) {
    eventType = EVENT_NI_ENQUEUE_OUTGOING_MESSAGE;
    message = mes;
    sourceInterface = ni;
    EVENT_CONSTRUCTOR_INFO();
}

NetworkInterfaceEnqueueOutgoingEvent::~NetworkInterfaceEnqueueOutgoingEvent() {
    message.reset();
    EVENT_DESTRUCTOR_INFO();
}

void NetworkInterfaceEnqueueOutgoingEvent::consume() {
    EVENT_CONSUME_INFO();
    sourceInterface->addToOutgoingBuffer(message);
}

const string NetworkInterfaceEnqueueOutgoingEvent::getEventName() {
    return("NetworkInterfaceEnqueueOutgoingEvent Event");
}


//===========================================================================================================
//
//          SetColorEvent  (class)
//
//===========================================================================================================

SetColorEvent::SetColorEvent(Time t, BuildingBlock *conBlock, float r, float g, float b, float a): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_SET_COLOR;
    randomNumber = conBlock->getRandomUint();
    color.set(r*a, g*a, b*a);
}

SetColorEvent::SetColorEvent(Time t, BuildingBlock *conBlock, Color &c): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_SET_COLOR;
    randomNumber = conBlock->getRandomUint();
    color = c;
}

SetColorEvent::SetColorEvent(SetColorEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    color = ev->color;
    //randomNumber = ev->randomNumber;
}

SetColorEvent::~SetColorEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void SetColorEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new SetColorEvent(this)));
}

const string SetColorEvent::getEventName() {
    return("SetColor Event");
}

//===========================================================================================================
//
//          AddNeighborEvent  (class)
//
//===========================================================================================================

AddNeighborEvent::AddNeighborEvent(Time t, BuildingBlock *conBlock, uint64_t f, uint64_t ta): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ADD_NEIGHBOR;
    face = f;
    target = ta;
}

AddNeighborEvent::AddNeighborEvent(AddNeighborEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    face = ev->face;
    target = ev->target;
}

AddNeighborEvent::~AddNeighborEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void AddNeighborEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new AddNeighborEvent(this)));
}

const string AddNeighborEvent::getEventName() {
    return("AddNeighbor Event");
}

//===========================================================================================================
//
//          RemoveNeighborEvent  (class)
//
//===========================================================================================================

RemoveNeighborEvent::RemoveNeighborEvent(Time t, BuildingBlock *conBlock, uint64_t f): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_REMOVE_NEIGHBOR;
    face = f;
}

RemoveNeighborEvent::RemoveNeighborEvent(RemoveNeighborEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    face = ev->face;
}

RemoveNeighborEvent::~RemoveNeighborEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void RemoveNeighborEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new RemoveNeighborEvent(this)));
}

const string RemoveNeighborEvent::getEventName() {
    return("RemoveNeighbor Event");
}

//===========================================================================================================
//
//          TapEvent  (class)
//
//===========================================================================================================

TapEvent::TapEvent(Time t, BuildingBlock *conBlock, const int face):
    BlockEvent(t, conBlock), tappedFace(face) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_TAP;
}

TapEvent::TapEvent(TapEvent *ev) : BlockEvent(ev), tappedFace(ev->tappedFace) {
    EVENT_CONSTRUCTOR_INFO();
}

TapEvent::~TapEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void TapEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new TapEvent(this)));
}

const string TapEvent::getEventName() {
    return("Tap Event");
}

//===========================================================================================================
//
//          AccelEvent  (class)
//
//===========================================================================================================

AccelEvent::AccelEvent(Time t, BuildingBlock *conBlock, uint64_t xx, uint64_t yy, uint64_t zz): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ACCEL;
    x = xx;
    y = yy;
    z = zz;
}

AccelEvent::AccelEvent(AccelEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    x = ev->x;
    y = ev->y;
    z = ev->z;
}

AccelEvent::~AccelEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void AccelEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new AccelEvent(this)));
}

const string AccelEvent::getEventName() {
    return("Accel Event");
}

//===========================================================================================================
//
//          ShakeEvent  (class)
//
//===========================================================================================================

ShakeEvent::ShakeEvent(Time t, BuildingBlock *conBlock, uint64_t f): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_SHAKE;
    force = f;
}

ShakeEvent::ShakeEvent(ShakeEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    force = ev->force;
}

ShakeEvent::~ShakeEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void ShakeEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new ShakeEvent(this)));
}

const string ShakeEvent::getEventName() {
    return("Shake Event");
}

//===========================================================================================================
//
//          InterruptionEvent  (class)
//
//===========================================================================================================

/*template <class T>
InterruptionEvent<T>::InterruptionEvent(Time t, BuildingBlock *conBlock, T c_data): BlockEvent(t, conBlock),data(c_data) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_INTERRUPTION;
}

template <class T>
InterruptionEvent<T>::InterruptionEvent(InterruptionEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    data = ev->data;
}

template <class T>
InterruptionEvent<T>::~InterruptionEvent() {
    EVENT_DESTRUCTOR_INFO();
}

template <class T>
void InterruptionEvent<T>::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new InterruptionEvent(this)));
}*/