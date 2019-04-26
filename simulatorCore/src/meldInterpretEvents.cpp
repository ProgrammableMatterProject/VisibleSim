/*
 * blinkyBlocksEvents.cpp
 *
 *  Created on: 28 March 2015
 *      Author: andre
 */

#include "meldInterpretEvents.h"
#include "meldInterpretScheduler.h"
#include "meldInterpretVM.h"

using namespace BaseSimulator;

namespace MeldInterpret{

//===========================================================================================================
//
//          VMSetIdEvent  (class)
//
//===========================================================================================================

VMSetIdEvent::VMSetIdEvent(Time t, BuildingBlock *conBlock): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_SET_ID;
}

VMSetIdEvent::VMSetIdEvent(VMSetIdEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

VMSetIdEvent::~VMSetIdEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void VMSetIdEvent::consume() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new VMSetIdEvent(this)));
}

const string VMSetIdEvent::getEventName() {
    return("VMSetId Event");
}

//===========================================================================================================
//
//          VMStopEvent  (class)
//
//===========================================================================================================

VMStopEvent::VMStopEvent(Time t, BuildingBlock *conBlock): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_STOP;
}

VMStopEvent::VMStopEvent(VMStopEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

VMStopEvent::~VMStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void VMStopEvent::consume() {
    EVENT_CONSUME_INFO();
    //concernedBlock->scheduleLocalEvent(EventPtr(new VMStopEvent(this)));
    concernedBlock->blockCode->processLocalEvent(EventPtr(new VMStopEvent(this)));
}

const string VMStopEvent::getEventName() {
    return("VMStop Event");
}

//===========================================================================================================
//
//          VMSendMessageEvent  (class)
//
//===========================================================================================================

VMSendMessageEvent::VMSendMessageEvent(Time t, BuildingBlock *conBlock, MessagePtr mes, P2PNetworkInterface *ni):BlockEvent(t, conBlock) {
    eventType = EVENT_SEND_MESSAGE;
    message = mes;
    sourceInterface = ni;
    randomNumber = conBlock->getRandomUint();
    EVENT_CONSTRUCTOR_INFO();
}

VMSendMessageEvent::VMSendMessageEvent(VMSendMessageEvent *ev) : BlockEvent(ev) {
    message = ev->message;
    sourceInterface = ev->sourceInterface;
    //randomNumber = ev->randomNumber;
    EVENT_CONSTRUCTOR_INFO();
}

VMSendMessageEvent::~VMSendMessageEvent() {
    message.reset();
    EVENT_DESTRUCTOR_INFO();
}

void VMSendMessageEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new VMSendMessageEvent(this)));
}

const string VMSendMessageEvent::getEventName() {
    return("VMSendMessage Event");
}

//===========================================================================================================
//
//          VMSendMessageEvent2  (class)
//
//===========================================================================================================

VMSendMessageEvent2::VMSendMessageEvent2(Time t, BuildingBlock *conBlock, MessagePtr mes,
                                         BaseSimulator::BuildingBlock* _sentto):BlockEvent(t, conBlock) {
    eventType = EVENT_SEND_MESSAGE_TO_BLOCK;
    message = mes;
    target = _sentto;
    randomNumber = conBlock->getRandomUint();
    EVENT_CONSTRUCTOR_INFO();
}

VMSendMessageEvent2::VMSendMessageEvent2(VMSendMessageEvent2 *ev) : BlockEvent(ev) {
    message = ev->message;
    target = ev->target;
    //randomNumber = ev->randomNumber;
    EVENT_CONSTRUCTOR_INFO();
}

VMSendMessageEvent2::~VMSendMessageEvent2() {
    message.reset();
    EVENT_DESTRUCTOR_INFO();
}

void VMSendMessageEvent2::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    //concernedBlock->processLocalEvent();
    concernedBlock->scheduleLocalEvent(EventPtr(new VMSendMessageEvent2(this)));
}

const string VMSendMessageEvent2::getEventName() {
    return("VMSendMessage Event 2");
}

//===========================================================================================================
//
//          VMReceiveMessageEvent2  (class)
//
//===========================================================================================================
VMReceiveMessageEvent2::VMReceiveMessageEvent2(Time t, BaseSimulator::BuildingBlock *conBlock, MessagePtr mes): BlockEvent(t, conBlock) {
  EVENT_CONSTRUCTOR_INFO();
  eventType = EVENT_RECEIVE_MESSAGE_FROM_BLOCK;
  message = mes;
  randomNumber = conBlock->getRandomUint();
}

VMReceiveMessageEvent2::VMReceiveMessageEvent2(VMReceiveMessageEvent2* ev) : BlockEvent(ev) {
  EVENT_CONSTRUCTOR_INFO();
  message = ev->message;
}

VMReceiveMessageEvent2::~VMReceiveMessageEvent2() {
  message.reset();
  EVENT_DESTRUCTOR_INFO();
}

void VMReceiveMessageEvent2::consumeBlockEvent() {
  EVENT_CONSUME_INFO();
  concernedBlock->scheduleLocalEvent(EventPtr(new VMReceiveMessageEvent2(this)));
}

const string VMReceiveMessageEvent2::getEventName() {
  return("VMReceiveMessage Event2");
}

//===========================================================================================================
//
//          VMHandleDebugMessageEvent  (class)
//
//===========================================================================================================

VMHandleDebugCommandEvent::VMHandleDebugCommandEvent(Time t, BuildingBlock *conBlock): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_HANDLE_DEBUG_COMMAND;
    //command = c;
}

VMHandleDebugCommandEvent::VMHandleDebugCommandEvent(VMHandleDebugCommandEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    //command = ev->command;
}

VMHandleDebugCommandEvent::~VMHandleDebugCommandEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void VMHandleDebugCommandEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new VMHandleDebugCommandEvent(this)));
}

const string VMHandleDebugCommandEvent::getEventName() {
    return("VMHandleDebugCommand Event");
}

//===========================================================================================================
//
//          VMDebugPauseSimEvent  (class)
//
//===========================================================================================================

VMDebugPauseSimEvent::VMDebugPauseSimEvent(Time t): Event(t) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_DEBUG_PAUSE_SIMULATION;
}

VMDebugPauseSimEvent::VMDebugPauseSimEvent(VMDebugPauseSimEvent *ev) : Event(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

VMDebugPauseSimEvent::~VMDebugPauseSimEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void VMDebugPauseSimEvent::consume() {
    EVENT_CONSUME_INFO();
    OUTPUT << "pause sim degin" << endl;
    getScheduler()->setState(Scheduler::PAUSED);
    OUTPUT << "pause sim end" << endl;
}

const string VMDebugPauseSimEvent::getEventName() {
    return("VMDebugPauseSim Event");
}

//===========================================================================================================
//
//          VMEndPollEvent  (class)
//
//===========================================================================================================

VMEndPollEvent::VMEndPollEvent(Time t, BuildingBlock *conBlock) : BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    randomNumber = conBlock->getRandomUint();
    eventType = EVENT_END_POLL;
}

VMEndPollEvent::VMEndPollEvent(VMEndPollEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

VMEndPollEvent::~VMEndPollEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void VMEndPollEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new VMEndPollEvent(this)));
    return;
}

const string VMEndPollEvent::getEventName() {
    return("VMEndPoll Event");
}

//===========================================================================================================
//
//          computePredicateEvent  (class)
//
//===========================================================================================================

ComputePredicateEvent::ComputePredicateEvent(Time t, BuildingBlock *conBlock): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_COMPUTE_PREDICATE;
}

ComputePredicateEvent::ComputePredicateEvent(ComputePredicateEvent *ev) : BlockEvent(ev) {
      eventType = EVENT_COMPUTE_PREDICATE;
    EVENT_CONSTRUCTOR_INFO();
}

ComputePredicateEvent::~ComputePredicateEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void ComputePredicateEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new ComputePredicateEvent(this)));
}

const string ComputePredicateEvent::getEventName() {
    return("Compute Predicate Event");
}

//===========================================================================================================
//
//          AddTupleEvent  (class)
//
//===========================================================================================================

AddTupleEvent::AddTupleEvent(Time t, BuildingBlock *conBlock, tuple_t tpl, meld_byte f): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ADD_TUPLE;
    tuple = tpl;
    face = f;
}

AddTupleEvent::AddTupleEvent(AddTupleEvent *ev) : BlockEvent(ev) {
      eventType = EVENT_ADD_TUPLE;
      tuple = ev->tuple;
      face = ev->face;
    EVENT_CONSTRUCTOR_INFO();
}

AddTupleEvent::~AddTupleEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void AddTupleEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new AddTupleEvent(this)));
}

const string AddTupleEvent::getEventName() {
    return("Add Tuple Event");
}

//===========================================================================================================
//
//          RemoveTupleEvent  (class)
//
//===========================================================================================================

RemoveTupleEvent::RemoveTupleEvent(Time t, BuildingBlock *conBlock, tuple_t tpl, meld_byte f): BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_REMOVE_TUPLE;
    tuple = tpl;
    face = f;
}

RemoveTupleEvent::RemoveTupleEvent(RemoveTupleEvent *ev) : BlockEvent(ev) {
      eventType = EVENT_REMOVE_TUPLE;
      tuple = ev->tuple;
      face = ev->face;
    EVENT_CONSTRUCTOR_INFO();
}

RemoveTupleEvent::~RemoveTupleEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void RemoveTupleEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new RemoveTupleEvent(this)));
}

const string RemoveTupleEvent::getEventName() {
    return("Remove Tuple Event");
}

}
