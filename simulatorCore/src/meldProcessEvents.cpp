/*
 * blinkyBlocksEvents.cpp
 *
 *  Created on: 28 March 2015
 *      Author: andre
 */

#include "meldProcessEvents.h"
#include "meldProcessScheduler.h"

namespace MeldProcess {

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

VMSendMessageEvent::VMSendMessageEvent(Time t, BuildingBlock *conBlock, Message *mes, P2PNetworkInterface *ni):BlockEvent(t, conBlock) {
	eventType = EVENT_SEND_MESSAGE;
	message = MessagePtr(mes);
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
//          VMHandleDebugMessageEvent  (class)
//
//===========================================================================================================

VMHandleDebugCommandEvent::VMHandleDebugCommandEvent(Time t, BuildingBlock *conBlock, DebbuggerVMCommand *c): BlockEvent(t, conBlock) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_HANDLE_DEBUG_COMMAND;
	command = c;
}

VMHandleDebugCommandEvent::VMHandleDebugCommandEvent(VMHandleDebugCommandEvent *ev) : BlockEvent(ev) {
	EVENT_CONSTRUCTOR_INFO();
	command = ev->command;
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

} // BlinkyBlocks namespace
