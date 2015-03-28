/*
 * meldProcessVMCommands.cpp
 *
 *  Created on: 11 aout 2013
 *      Author: Andre
 */

#include "meldProcessVMCommands.h"
#include "scheduler.h"

// Format: <content size (in bytes)> <type> <timestamp> <src> <parameter1> ...

#define CONTENT_SIZE 0
#define TYPE 1
#define TIMESTAMP 2
#define NODE	3
#define PARAM1	4 
#define PARAM2	5
#define PARAM3	6

namespace MeldProcess {
	
//===========================================================================================================
//
//          VMCommand  (class)
//
//===========================================================================================================

VMCommand::VMCommand(commandType *d, commandType s, commandType t, commandType src) {
	data = d;
	data[CONTENT_SIZE] = s;
	data[TYPE] = t;
	data[TIMESTAMP] = BaseSimulator::getScheduler()->now();
	data[NODE] = src;
}

VMCommand::VMCommand(commandType *d) {
	data = d;
}

commandType VMCommand::getType(commandType *d) {
	return d[TYPE];
}

commandType VMCommand::getTimestamp(commandType *d) {
	return d[TIMESTAMP];
}

commandType VMCommand::getContentSize() {
	return data[CONTENT_SIZE];
}

commandType VMCommand::getSize() {
	return getContentSize() + sizeof(commandType);
}

commandType VMCommand::getType() {
	return data[TYPE];
}

commandType VMCommand::getTimestamp() {
	return data[TIMESTAMP];
}

commandType VMCommand::getSource() {
	return data[NODE];
}

commandType* VMCommand::getData() {
	return data;
}

void VMCommand::setTimestamp(commandType ts) {
	data[TIMESTAMP] = ts;
}

void VMCommand::copyData() {
	commandType size = getSize();
	commandType* copy = new uint64_t[size/sizeof(commandType)+1];
	memcpy(copy, data, size);
	data = copy;
}

string VMCommand::getString(commandType t) {
	switch(t) {
		case VM_COMMAND_SET_COLOR:
			return string("VM_COMMAND_SET_COLOR");
			break;
		case VM_COMMAND_SEND_MESSAGE:
			return string("VM_COMMAND_SEND_MESSAGE");
			break;
		case VM_COMMAND_DEBUG:
			return string("VM_COMMAND_DEBUG");
			break;
		case VM_COMMAND_WORK_END:
			return string("VM_COMMAND_WORK_END");
			break;
		case VM_COMMAND_TIME_INFO:
			return string("VM_MESSAGE_TIME_INFO");
		case VM_COMMAND_POLL_START:
			return string("VM_MESSAGE_POLL_START");
		case VM_COMMAND_END_POLL:
			return string("VM_COMMAND_END_POLL");
		default:
			ERRPUT << "Unknown received-message type" << endl;
			return string("Unknown");
			break;
		}
}

//===========================================================================================================
//
//          SetIdVMCommand  (class)
//
//===========================================================================================================

SetIdVMCommand::SetIdVMCommand(commandType *d, commandType src): 
	VMCommand(d, 3*sizeof(commandType), VM_COMMAND_SET_ID, src) { };

//===========================================================================================================
//
//          StopVMCommand  (class)
//
//===========================================================================================================
StopVMCommand::StopVMCommand(commandType *d, commandType src):
		VMCommand(d, 3*sizeof(commandType), VM_COMMAND_STOP, src) { };

//===========================================================================================================
//
//          AddNeighborVMCommand  (class)
//
//===========================================================================================================

AddNeighborVMCommand::AddNeighborVMCommand(commandType *d, commandType src,
		commandType t, commandType f):
		VMCommand(d, 5*sizeof(commandType), VM_COMMAND_ADD_NEIGHBOR, src) {
		data[PARAM1] = t;
		data[PARAM2] = f;
}

//===========================================================================================================
//
//          RemoveNeighborVMCommand  (class)
//
//===========================================================================================================

RemoveNeighborVMCommand::RemoveNeighborVMCommand(commandType *d, commandType src, commandType f):
		VMCommand(d, 4*sizeof(commandType), VM_COMMAND_REMOVE_NEIGHBOR, src) {
		data[PARAM1] = f;
}

//===========================================================================================================
//
//          TapVMCommand  (class)
//
//===========================================================================================================

TapVMCommand::TapVMCommand(commandType *d, commandType src) : 
	VMCommand(d, 3*sizeof(commandType), VM_COMMAND_TAP, src) { };

//===========================================================================================================
//
//          SetColorVMCommand  (class)
//
//===========================================================================================================

SetColorVMCommand::SetColorVMCommand(commandType* d): VMCommand(d) {};

Vecteur SetColorVMCommand::getColor() {
	return Vecteur((float)data[4]/255.0, (float)data[5]/255.0, (float)data[6]/255.0, (float)data[7]/255.0);
}


//===========================================================================================================
//
//          SendMessageVMCommand  (class)
//
//===========================================================================================================
// format: <size> <command> <timestamp> <src> <destFace=0: not used> <destId> < <content...>

SendMessageVMCommand::SendMessageVMCommand(commandType *d): VMCommand(d) {};
	
commandType SendMessageVMCommand::getDestFace() {
	return data[PARAM1];
}

commandType SendMessageVMCommand::getDestId() {
	return data[PARAM2];
}

//===========================================================================================================
//
//          ReceiveMessageVMCommand  (class)
//
//===========================================================================================================

ReceiveMessageVMCommand::ReceiveMessageVMCommand(SendMessageVMCommand &c) : VMCommand(c.getData()) {
	this->copyData();
	data[TYPE] = VM_COMMAND_RECEIVE_MESSAGE;
	data[PARAM2] = c.getSource();
	data[NODE] = c.getDestId();
}

unsigned int ReceiveMessageVMCommand::size() { 
	//return (getContentSize() - 5*sizeof(commandType)); // data size only
	return 17; // bad fix, BB have fixed size data packet
}

ReceiveMessageVMCommand::~ReceiveMessageVMCommand() {
	delete[] data;
}


//===========================================================================================================
//
//          AccelVMCommand  (class)
//
//===========================================================================================================

AccelVMCommand::AccelVMCommand(commandType *d, commandType src, commandType x, commandType y, commandType z) : 
	VMCommand(d, 6*sizeof(commandType), VM_COMMAND_ACCEL, src) {
	data[PARAM1] = x;
	data[PARAM2] = y;
	data[PARAM3] = z;
}

//===========================================================================================================
//
//          ShakeVMCommand  (class)
//
//===========================================================================================================

ShakeVMCommand::ShakeVMCommand(commandType *d, commandType src, commandType f): 
	VMCommand(d, 4*sizeof(commandType), VM_COMMAND_SHAKE, src) {
	data[PARAM1] = f;
}

//===========================================================================================================
//
//          SetDeterministicModeVMCommand  (class)
//
//===========================================================================================================

SetDeterministicModeVMCommand::SetDeterministicModeVMCommand(commandType *d, commandType src):
	VMCommand(d, 3*sizeof(commandType), VM_COMMAND_SET_DETERMINISTIC_MODE, src) {};

//===========================================================================================================
//
//          WorkEndVMCommand  (class)
//
//===========================================================================================================

WorkEndVMCommand::WorkEndVMCommand(commandType *d) : VMCommand(d) {};

commandType WorkEndVMCommand::getNbProcessedMsg() { return data[PARAM1]; }

//===========================================================================================================
//
//          DebbuggerVMCommand  (class)
//
//===========================================================================================================

DebbuggerVMCommand::DebbuggerVMCommand(commandType *d) : VMCommand(d) {};

commandType DebbuggerVMCommand::getDebuggerCommand() {
	int *d = (int*) (data+PARAM1);	
	return d[1];
}

//===========================================================================================================
//
//          EndPollVMCommand  (class)
//
//===========================================================================================================

EndPollVMCommand::EndPollVMCommand(commandType *d, commandType src):
	VMCommand(d, 3*sizeof(commandType), VM_COMMAND_END_POLL, src) {};
	
}
