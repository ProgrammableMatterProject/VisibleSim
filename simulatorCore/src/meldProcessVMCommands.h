/*
 * BlinkyBlocksVMCommands.h
 *
 *  Created on: 11 aout 2013
 *      Author: Andre
 */

#ifndef MELDPROCESSVMCOMMANDS_H_
#define MELDPROCESSVMCOMMANDS_H_

#include "network.h"
#include "color.h"

#define VM_COMMAND_MAX_LENGHT_BYTES 544 // debugger
#define VM_COMMAND_TYPE_SIZE sizeof(commandType)
#define VM_COMMAND_MAX_LENGHT (VM_COMMAND_MAX_LENGHT_BYTES/VM_COMMAND_TYPE_SIZE)

#define VM_COMMAND_SET_ID						1
#define VM_COMMAND_STOP							4
#define VM_COMMAND_ADD_NEIGHBOR					5
#define VM_COMMAND_REMOVE_NEIGHBOR				6
#define VM_COMMAND_TAP							7
#define VM_COMMAND_SET_COLOR					8
#define VM_COMMAND_SEND_MESSAGE					12
#define VM_COMMAND_RECEIVE_MESSAGE				13
#define VM_COMMAND_ACCEL						14
#define VM_COMMAND_SHAKE						15
#define VM_COMMAND_DEBUG						16

#define VM_COMMAND_SET_DETERMINISTIC_MODE		20
#define VM_COMMAND_POLL_START					21
#define VM_COMMAND_END_POLL						22
#define VM_COMMAND_WORK_END						23

#define VM_COMMAND_TIME_INFO					24

typedef uint64_t commandType;

namespace MeldProcess {

//===========================================================================================================
//
//          VMCommand  (class)
//
//===========================================================================================================

class VMCommand {

protected:
	commandType *data;
	VMCommand(commandType *d, commandType s, commandType t, commandType src);
public:

	VMCommand(commandType *d);
	static commandType getType(commandType *d);
	static commandType getTimestamp(commandType *d);
	
	commandType getContentSize();
	commandType getSize();
	commandType getType();
	commandType getTimestamp();
	commandType getSource();
	commandType* getData();
	
	void setTimestamp(commandType ts);
	void copyData();
	
	static string getString(commandType t);
	
};


//===========================================================================================================
//
//          SetColorVMCommand  (class)
//
//===========================================================================================================

class SetColorVMCommand : public VMCommand {
public:
	SetColorVMCommand(commandType *d);		
	Color getColor();
};


//===========================================================================================================
//
//          SendMessageVMCommand  (class)
//
//===========================================================================================================

class SendMessageVMCommand : public VMCommand, public Message {
public:
	SendMessageVMCommand(commandType *d);
	
	commandType getDestId();
	commandType getDestFace();
};

//===========================================================================================================
//
//          SetIdVMCommand  (class)
//
//===========================================================================================================

class SetIdVMCommand : public VMCommand {
public:
	SetIdVMCommand(commandType *d, commandType src);	
};

//===========================================================================================================
//
//          StopVMCommand  (class)
//
//===========================================================================================================

class StopVMCommand : public VMCommand {
public:
	StopVMCommand(commandType *d, commandType src);	
};

//===========================================================================================================
//
//          AddNeighborVMCommand  (class)
//
//===========================================================================================================

class AddNeighborVMCommand : public VMCommand {
public:
	AddNeighborVMCommand(commandType *d, commandType src, commandType t, commandType f);
};

//===========================================================================================================
//
//          RemoveNeighborVMCommand  (class)
//
//===========================================================================================================

class RemoveNeighborVMCommand : public VMCommand {
public:
	RemoveNeighborVMCommand(commandType *d, commandType src, commandType f);	
};

//===========================================================================================================
//
//          TapVMCommand  (class)
//
//===========================================================================================================

class TapVMCommand : public VMCommand {
public:
	TapVMCommand(commandType *d, commandType src);	
};

//===========================================================================================================
//
//          ReceiveMessageVMCommand  (class)
//
//===========================================================================================================

class ReceiveMessageVMCommand : public VMCommand, public Message {
public:
	ReceiveMessageVMCommand(SendMessageVMCommand &c);
	unsigned int size(); // redefine virtual size function of Message
	~ReceiveMessageVMCommand();
};

//===========================================================================================================
//
//          AccelVMCommand  (class)
//
//===========================================================================================================

class AccelVMCommand : public VMCommand {
public:
	AccelVMCommand(commandType *d, commandType src, commandType x, commandType y, commandType z);	
};

//===========================================================================================================
//
//          ShakeVMCommand  (class)
//
//===========================================================================================================

class ShakeVMCommand : public VMCommand {
public:
	ShakeVMCommand(commandType *d, commandType src, commandType f);	
};

//===========================================================================================================
//
//          SetDeterministicModeVMCommand  (class)
//
//===========================================================================================================

class SetDeterministicModeVMCommand : public VMCommand {
public:
	SetDeterministicModeVMCommand(commandType *d, commandType src);	
};

//===========================================================================================================
//
//          WorkEndVMCommand  (class)
//
//===========================================================================================================

class WorkEndVMCommand : public VMCommand {
public:
	
	WorkEndVMCommand(commandType *d);
	commandType getNbProcessedMsg();
};

//===========================================================================================================
//
//          DebbuggerVMCommand  (class)
//
//===========================================================================================================

class DebbuggerVMCommand : public VMCommand {
public:
	DebbuggerVMCommand(commandType *d);
	commandType getDebuggerCommand();
};

//===========================================================================================================
//
//          EndPollVMCommand  (class)
//
//===========================================================================================================

class EndPollVMCommand : public VMCommand {
public:
	EndPollVMCommand(commandType *d, commandType src);
};

}
#endif
