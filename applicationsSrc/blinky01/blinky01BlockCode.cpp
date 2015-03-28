/*
 * blinky01BlockCode.cpp
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <sstream>
#include <boost/asio.hpp> 
#include "scheduler.h"
#include "network.h"
#include "blinky01BlockCode.h"
#include "blinkyBlocksEvents.h"
#include "blinkyBlocksDebugger.h"
#include "trace.h"

using namespace std;
using namespace BlinkyBlocks;
using boost::asio::ip::tcp;

Blinky01BlockCode::Blinky01BlockCode(BlinkyBlocksBlock *host): BlinkyBlocksBlockCode(host) {
	OUTPUT << "Blinky01BlockCode constructor" << endl;
	hasWork = true; // mode fastest
	polling = false; // mode fastest
	currentLocalDate = 0; // mode fastest
	vm = new BlinkyBlocksVM(host);
}

Blinky01BlockCode::~Blinky01BlockCode() {
	OUTPUT << "Blinky01BlockCode destructor" << endl;
	killVM();
}

void Blinky01BlockCode::init() {
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	stringstream info;
	commandType c[5];
	//BlinkyBlocksVM *vm;
	
	lockVM();
	//vm = bb->vm;
	if((vm != NULL)) {
		if((BlinkyBlocks::getScheduler()->getMode() == SCHEDULER_MODE_FASTEST) && !vm->deterministicSet) {
			vm->deterministicSet = true;
			SetDeterministicModeVMCommand determinismCommand(c, bb->blockId);
			vm->sendCommand(determinismCommand);
			info << "deterministic mode set";		
			BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
			OUTPUT << "deterministic mode enable on the VM " << hostBlock->blockId << endl;
		}
		if(!vm->idSent) {
			vm->idSent = true;
			SetIdVMCommand idCommand(c, bb->blockId);	
			vm->sendCommand(idCommand);
			info << "ID sent";			
			BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
			OUTPUT << "ID sent to the VM " << hostBlock->blockId << endl;
		}
	}
	unlockVM();
}

void Blinky01BlockCode::startup() {
	stringstream info;
	
	currentLocalDate = BaseSimulator::getScheduler()->now();
	info << "  Starting Blinky01BlockCode in block " << hostBlock->blockId;
	BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
	init();
}

void Blinky01BlockCode::handleCommand(VMCommand &command) {
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	uint64_t dateToSchedule;
	
	//OUTPUT << "Blinky01BlockCode: type: " << VMCommand::getString(command.getType()) << " size: " << command.getSize() << endl;
	OUTPUT << bb->blockId << " message received: date: " <<command.getTimestamp() << ", type: " << VMCommand::getString(command.getType()) << endl;
	//assert(hasWork); // mode 1
	OUTPUT << "scheduler time " << BlinkyBlocks::getScheduler()->now() << endl;
	OUTPUT << "current local time " << currentLocalDate << "timestamp" << command.getTimestamp() << endl;	
	
	currentLocalDate = max(BlinkyBlocks::getScheduler()->now(), command.getTimestamp());
	if (BlinkyBlocks::getScheduler()->getMode() == SCHEDULER_MODE_FASTEST) {
		//assert(currentLocalDate <= command.getTimestamp()); -- not true because of asynchrone debug commands
		dateToSchedule = currentLocalDate;
	} else {
		dateToSchedule = BlinkyBlocks::getScheduler()->now();
	}
	
	switch (command.getType()) {
		case VM_COMMAND_SET_COLOR:	
			{
			// format: <size> <command> <timestamp> <src> <red> <blue> <green> <intensity>
			SetColorVMCommand c(command.getData());
			Vecteur color = c.getColor();
			BlinkyBlocks::getScheduler()->scheduleLock(new VMSetColorEvent(dateToSchedule, bb, color));
			}
			break;
		case VM_COMMAND_SEND_MESSAGE:
			{
			P2PNetworkInterface *interface;
			SendMessageVMCommand c(command.getData());
			interface = bb->getInterfaceDestId(c.getDestId());
			if (interface == NULL) {
				stringstream info;
				info.str("");
				info << "Warning: sends a message to " << endl << "the non-connected block " << c.getDestId();
				BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
				ERRPUT << "Interface not found" << endl;
				return;
			}
			BlinkyBlocks::getScheduler()->scheduleLock(new VMSendMessageEvent(dateToSchedule, bb,
					new ReceiveMessageVMCommand(c), interface));
			}
			break;
		case VM_COMMAND_DEBUG:
			{
			// Copy the message because it will be queued
			DebbuggerVMCommand *c = new DebbuggerVMCommand(command.getData());
			c->copyData();
			handleDebugCommand(c);
			}
			break;
		case VM_COMMAND_WORK_END:
			{
			WorkEndVMCommand c(command.getData());
			if (c.getNbProcessedMsg() == vm->nbSentCommands) {
					hasWork = false;
			}
			}
			break;
		case VM_COMMAND_TIME_INFO:
			;
			break;
		case VM_COMMAND_POLL_START:
			// Polling lasts 1us
			BlinkyBlocks::getScheduler()->scheduleLock(new VMEndPollEvent(dateToSchedule+1, bb));
			polling = true;
			break;
		default:
			ERRPUT << "*** ERROR *** : unsupported message received from VM (" << command.getType() <<")" << endl;
			break;
	}
}

void Blinky01BlockCode::handleDeterministicMode(VMCommand &command){
	currentLocalDate = max(BaseSimulator::getScheduler()->now(), currentLocalDate);
	if(!hasWork && (command.getType() != VM_COMMAND_STOP)) {
		hasWork = true;
#ifdef TEST_DETER
		//cout << hostBlock->blockId << " has work again at " << BaseSimulator::getScheduler()->now() << endl;
#endif
	}
}

void Blinky01BlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	//BlinkyBlocksVM *vm = bb->vm;
	assert(vm != NULL);
	info.str("");
	
	OUTPUT << bb->blockId << " processLocalEvent: date: "<< BaseSimulator::getScheduler()->now() << " process event " << pev->getEventName() << "(" << pev->eventType << ")" << ", random number : " << pev->randomNumber << endl;

#ifdef TEST_DETER
	cout << bb->blockId << " processLocalEvent: date: "<< BaseSimulator::getScheduler()->now() << " process event " << pev->getEventName() << "(" << pev->eventType << ")" << ", random number : " << pev->randomNumber << endl;
#endif
	switch (pev->eventType) {
		case EVENT_STOP:
			{
			if(BlinkyBlocksVM::isInDebuggingMode()) {
				//getDebugger()->sendTerminateMsg(bb->blockId);
				delete vm;
				vm = NULL;
			} else {
				StopVMCommand command(outBuffer, bb->blockId);
				sendCommand(command);
				killVM();
			}			
			info << "VM stopped";
			}
			break;
		case EVENT_ADD_NEIGHBOR:
			{
			AddNeighborVMCommand command(outBuffer, bb->blockId, (boost::static_pointer_cast<VMAddNeighborEvent>(pev))->target,
				(boost::static_pointer_cast<VMAddNeighborEvent>(pev))->face);
			sendCommand(command);
			info << "Add neighbor "<< (boost::static_pointer_cast<VMAddNeighborEvent>(pev))->target << " at face " << BlinkyBlocks::NeighborDirection::getString(BlinkyBlocks::NeighborDirection::getOpposite((boost::static_pointer_cast<VMAddNeighborEvent>(pev))->face));
			}
			break;
		case EVENT_REMOVE_NEIGHBOR:
			{
			RemoveNeighborVMCommand command(outBuffer, bb->blockId, (boost::static_pointer_cast<VMRemoveNeighborEvent>(pev))->face);
			sendCommand(command);
			info << "Remove neighbor at face " << BlinkyBlocks::NeighborDirection::getString(BlinkyBlocks::NeighborDirection::getOpposite((boost::static_pointer_cast<VMAddNeighborEvent>(pev))->face));
			}
			break;
		case EVENT_TAP:
			{
			TapVMCommand command(outBuffer, bb->blockId);
			sendCommand(command);
			info << "tapped";
			}
			break;
		case EVENT_SET_COLOR:
			{
			Vecteur color = (boost::static_pointer_cast<VMSetColorEvent>(pev))->color;
			bb->setColor(color);
#ifdef TEST_DETER
			cout << bb->blockId << " SET_COLOR_EVENT" << endl;
#endif
			info << "set color "<< color << endl;
			}
			break;
		case EVENT_SEND_MESSAGE:
			{
			MessagePtr message = (boost::static_pointer_cast<VMSendMessageEvent>(pev))->message;
			P2PNetworkInterface *interface = (boost::static_pointer_cast<VMSendMessageEvent>(pev))->sourceInterface;
			BlinkyBlocks::getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(BaseSimulator::getScheduler()->now(),
				message, interface));
			info << "sends a message at face " << NeighborDirection::getString(bb->getDirection(interface))  << " to " << interface->connectedInterface->hostBlock->blockId;
			}
			break;
		case EVENT_RECEIVE_MESSAGE: /*EVENT_NI_RECEIVE: */
			{
			ReceiveMessageVMCommand *command = (ReceiveMessageVMCommand*) (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message.get();
			command->setTimestamp(BlinkyBlocks::getScheduler()->now());
			sendCommand(*command);
#ifdef TEST_DETER
			cout << "message received from " << command->sourceInterface->hostBlock->blockId << endl;
#endif
			info << "message received at face " << NeighborDirection::getString(bb->getDirection(command->sourceInterface->connectedInterface)) << " from " << command->sourceInterface->hostBlock->blockId;
			}
			break;
		case EVENT_ACCEL:
			{
			AccelVMCommand command(outBuffer, bb->blockId, (boost::static_pointer_cast<VMAccelEvent>(pev))->x, (boost::static_pointer_cast<VMAccelEvent>(pev))->y,
			(boost::static_pointer_cast<VMAccelEvent>(pev))->z);
			sendCommand(command);
			info << "accel";
			}
			break;
		case EVENT_SHAKE:
			{
			ShakeVMCommand command(outBuffer, bb->blockId, (boost::static_pointer_cast<VMShakeEvent>(pev))->force);
			sendCommand(command);
			info << "shake";
			}
			break;
		case EVENT_SET_DETERMINISTIC:
			{
			SetDeterministicModeVMCommand command(outBuffer, bb->blockId);
			sendCommand(command);
			OUTPUT << "VM set in deterministic mode " << hostBlock->blockId << endl;
			info << "VM set in deterministic mode";
			}
			break;
		case EVENT_END_POLL: 
			{
			polling = false;
			EndPollVMCommand command(outBuffer, bb->blockId);
			sendCommand(command);
			info << "Polling time period ended" << endl;
			}
			break;
		default:
			ERRPUT << "*** ERROR *** : unknown local event" << endl;
			break;
		}
		BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
}

BlinkyBlocks::BlinkyBlocksBlockCode* Blinky01BlockCode::buildNewBlockCode(BlinkyBlocksBlock *host) {
	return(new Blinky01BlockCode(host));
}


void Blinky01BlockCode::lockVM() {
	if (BlinkyBlocksVM::isInDebuggingMode()) {
		mutex_vm.lock();
	}
}

void Blinky01BlockCode::unlockVM() {
	if (BlinkyBlocksVM::isInDebuggingMode()) {
		mutex_vm.unlock();
	}
}

int Blinky01BlockCode::sendCommand(VMCommand &c) {
	int ret = 0;
	lockVM();
	if(vm != NULL) {
		if ((hostBlock->state == BuildingBlock::ALIVE) || (c.getType() == VM_COMMAND_STOP)) {
			ret = vm->sendCommand(c);
		}
	}
	unlockVM();
	return ret;
}

void Blinky01BlockCode::killVM() {
	lockVM();
	if(vm != NULL) {
		delete vm;
		vm = NULL;
	}
	unlockVM();
}
