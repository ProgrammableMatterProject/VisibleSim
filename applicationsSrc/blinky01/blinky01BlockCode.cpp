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
#include "blinkyBlocksBlock.h"

#include "meldProcessEvents.h"
#include "meldProcessDebugger.h"

#include "trace.h"

using namespace std;
using namespace MeldProcess;
using namespace BlinkyBlocks;
using boost::asio::ip::tcp;

Blinky01BlockCode::Blinky01BlockCode(BlinkyBlocksBlock *host): BlinkyBlocksBlockCode(host) {
	OUTPUT << "Blinky01BlockCode constructor" << endl;
	vm = new MeldProcessVM(host);
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
		if((MeldProcess::getScheduler()->getMode() == SCHEDULER_MODE_FASTEST) && !vm->deterministicSet) {
			vm->deterministicSet = true;
			SetDeterministicModeVMCommand determinismCommand(c, bb->blockId);
			vm->sendCommand(determinismCommand);
			info << "deterministic mode set";		
			MeldProcess::getScheduler()->trace(info.str(),hostBlock->blockId);
			OUTPUT << "deterministic mode enable on the VM " << hostBlock->blockId << endl;
		}
		if(!vm->idSent) {
			vm->idSent = true;
			SetIdVMCommand idCommand(c, bb->blockId);	
			vm->sendCommand(idCommand);
			info << "ID sent";			
			MeldProcess::getScheduler()->trace(info.str(),hostBlock->blockId);
			OUTPUT << "ID sent to the VM " << hostBlock->blockId << endl;
		}
	}
	unlockVM();
}

void Blinky01BlockCode::startup() {
	stringstream info;
	
	vm->setCurrentLocalDate(MeldProcess::getScheduler()->now());
	info << "  Starting Blinky01BlockCode in block " << hostBlock->blockId;
	MeldProcess::getScheduler()->trace(info.str(),hostBlock->blockId);
	init();
}

void Blinky01BlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	assert(vm != NULL);
	info.str("");
	
	OUTPUT << bb->blockId << " processLocalEvent: date: "<< BaseSimulator::getScheduler()->now() << " process event " << pev->getEventName() << "(" << pev->eventType << ")" << ", random number : " << pev->randomNumber << endl;

	switch (pev->eventType) {
		case EVENT_STOP:
			{
			if(MeldProcessVM::isInDebuggingMode()) {
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
			AddNeighborVMCommand command(outBuffer, bb->blockId, (boost::static_pointer_cast<AddNeighborEvent>(pev))->target,
				(boost::static_pointer_cast<AddNeighborEvent>(pev))->face);
				sendCommand(command);
				info << "Add neighbor "<< (boost::static_pointer_cast<AddNeighborEvent>(pev))->target << " at face " << BlinkyBlocks::NeighborDirection::getString(BlinkyBlocks::NeighborDirection::getOpposite((boost::static_pointer_cast<AddNeighborEvent>(pev))->face));
			}
			break;
		case EVENT_REMOVE_NEIGHBOR:
			{
			RemoveNeighborVMCommand command(outBuffer, bb->blockId, (boost::static_pointer_cast<RemoveNeighborEvent>(pev))->face);
			sendCommand(command);
			info << "Remove neighbor at face " << BlinkyBlocks::NeighborDirection::getString(BlinkyBlocks::NeighborDirection::getOpposite((boost::static_pointer_cast<RemoveNeighborEvent>(pev))->face));
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
			Color color = (boost::static_pointer_cast<SetColorEvent>(pev))->color;
			bb->setColor(color);
			info << "set color "<< color;
			}
			break;
		case EVENT_SEND_MESSAGE:
			{
			MessagePtr message = (boost::static_pointer_cast<VMSendMessageEvent>(pev))->message;
			P2PNetworkInterface *interface = (boost::static_pointer_cast<VMSendMessageEvent>(pev))->sourceInterface;
			MeldProcess::getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(BaseSimulator::getScheduler()->now(),
				message, interface));
			info << "sends a message at face " << NeighborDirection::getString(bb->getDirection(interface))  << " to " << interface->connectedInterface->hostBlock->blockId;
			}
			break;
		case EVENT_RECEIVE_MESSAGE: /*EVENT_NI_RECEIVE: */
			{
			ReceiveMessageVMCommand *command = (ReceiveMessageVMCommand*) (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message.get();
			command->setTimestamp(MeldProcess::getScheduler()->now());
			sendCommand(*command);
			info << "message received at face " << NeighborDirection::getString(bb->getDirection(command->sourceInterface->connectedInterface)) << " from " << command->sourceInterface->hostBlock->blockId;
			}
			break;
		case EVENT_ACCEL:
			{
			AccelVMCommand command(outBuffer, bb->blockId, (boost::static_pointer_cast<AccelEvent>(pev))->x, (boost::static_pointer_cast<AccelEvent>(pev))->y,
			(boost::static_pointer_cast<AccelEvent>(pev))->z);
			sendCommand(command);
			info << "accel";
			}
			break;
		case EVENT_SHAKE:
			{
			ShakeVMCommand command(outBuffer, bb->blockId, (boost::static_pointer_cast<ShakeEvent>(pev))->force);
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
			vm->setPolling(false);
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
	if (MeldProcessVM::isInDebuggingMode()) {
		mutex_vm.lock();
	}
}

void Blinky01BlockCode::unlockVM() {
	if (MeldProcessVM::isInDebuggingMode()) {
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
