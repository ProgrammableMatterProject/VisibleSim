/*
 * flavio01BlockCode.cpp
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <sstream>
#include "flavio01BlockCode.h"
#include "scheduler.h"

using namespace std;
using namespace MultiCores;

Flavio01BlockCode::Flavio01BlockCode(MultiCoresBlock *host):MultiCoresBlockCode(host) {
	cout << "Flavio01BlockCode constructor" << endl;
	computing = false;
	waitingForVM = true;
}

Flavio01BlockCode::~Flavio01BlockCode() {
	cout << "Flavio01BlockCode destructor" << endl;
}

void Flavio01BlockCode::startup() {
	stringstream info;

	info << "  Starting Flavio01BlockCode in block " << hostBlock->blockId;
	getScheduler()->trace(info.str());
}


void Flavio01BlockCode::processLocalEvent(EventPtr pev) {
	MessagePtr message;
	stringstream info;

	//info << "FlavioBlockCode processing a local event : " << pev->getEventName();
	//Scheduler::trace(info.str());

	switch (pev->eventType) {
	case EVENT_NI_RECEIVE:
		{
			unsigned int sourceId;
			message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
			sourceId = message->sourceInterface->hostBlock->blockId;
			info.str("");
			info << "Block " << hostBlock->blockId << " received a message from " << sourceId;
			info << "   size : " << message->size();
			getScheduler()->trace(info.str());
			VMMessage response;
			response.messageType = VM_MESSAGE_TYPE_RECEIVE_MESSAGE;
			response.param1 = hostBlock->blockId;
			response.param2 = getScheduler()->now();
			response.param3 = message->size();
//			((MultiCoresBlock*)hostBlock)->setUndefinedState(true);
			getScheduler()->addUndefinedBlock(hostBlock->blockId);
			VM_TRACE_MESSAGE(response);
			getScheduler()->sendMessageToVM(response);
			waitingForVM = true;
		}
		break;
	case EVENT_VM_END_COMPUTATION:
		{
			computing = false;
			//boost::shared_ptr<EndComputationEvent>endComputationEvent = (boost::static_pointer_cast<EndComputationEvent>(pev));
			info.str("");
			info << "FlavioBlockCode " << hostBlock->blockId << " finished its computation";
			getScheduler()->trace(info.str());
			VMMessage response;
			response.messageType = VM_MESSAGE_TYPE_COMPUTATION_UNLOCK;
			response.param1 = hostBlock->blockId;
			response.param2 = getScheduler()->now();
//			((MultiCoresBlock*)hostBlock)->setUndefinedState(true);
			getScheduler()->addUndefinedBlock(hostBlock->blockId);
			VM_TRACE_MESSAGE(response);
			getScheduler()->sendMessageToVM(response);
			waitingForVM = true;
		}
		break;
	case EVENT_VM_START_COMPUTATION:
		uint64_t duration;
		if (computing) {
			info.str("");
			info << "*** ERROR *** block " << hostBlock->blockId << " got a COMPUTATION_LOCK from VM but it was already computing !";
			getScheduler()->trace(info.str());
		}
		if (!waitingForVM) {
			info.str("");
			info << "*** ERROR *** block " << hostBlock->blockId << " got a COMPUTATION_LOCK from VM but was not waiting for one";
			getScheduler()->trace(info.str());
		}
		computing = true;
		waitingForVM = false;
		duration = (boost::static_pointer_cast<VMStartComputationEvent>(pev))->duration;
		availabilityDate = getScheduler()->now()+duration;
		info.str("");
		info << "FlavioBlockCode " << hostBlock->blockId << " starting computation (will last for " << duration << ")" ;
		getScheduler()->trace(info.str());
		getScheduler()->schedule(new VMEndComputationEvent(getScheduler()->now()+duration, (MultiCoresBlock*)hostBlock));
		break;
	case EVENT_VM_START_TRANSMISSION:
		{
			unsigned int destId = (boost::static_pointer_cast<VMStartTransmissionEvent>(pev))->destBlockId;
			unsigned int messageSize = (boost::static_pointer_cast<VMStartTransmissionEvent>(pev))->messageSize;
			info.str("");
			info << "FlavioBlockCode " << hostBlock->blockId << " was asked to start transmitting to " << destId;
			getScheduler()->trace(info.str());

			P2PNetworkInterface *interface;
			interface = hostBlock->getP2PNetworkInterfaceByDestBlockId(destId);
			getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now(),new VMDataMessage(messageSize), interface));
		}
		break;
	default:
		break;
	}
}

MultiCores::MultiCoresBlockCode* Flavio01BlockCode::buildNewBlockCode(MultiCoresBlock *host) {
	return(new Flavio01BlockCode(host));
}




