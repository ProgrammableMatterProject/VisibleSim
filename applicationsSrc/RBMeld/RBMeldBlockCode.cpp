#include <iostream>
#include <sstream>

#include "scheduler.h"
#include "network.h"

#include "RBMeldBlockCode.h"
#include "meldInterpretEvents.h"
#include "meldInterpretMessages.h"
#include "meldInterpretVM.h"
#include "lattice.h"

#include "trace.h"

using namespace std;
using namespace RobotBlocks;
using namespace MeldInterpret;

RBMeldBlockCode::RBMeldBlockCode(RobotBlocksBlock *host): RobotBlocksBlockCode(host) {
	OUTPUT << "RBMeldBlockCode constructor" << endl;
	vm = new MeldInterpretVM(hostBlock);
	hasWork = true; // mode fastest
	polling = false; // mode fastest
	currentLocalDate = 0; // mode fastest
	bb = (RobotBlocksBlock*) hostBlock;
}

RBMeldBlockCode::~RBMeldBlockCode() {
	delete vm;
	OUTPUT << "RBMeldBlockCode destructor" << endl;
}

void RBMeldBlockCode::init() {
	stringstream info;
	
	if((vm != NULL)) {
		bb = (RobotBlocksBlock*) hostBlock;
		for (int i = 0; i < bb->getNbInterfaces(); i++) {
			vm->neighbors[i] = vm->get_neighbor_ID(i);
			OUTPUT << "Adding neighbor " << vm->neighbors[i] << " on face " << i << endl;
			vm->enqueue_face(vm->neighbors[i], i, 1);
		}
		
		BaseSimulator::getScheduler()->schedule(
			new ComputePredicateEvent(BaseSimulator::getScheduler()->now(), bb));

		if((getScheduler()->getMode() == SCHEDULER_MODE_FASTEST) && !vm->deterministicSet) {
			vm->deterministicSet = true;
			/*SetDeterministicModeVMCommand determinismCommand(c, bb->blockId);
			  vm->sendCommand(determinismCommand);*/
			info << "deterministic mode set";
			getScheduler()->trace(info.str(),hostBlock->blockId);
			OUTPUT << "deterministic mode enable on the VM " << hostBlock->blockId << endl;
		}
	}
}

void RBMeldBlockCode::startup() {
	stringstream info;

	currentLocalDate = BaseSimulator::getScheduler()->now();
	info << "  Starting RBMeldBlockCode in block " << hostBlock->blockId;
	getScheduler()->trace(info.str(),hostBlock->blockId);
	init();
}

void RBMeldBlockCode::handleDeterministicMode(/*VMCommand &command*/){
	currentLocalDate = max(BaseSimulator::getScheduler()->now(), currentLocalDate);
	/*if(!hasWork && (command.getType() != VM_COMMAND_STOP)) {
	  hasWork = true;
	  #ifdef TEST_DETER
	  //cout << hostBlock->blockId << " has work again at " << BaseSimulator::getScheduler()->now() << endl;
	  #endif
	  }*/
}

void RBMeldBlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	assert(vm != NULL);
	info.str("");
	OUTPUT << bb->blockId << " processLocalEvent: date: "<< BaseSimulator::getScheduler()->now()
		   << " process event " << pev->getEventName() << "(" << pev->eventType << ")"
		   << ", random number : " << pev->randomNumber << endl;

#ifdef TEST_DETER
	cout << bb->blockId << " processLocalEvent: date: "<< BaseSimulator::getScheduler()->now()
		 << " process event " << pev->getEventName() << "(" << pev->eventType << ")"
		 << ", random number : " << pev->randomNumber << endl;
#endif
	switch (pev->eventType) {
	case EVENT_COMPUTE_PREDICATE:
	{
		//Call the VM function to process one rule
		vm->processOneRule();
		//Add another compute event on condition
		//if...
		if(vm->isWaiting()){
			// random delay before recomputing a predicate
			// between 0.1 ms and 1ms
			int delay = (bb->getNextRandomNumber() % (1000 - 100 +1 )) + 100;
			BaseSimulator::getScheduler()->schedule(
				new ComputePredicateEvent(BaseSimulator::getScheduler()->now()+delay, bb));
		}
		//else info << " Block is no longer waiting";
		//info << "Compute predicate event";
	}
	break;
	case EVENT_STOP:
	{
		//getDebugger()->sendTerminateMsg(bb->blockId);
		delete vm;
		info << "VM stopped";
	}
	break;
	case EVENT_ADD_NEIGHBOR:
	{
		//Should not be used by itself, presence of another block is tested by P2PNetworkInterface
		unsigned int face = (std::static_pointer_cast<AddNeighborEvent>(pev))->face;
		vm->neighbors[face] = (std::static_pointer_cast<AddNeighborEvent>(pev))->target;
		vm->enqueue_face(vm->neighbors[face], face, 1);
		BaseSimulator::getScheduler()->schedule(
			new ComputePredicateEvent(BaseSimulator::getScheduler()->now(), bb));
		info << "Add neighbor "<< (std::static_pointer_cast<AddNeighborEvent>(pev))->target
			 << " at face "
			 << lattice->getDirectionString(lattice->getOppositeDirection(
										 (std::static_pointer_cast<AddNeighborEvent>(pev))->face));
	}
	break;
	case EVENT_REMOVE_NEIGHBOR:
	{
		unsigned int face = (std::static_pointer_cast<AddNeighborEvent>(pev))->face;
		vm->neighbors[face] = VACANT;
		vm->enqueue_face(vm->neighbors[face], face, -1);
		BaseSimulator::getScheduler()->schedule(
			new ComputePredicateEvent(BaseSimulator::getScheduler()->now(), bb));
		info << "Remove neighbor at face " << lattice->getDirectionString(lattice->getOppositeDirection(face));
	}
	break;
	case EVENT_TAP:
	{
		vm->enqueue_tap();
		BaseSimulator::getScheduler()->schedule(
			new ComputePredicateEvent(BaseSimulator::getScheduler()->now(), bb));
		info << "tapped";
	}
	break;
	case EVENT_SET_COLOR:
	{
		bb->getTime();
		//Called by the VM, no need to enqueue things
		Color color = (std::static_pointer_cast<SetColorEvent>(pev))->color;
		bb->setColor(color);
#ifdef TEST_DETER
		cout << bb->blockId << " SET_COLOR_EVENT" << endl;
#endif
		info << "set color "<< color;
	}
	break;

	/*The interface being connected is tested in function tuple_send of the MeldInterpVM*/
	case EVENT_SEND_MESSAGE:
	{
		MessagePtr message = (std::static_pointer_cast<VMSendMessageEvent>(pev))->message;
		P2PNetworkInterface *interface = (std::static_pointer_cast<VMSendMessageEvent>(pev))->sourceInterface;
		getScheduler()->schedule(
			new NetworkInterfaceEnqueueOutgoingEvent(BaseSimulator::getScheduler()->now(),
													 message, interface));
		//info << "sends a message at face " << NeighborDirection::getString(bb->getDirection(interface))  << " to " << interface->connectedInterface->hostBlock->blockId;
	}
	break;
	case EVENT_RECEIVE_MESSAGE: /*EVENT_NI_RECEIVE: */
	{
		MessagePtr mes = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
		switch(mes->type){
		case ADD_TUPLE_MSG_ID:
			getScheduler()->schedule(
				new AddTupleEvent(BaseSimulator::getScheduler()->now(), hostBlock,
								  std::static_pointer_cast<AddTupleMessage>(mes)->tuple,
								  bb->getDirection(mes->sourceInterface->connectedInterface)));
			break;
		case REMOVE_TUPLE_MSG_ID:
			getScheduler()->schedule(
				new RemoveTupleEvent(BaseSimulator::getScheduler()->now(), hostBlock,
									 std::static_pointer_cast<RemoveTupleMessage>(mes)->tuple,
									 bb->getDirection(mes->sourceInterface->connectedInterface)));
			break;
		}
#ifdef TEST_DETER
		cout << "message received from " << command->sourceInterface->hostBlock->blockId << endl;
#endif
		//info << "message received at face " << NeighborDirection::getString(bb->getDirection(mes->sourceInterface->connectedInterface)) << " from " << mes->sourceInterface->hostBlock->blockId;
	}
	break;
	case EVENT_ACCEL:
	{
		/*Not written yet, have to check how the vm handle accel (tuple, etc)*/
		info << "accel";
	}
	break;
	case EVENT_SHAKE:
	{
		/*Not written yet, same as accel*/

		info << "shake";
	}
	break;
	case EVENT_SET_DETERMINISTIC:
	{
		/*Not sure how to handle that with MeldInterp*/
		OUTPUT << "VM set in deterministic mode " << hostBlock->blockId << endl;
		info << "VM set in deterministic mode";
	}
	break;
	case EVENT_END_POLL:
	{
		polling = false;
		/*Not written yet
		  Need to check what this is for*/
		info << "Polling time period ended";
	}
	break;
	case EVENT_ADD_TUPLE:
		this->vm->receive_tuple(1, std::static_pointer_cast<AddTupleEvent>(pev)->tuple,
								std::static_pointer_cast<AddTupleEvent>(pev)->face);
		BaseSimulator::getScheduler()->schedule(
			new ComputePredicateEvent(BaseSimulator::getScheduler()->now(), bb));
		//info << "Adding tuple";
		break;
	case EVENT_REMOVE_TUPLE:
		this->vm->receive_tuple(-1, std::static_pointer_cast<RemoveTupleEvent>(pev)->tuple,
								std::static_pointer_cast<RemoveTupleEvent>(pev)->face);
		BaseSimulator::getScheduler()->schedule(
			new ComputePredicateEvent(BaseSimulator::getScheduler()->now(), bb));
		//info << "Removing tuple";
		break;
	default:
		ERRPUT << "*** ERROR *** : unknown local event";
		break;
	}
	if(info.str() != "") {
		getScheduler()->trace(info.str(),hostBlock->blockId);
	}
}

BlockCode* RBMeldBlockCode::buildNewBlockCode(BuildingBlock *host) {
	return(new RBMeldBlockCode((RobotBlocksBlock*)host));
}
