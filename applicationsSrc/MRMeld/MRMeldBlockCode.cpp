
#include <iostream>
#include <sstream>
#include <memory>
#include <list>

#include "scheduler.h"
#include "network.h"
#include "lattice.h"

#include "MRMeldBlockCode.h"
#include "meldInterpretEvents.h"
#include "meldInterpretMessages.h"
#include "meldInterpretVM.h"

#include "trace.h"

using namespace std;
using namespace MultiRobots;
using namespace MeldInterpret;

MRMeldBlockCode::MRMeldBlockCode(MultiRobotsBlock *host): MultiRobotsBlockCode(host) {
	// OUTPUT << "MRMeldBlockCode constructor" << endl;
	vm = new MeldInterpretVM(hostBlock);
	hasWork = true; // mode fastest
	polling = false; // mode fastest
	currentLocalDate = 0; // mode fastest
	bb = (MultiRobotsBlock*)hostBlock;
}

MRMeldBlockCode::~MRMeldBlockCode() {
	delete vm;
	// OUTPUT << "MRMeldBlockCode destructor" << endl;
}

void MRMeldBlockCode::init() {
	stringstream info;

	if((vm != NULL)) {
		for (BuildingBlock *mrb : static_cast<BCLattice*>(MultiRobots::getWorld()->lattice)->connected) {
			if (mrb->blockId != hostBlock->blockId)
				vm->enqueue_edge(mrb->blockId, 1);
		}

		vm->enqueue_at((meld_int)bb->position.pt[0], (meld_int)bb->position.pt[1],
					   (meld_int)bb->position.pt[2], 1);
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

void MRMeldBlockCode::startup() {
	stringstream info;

	currentLocalDate = BaseSimulator::getScheduler()->now();
	info << "  Starting MRMeldBlockCode in block " << hostBlock->blockId;
	getScheduler()->trace(info.str(),hostBlock->blockId);
	init();
}

void MRMeldBlockCode::handleDeterministicMode(/*VMCommand &command*/){
	currentLocalDate = max(BaseSimulator::getScheduler()->now(), currentLocalDate);
	/*if(!hasWork && (command.getType() != VM_COMMAND_STOP)) {
	  hasWork = true;
	  #ifdef TEST_DETER
	  //cout << hostBlock->blockId << " has work again at " << BaseSimulator::getScheduler()->now() << endl;
	  #endif
	  }*/
}

void MRMeldBlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	assert(vm != NULL);
	info.str("");
#ifdef TEST_DETER
	cout << bb->blockId << " processLocalEvent: date: "<< BaseSimulator::getScheduler()->now() << " process event " << pev->getEventName() << "(" << pev->eventType << ")" << ", random number : " << pev->randomNumber << endl;
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
			BaseSimulator::getScheduler()->schedule(new ComputePredicateEvent(BaseSimulator::getScheduler()->now()+delay, bb));
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
	}
	break;
	case EVENT_REMOVE_NEIGHBOR:
	{
		unsigned int face = (std::static_pointer_cast<AddNeighborEvent>(pev))->face;
		vm->neighbors[face] = VACANT;
		vm->enqueue_face(vm->neighbors[face], face, -1);
		BaseSimulator::getScheduler()->schedule(
			new ComputePredicateEvent(BaseSimulator::getScheduler()->now(), bb));
	}
	break;
	case EVENT_TAP:
	{
		vm->enqueue_tap();
		BaseSimulator::getScheduler()->schedule(new ComputePredicateEvent(BaseSimulator::getScheduler()->now(), bb));
		info << "tapped, database dump printed to stderr...";
		vm->facts_dump();
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
			new NetworkInterfaceEnqueueOutgoingEvent(BaseSimulator::getScheduler()->now(), message, interface));
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
	case EVENT_TRANSLATION_START:
		vm->enqueue_at((meld_int)bb->position.pt[0], (meld_int)bb->position.pt[1],
					   (meld_int)bb->position.pt[2], -1);
		BaseSimulator::getScheduler()->schedule(
			new ComputePredicateEvent(BaseSimulator::getScheduler()->now(), bb));
		break;
	case EVENT_TRANSLATION_END:
		vm->enqueue_at((meld_int)bb->position.pt[0], (meld_int)bb->position.pt[1],
					   (meld_int)bb->position.pt[2], 1);
		BaseSimulator::getScheduler()->schedule(
			new ComputePredicateEvent(BaseSimulator::getScheduler()->now(), bb));
		break;
	case EVENT_SEND_MESSAGE_TO_BLOCK:
	{
        MessagePtr message = (std::static_pointer_cast<VMSendMessageEvent2>(pev))->message;
		BaseSimulator::BuildingBlock* target =
			(std::static_pointer_cast<VMSendMessageEvent2>(pev))->target;
        target->scheduleLocalEvent(
			EventPtr(new VMReceiveMessageEvent2(BaseSimulator::getScheduler()->now(), hostBlock, message)));
	}
	break;
	case EVENT_RECEIVE_MESSAGE_FROM_BLOCK: /*EVENT_NI_RECEIVE: */
	{
        MessagePtr mes = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
        switch(mes->type){
		case ADD_TUPLE_MSG_ID:
            getScheduler()->schedule(
				new AddTupleEvent(BaseSimulator::getScheduler()->now(), hostBlock,
								  std::static_pointer_cast<AddTupleMessage>(mes)->tuple, -1));
            break;
		case REMOVE_TUPLE_MSG_ID:
            getScheduler()->schedule(
				new RemoveTupleEvent(BaseSimulator::getScheduler()->now(), hostBlock,
									 std::static_pointer_cast<RemoveTupleMessage>(mes)->tuple, -1));
            break;
        }
	} break;
	default:
		ERRPUT << "*** ERROR *** : unknown local event";
		break;
	}
	if(info.str() != "") {
		getScheduler()->trace(info.str(),hostBlock->blockId);
	}
}

BlockCode* MRMeldBlockCode::buildNewBlockCode(BuildingBlock *host) {
	return(new MRMeldBlockCode((MultiRobotsBlock*)host));
}
