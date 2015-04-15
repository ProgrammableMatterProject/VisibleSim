#include <iostream>
#include <sstream>
#include <boost/asio.hpp>
#include "scheduler.h"
#include "network.h"

#include "BlinkyMeldBlockCode.h"
#include "blinkyBlocksBlock.h"

#include "meldInterpretEvents.h"
#include "meldInterpretVM.h"
#include "meldInterpretVMCore.h"

#include "trace.h"

using namespace std;
using namespace BlinkyBlocks;

BlinkyMeldBlockCode::BlinkyMeldBlockCode(BlinkyBlocksBlock *host): BlinkyBlocksBlockCode(host) {
	OUTPUT << "BlinkyMeldBlockCode constructor" << endl;
	hasWork = true; // mode fastest
	polling = false; // mode fastest
	currentLocalDate = 0; // mode fastest
}

BlinkyMeldBlockCode::~BlinkyMeldBlockCode() {
	OUTPUT << "BlinkyMeldBlockCode destructor" << endl;
}

void BlinkyMeldBlockCode::init() {
	bb = (BlinkyBlocksBlock*) hostBlock;
	stringstream info;
	commandType c[5];

	vm = bb->vm;
	if((vm != NULL)) {
		if((BlinkyBlocks::getScheduler()->getMode() == SCHEDULER_MODE_FASTEST) && !vm->deterministicSet) {
			/*vm->deterministicSet = true;
			SetDeterministicModeVMCommand determinismCommand(c, bb->blockId);
			vm->sendCommand(determinismCommand);*/
			info << "deterministic mode set";
			BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
			OUTPUT << "deterministic mode enable on the VM " << hostBlock->blockId << endl;
		}
	}
}

void BlinkyMeldBlockCode::startup() {
	stringstream info;

	currentLocalDate = BaseSimulator::getScheduler()->now();
	info << "  Starting BlinkyMeldBlockCode in block " << hostBlock->blockId;
	BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
	init();
}

void BlinkyMeldBlockCode::handleDeterministicMode(/*VMCommand &command*/){
	currentLocalDate = max(BaseSimulator::getScheduler()->now(), currentLocalDate);
	/*if(!hasWork && (command.getType() != VM_COMMAND_STOP)) {
		hasWork = true;
#ifdef TEST_DETER
		//cout << hostBlock->blockId << " has work again at " << BaseSimulator::getScheduler()->now() << endl;
#endif
	}*/
}

void BlinkyMeldBlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	assert(vm != NULL);
	info.str("");

	OUTPUT << bb->blockId << " processLocalEvent: date: "<< BaseSimulator::getScheduler()->now() << " process event " << pev->getEventName() << "(" << pev->eventType << ")" << ", random number : " << pev->randomNumber << endl;

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
                              BaseSimulator::getScheduler()->schedule(new ComputePredicateEvent(BaseSimulator::getScheduler()->now(), bb));
                        }
		      }
			break;
		case EVENT_STOP:
			{
				//getDebugger()->sendTerminateMsg(bb->blockId);
				delete vm;
				bb->vm = NULL;
                        info << "VM stopped";
			}
			break;
		case EVENT_ADD_NEIGHBOR:
			{
			      //Should not be used by itself, presence of another block is tested by P2PNetworkInterface
			      unsigned int face = boost::static_pointer_cast<AddNeighborEvent>(pev))->face;
                        vm->neighbors[face] = (boost::static_pointer_cast<AddNeighborEvent>(pev))->target;
                        vm->enqueue_face(vm->neighbors[face], face, 1);
                        info << "Add neighbor "<< (boost::static_pointer_cast<AddNeighborEvent>(pev))->target << " at face " << BlinkyBlocks::NeighborDirection::getString(BlinkyBlocks::NeighborDirection::getOpposite((boost::static_pointer_cast<VMAddNeighborEvent>(pev))->face));
			}
			break;
		case EVENT_REMOVE_NEIGHBOR:
			{
			      unsigned int face = boost::static_pointer_cast<AddNeighborEvent>(pev))->face;
                        vm->enqueue_face(vm->neighbors[face], face, -1);
                        vm->neighbors[face] = NULL;
                        info << "Remove neighbor at face " << BlinkyBlocks::NeighborDirection::getString(BlinkyBlocks::NeighborDirection::getOpposite(face));
			}
			break;
		case EVENT_TAP:
			{
			      vm->enqueue_tap();
                        info << "tapped";
			}
			break;
		case EVENT_SET_COLOR:
			{
			      //Called by the VM, no need to enqueue things
                        Vecteur color = (boost::static_pointer_cast<VMSetColorEvent>(pev))->color;
                        bb->setColor(color);
#ifdef TEST_DETER
                        cout << bb->blockId << " SET_COLOR_EVENT" << endl;
#endif
                        info << "set color "<< color << endl;
			}
			break;

            /*The interface being connected is tested in function tuple_send of the MeldInterpVM*/
		case EVENT_SEND_MESSAGE:
			{
                        MessagePtr message = (boost::static_pointer_cast<VMSendMessageEvent>(pev))->message;
                        P2PNetworkInterface *interface = (boost::static_pointer_cast<VMSendMessageEvent>(pev))->sourceInterface;
                        BlinkyBlocks::getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(BaseSimulator::getScheduler()->now(), message, interface));
                        info << "sends a message at face " << NeighborDirection::getString(bb->getDirection(interface))  << " to " << interface->connectedInterface->hostBlock->blockId;
			}
			break;
		case EVENT_RECEIVE_MESSAGE: /*EVENT_NI_RECEIVE: */
			{
			      MessagePtr mes = boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message.get();
                        switch(message->type){
                        case ADD_TUPLE_MSG_ID:
                              BlinkyBlocks::getScheduler()->schedule(new AddTupleEvent(BaseSimulator::getScheduler()->now(), (AddTupleMessage*)mes->t, (AddTupleMessage*)mes->destinationInterface));
                              break;
                        case REMOVE_TUPLE_MSG_ID:
                              BlinkyBlocks::getScheduler()->schedule(new RemoveTupleEvent(BaseSimulator::getScheduler()->now(), (RemoveTupleMessage*)mes->t, (RemoveTupleMessage*)mes->destinationInterface));
                              break;
                        }
#ifdef TEST_DETER
                        cout << "message received from " << command->sourceInterface->hostBlock->blockId << endl;
#endif
                        info << "message received at face " << NeighborDirection::getString(bb->getDirection(mes->sourceInterface->connectedInterface)) << " from " << mes->sourceInterface->hostBlock->blockId;
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
                        info << "Polling time period ended" << endl;
			}
			break;
		default:
			ERRPUT << "*** ERROR *** : unknown local event" << endl;
			break;
		}
		BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
}

BlinkyBlocks::BlinkyBlocksBlockCode* BlinkyMeldBlockCode::buildNewBlockCode(BlinkyBlocksBlock *host) {
	return(new BlinkyMeldBlockCode(host));
}
