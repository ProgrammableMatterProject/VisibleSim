/*
 * multiCoresScheduler.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <stdlib.h>
#include <boost/bind.hpp>
#include "multiCoresSimulator.h"
#include "multiCoresScheduler.h"
#include "multiCoresWorld.h"

using namespace std;
using boost::asio::ip::tcp;

namespace MultiCores {

typedef void*(MultiCoresScheduler::*startPaused)(void*);

MultiCoresScheduler::MultiCoresScheduler() : ios2(), socket2(ios2){
	cout << "MultiCoresScheduler constructor" << endl;

	if (sizeof(uint64_t) != 8) {
		puts("ERROR : Scheduler requires 8bytes integer that are not available on this computer");
		exit(EXIT_FAILURE);
	}

	ios = new boost::asio::io_service();
	//socket2 = new tcp::socket(*ios);
	//socket = new udp::socket(*ios,udp::endpoint(udp::v4(), 7800));
	acceptor = new tcp::acceptor(ios2, tcp::endpoint(tcp::v4(), 7800));
	schedulerThread = new thread(bind(&MultiCoresScheduler::startPaused, this));

}

MultiCoresScheduler::~MultiCoresScheduler() {
	cout << "MultiCoresScheduler destructor" << endl;
}


void MultiCoresScheduler::createScheduler() {
	scheduler = new MultiCoresScheduler();
}

void MultiCoresScheduler::deleteScheduler() {
	delete((MultiCoresScheduler*)scheduler);
}

void *MultiCoresScheduler::startPaused(/*void *param*/) {
	//MultiCoresScheduler *scheduler = (MultiCoresScheduler*)param;
	int systemStartTime, systemStopTime;
	multimap<uint64_t, EventPtr>::iterator first;
	EventPtr pev;

	trace("\033[1;33mScheduler : created and waiting for VM connection : \033[0m");

	acceptor->accept(socket2);

	trace("\033[1;33mScheduler : VM connected and waiting for start order : \033[0m");

//	sem_schedulerStart.wait();
	waitForVMMessage();
	schedulerMode = SCHEDULER_MODE_FASTEST;
	cout << schedulerMode << endl;

	systemStartTime = (glutGet(GLUT_ELAPSED_TIME))*1000;
	cout << "\033[1;33m" << "Scheduler : start order received " << systemStartTime << "\033[0m" << endl;

	switch (schedulerMode) {
	case SCHEDULER_MODE_FASTEST:
	    while ( (!eventsMap.empty() || !undefinedBlocksSetIsEmpty()) && currentDate < maximumDate) {

	    	while (!undefinedBlocksSetIsEmpty()) {
	    		waitForVMMessage();
	    	}

	    	if (!eventsMap.empty()) {
	    		first=eventsMap.begin();
	    		pev = (*first).second;
	    		currentDate = pev->date;
// 		   		lock();
				pev->consume();
//    			unlock();
				eventsMap.erase(first);
				eventsMapSize--;
	    	}
	    }
		break;
	case SCHEDULER_MODE_REALTIME:
		break;
	default:
		cout << "ERROR : Scheduler mode not recognized !!" << endl;
	}

	systemStopTime = ((uint64_t)glutGet(GLUT_ELAPSED_TIME))*1000;

	cout << "\033[1;33m" << "Scheduler end : " << systemStopTime << "\033[0m" << endl;

	pev.reset();

	cout << "end time : " << currentDate << endl;
	cout << "real time elapsed : " << ((double)(systemStopTime-systemStartTime))/1000000 << endl;
//	cout << "Nombre d'événements restants en mémoire : " << Evenement::nbEvenements << endl;
//	cout << "Nombre de messages restants en mémoire : " << Message::nbMessages << endl;
	cout << "Maximum sized reached by the events list : " << largestEventsMapSize << endl;
	cout << "Size of the events list at the end : " << eventsMap.size() << endl;
	cout << "Number of events processed : " << Event::getNextId() << endl;
	cout << "Events(s) left in memory before destroying Scheduler : " << Event::getNbLivingEvents() << endl;
	cout << "Message(s) left in memory before destroying Scheduler : " << Message::getNbMessages() << endl;

	return(NULL);
}

void MultiCoresScheduler::waitForVMMessage() {

	//MultiCoresWorld *world = (MultiCoresWorld*)Simulator::world;
	stringstream info;
	VMMessage mes;

	float blockX = 0.0;
	float blockY = 0.0;
	float blockZ = 0.0;
	float blockRed = 1.0;
	float blockGreen = 1.0;
	float blockBlue = 1.0;

	size_t length;

	//size_t length = socket->receive_from(boost::asio::buffer((void*)&mes, sizeof(VMMessage)), VMEndpoint);
	boost::system::error_code error;
	try {
		length = boost::asio::read(socket2,boost::asio::buffer((void*)&mes, sizeof(VMMessage)) );
/*
		if (error == boost::asio::error::eof) {
		    trace("VM closed its connection");

		} else {
			if (error) {
				throw boost::system::system_error(error); // Some other error.
			}
		}
*/
	}
	catch (boost::system::system_error& e) {
		std::cerr << "End of file" << endl;
	}

	VM_TRACE_MESSAGE(mes);

	if (length == sizeof(VMMessage)) {
		switch (mes.messageType) {
		case VM_MESSAGE_TYPE_START_SIMULATION:
			info.str("");
			info << " The simulator needs " << mes.param1 << " blocks, creating them";
			Scheduler::trace(info.str());
			for (unsigned int i=0; i<mes.param1; i++) {
				getScheduler()->addUndefinedBlock(i);
				getWorld()->addBlock(i,
						MultiCoresSimulator::buildNewBlockCode,blockX, blockY, blockZ, blockRed, blockGreen, blockBlue);
			}
			break;
		case VM_MESSAGE_TYPE_CREATE_LINK:
			info.str("");
			info << " Linking block " << mes.param1 << " with " << mes.param2;
			Scheduler::trace(info.str());
			MultiCoresBlock *b0, *b1;
			b0 = getWorld()->getBlockById(mes.param1);
			b1 = getWorld()->getBlockById(mes.param2);
			b0->addP2PNetworkInterfaceAndConnectTo(b1);
			break;
		case VM_MESSAGE_TYPE_COMPUTATION_LOCK:
			if (!getScheduler()->isBlockUndefined(mes.param1)) {
				info.str("");
				info << "Block " << mes.param1 << " received a COMPUTATION_LOCK whereas it was not in undefined state !";
				getScheduler()->trace(info.str());
			}
			MultiCoresBlock *mcb0;
			mcb0 = getWorld()->getBlockById(mes.param1);
			getScheduler()->schedule(new VMStartComputationEvent(getScheduler()->now(), mcb0, mes.param2));

			//mcb0->setUndefinedState(false);
			getScheduler()->removeUndefinedBlock(mes.param1);
			break;
		case VM_MESSAGE_TYPE_END_SIMULATION:
			if (!getScheduler()->undefinedBlocksSetIsEmpty()) {
				info.str("");
				info << "A SIMULATION_END was received whereas there is at least one more block in undefined state !";
				getScheduler()->trace(info.str());
			}
//			endSimulation = true;
			break;
		case VM_MESSAGE_TYPE_SEND_MESSAGE:
			if (!getScheduler()->isBlockUndefined(mes.param1)) {
				info.str("");
				info << "Block " << mes.param1 << " received a SEND_MESSAGE whereas it was not in undefined state !";
				getScheduler()->trace(info.str());
			}
			{
				MultiCoresBlock *mcb0;
				mcb0 = getWorld()->getBlockById(mes.param1);
				getScheduler()->schedule(new VMStartTransmissionEvent(getScheduler()->now(), mcb0, mes.param2, mes.param3));
			}

			break;
		case VM_MESSAGE_TYPE_WAIT_FOR_MESSAGE:
			if (!getScheduler()->isBlockUndefined(mes.param1)) {
				info.str("");
				info << "Block " << mes.param1 << " received a WAIT_FOR_MESSAGE whereas it was not in undefined state !";
				getScheduler()->trace(info.str());
			}
			getScheduler()->removeUndefinedBlock(mes.param1);
			//getScheduler()->printUndefinedBlocksSet();
			break;
		default:
			cout << "*** ERROR *** : unsupported message received from VM" << endl;
			break;
		}
	}
}

void MultiCoresScheduler::sendMessageToVM(VMMessage message) {
	//socket->send_to(boost::asio::buffer((void*)&message,sizeof(message)),VMEndpoint);
	boost::asio::write(socket2, boost::asio::buffer((void*)&message,sizeof(message)));
}

} // MultiCores namespace
