/*
 * map03BlockCode.cpp
 *
 *  Created on: 30 avril 2015
 *      Author: Vincent
 */

#include <iostream>
#include <sstream>
#include "map03BlockCode.h"
#include "scheduler.h"
#include "robotBlocksEvents.h"
#include <boost/shared_ptr.hpp>

const int COM_DELAY=1000000;
using namespace std;
using namespace RobotBlocks;

Map03BlockCode::Map03BlockCode(RobotBlocksBlock *host):RobotBlocksBlockCode(host) {
	cout << "Map03BlockCode constructor" << endl;
	scheduler = RobotBlocks::getScheduler();
	robotBlock = (RobotBlocksBlock*)hostBlock;
	
	// initialize object deleted in destructor
}

Map03BlockCode::~Map03BlockCode() {
	cout << "Map03BlockCode destructor" << endl;
}

RobotBlocks::RobotBlocksBlockCode* Map03BlockCode::buildNewBlockCode(RobotBlocksBlock *host) {
	return(new Map03BlockCode(host));
}

void Map03BlockCode::startup() {
	stringstream info;

	if(robotBlock->isMaster) {

        RobotBlocksWorld *wrl = RobotBlocksWorld::getWorld();

        canStore = 0;

        SendRequestStorage(NULL,robotBlock->blockId);

	}else{

    	canStore = 1;

	}

	// Disable for see all message without scrolling
	// info << "start # " << robotBlock->blockId;
	// scheduler->trace(info.str(),robotBlock->blockId);
}

void Map03BlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	MessagePtr message;

	switch (pev->eventType) {

        case EVENT_MOTION_END:

	        robotBlock->setColor(LIGHTBLUE);
	        info.str("");
	        info << robotBlock->blockId << " rec.: EVENT_MOTION_END";
	        scheduler->trace(info.str(),hostBlock->blockId);
	        // prepare for next motion

        break;

		case EVENT_NI_RECEIVE:

			message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
			P2PNetworkInterface * recvInterface = message->destinationInterface;

			switch(message->id){

				case REQUEST_STORAGE:

					if(canStore){

						robotBlock->setColor(PINK);
						// Yes
					}else{
						robotBlock->setColor(BLUE);
						// No + ask neighbor
					}

				break;

        		default:
					cerr << "Block " << hostBlock->blockId << " received an unrecognized message from " << message->sourceInterface->hostBlock->blockId << endl;
				break;
			}

		break;

	}
}

void Map03BlockCode::SendRequestStorage(P2PNetworkInterface *send, int &applicantId_){
	P2PNetworkInterface *p2p;
	stringstream info;

	//Do a for, for all direction, but in specific order
	p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Front);



	if(p2p->connectedInterface){

		int applicantId = applicantId_;
		RequestStorage *message = new RequestStorage(applicantId);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));

		info.str("");
		info << "send a request storage";

	}

	scheduler->trace(info.str(),robotBlock->blockId);
}

RequestStorage::RequestStorage(int &applicantId_):Message(){

	id = REQUEST_STORAGE;
	applicantId = applicantId_;

}
RequestStorage::~RequestStorage(){}