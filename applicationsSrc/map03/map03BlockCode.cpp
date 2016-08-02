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
#include "translationEvents.h"
#include <memory>

const int COM_DELAY=1000000;
using namespace std;
using namespace RobotBlocks;


Map03BlockCode::Map03BlockCode(RobotBlocksBlock *host):RobotBlocksBlockCode(host) {
	cout << "Map03BlockCode constructor" << endl;
	scheduler = getScheduler();
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
	direction = 1;

	if(robotBlock->isMaster) {
        canStore = 0;


        SendRequestStorage(direction, robotBlock->blockId);


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

			message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
			P2PNetworkInterface * recvInterface = message->destinationInterface;

			switch(message->id){

				case REQUEST_STORAGE:{

					RequestStorage_ptr recvMessage = std::static_pointer_cast<RequestStorage>(message);

					if(canStore){

						answer = recvInterface;
						robotBlock->setColor(PINK);
						SendOkStorage(recvInterface, recvMessage->applicantId, robotBlock->blockId);

					}else{
						robotBlock->setColor(BLUE);
						SendNOkStorage(recvInterface);
					}

				}break;

				case OK_STORAGE:{

					OkStorage_ptr recvMessage = std::static_pointer_cast<OkStorage>(message);

					robotBlock->setColor(GREEN);
					
					SendDataToStore(recvInterface, recvMessage->canStoreId, d, d, d, d);
					d++;

				}break;

				case NOK_STORAGE:

					direction++;
					SendRequestStorage(direction, robotBlock->blockId);

				break;

				case DATA_TO_STORE:{

					DataToStore_ptr recvMessage = std::static_pointer_cast<DataToStore>(message);
					info.str("");

					robotBlock->setColor(ORANGE);

					if(CountNeighbor(recvInterface) == 0){

						info << "storing : " << CountNeighbor(recvInterface) << " ! " << recvMessage->mid;
						robotBlock->setColor(RED);
						SendDataStored(answer);
						canStore = 0;

					}else{

						info << "diffusion : " << CountNeighbor(recvInterface);
						SendRequestStorage(direction, robotBlock->blockId);

					}

					scheduler->trace(info.str(),robotBlock->blockId);

				}break;

				case DATA_STORED:
					if(robotBlock->isMaster){
						SendRequestStorage(direction, robotBlock->blockId);
					}else{
						SendDataStored(answer);
					}
				break;

        		default:
					cerr << "Block " << hostBlock->blockId << " received an unrecognized message from " << message->sourceInterface->hostBlock->blockId << endl;
				break;
			}

		break;

	}
}

void Map03BlockCode::SendRequestStorage(char &direction, int &applicantId_){
	P2PNetworkInterface *p2p;
	stringstream info;

	if(direction == 1){
		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Left);
		if(!p2p->connectedInterface){
			direction++;
		}
	}
	if(direction == 2){
		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Right);
		if(!p2p->connectedInterface){
			direction++;
		}
	}
	if(direction == 3){
		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Front);
		if(!p2p->connectedInterface){
			direction++;
		}
	}
	if(direction == 4){
		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Back);
		if(!p2p->connectedInterface){
			direction++;
		}
	}
	if(direction == 5){
		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Top);
		if(!p2p->connectedInterface){
			direction++;
		}
	}
	if(direction == 6){
		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Bottom);
		if(!p2p->connectedInterface){
			direction++;
		}
	}

	if(p2p->connectedInterface){

		int applicantId = applicantId_;
		RequestStorage *message = new RequestStorage(applicantId);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));

		info.str("");
		info << "send a request storage";

	}

	scheduler->trace(info.str(),robotBlock->blockId);
}

void Map03BlockCode::SendOkStorage(P2PNetworkInterface *send, int &applicantId_, int &canStoreId_){
	P2PNetworkInterface *p2p;
	stringstream info;

	p2p = send;

	if(p2p->connectedInterface){

		int applicantId = applicantId_;
		int canStoreId = canStoreId_;
		OkStorage *message = new OkStorage(applicantId, canStoreId);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));

		info.str("");
		info << "send ok storage answer";
	}

	scheduler->trace(info.str(),robotBlock->blockId);

}

void Map03BlockCode::SendNOkStorage(P2PNetworkInterface	*send){
	P2PNetworkInterface *p2p;
	stringstream info;

	p2p = send;

	if(p2p->connectedInterface){

		NOkStorage *message = new NOkStorage();
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));

		info.str("");
		info << "send NOK storage answer";

	}

	scheduler->trace(info.str(),robotBlock->blockId);

}

int Map03BlockCode::CountNeighbor(P2PNetworkInterface *except){
	P2PNetworkInterface *p2p;
	stringstream info;
	int nb = 0;

	for(int i = 0; i<6; i++) {
	p2p = robotBlock->getInterface((RobotBlocks::NeighborDirection::Direction)i);

    	if(p2p->connectedInterface && p2p !=except){
    		nb++;
    	}
    }

    return nb;
}

void Map03BlockCode::SendDataToStore(P2PNetworkInterface *send, int &canStoreId_, int &mid_, int &x_, int &y_, int &z_){
	P2PNetworkInterface *p2p;
	stringstream info;

	p2p = send;

	if(p2p->connectedInterface){

		int canStoreId = canStoreId_;
		int mid = mid_;
		int x = x_;
		int y = y_;
		int z = z_;

		DataToStore *message = new DataToStore(canStoreId, mid, x, y, z);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));

		info.str("");
		info << "send data to store";

	}

	scheduler->trace(info.str(),robotBlock->blockId);
}

void Map03BlockCode::SendDataStored(P2PNetworkInterface *send){
	P2PNetworkInterface *p2p;
	stringstream info;

	p2p = send;
	info.str("");

	if(p2p->connectedInterface){

		DataStored *message = new DataStored();
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
		info << "data stored message";

	}

	scheduler->trace(info.str(),robotBlock->blockId);
	
}

CTStatus::CTStatus(bool &ct_):Message(){
	id = CT_STATUS;

	ct = ct_;
}
CTStatus::~CTStatus(){}

DataStored::DataStored():Message(){
	id = DATA_STORED;
}
DataStored::~DataStored(){}

DataToStore::DataToStore(int &canStoreId_, int &mid_, int &x_, int &y_, int &z_):Message(){

	id = DATA_TO_STORE;
	canStoreId = canStoreId_;

	mid = mid_;
	x = x_;
	y = y_;
	z = z_;

}
DataToStore::~DataToStore(){}

NOkStorage::NOkStorage():Message(){

	id = NOK_STORAGE;

}

NOkStorage::~NOkStorage(){}

OkStorage::OkStorage(int &applicantId_, int &canStoreId_):Message(){

	id = OK_STORAGE;
	applicantId = applicantId_;
	canStoreId = canStoreId_;

}
OkStorage::~OkStorage(){}
RequestStorage::RequestStorage(int &applicantId_):Message(){

	id = REQUEST_STORAGE;
	applicantId = applicantId_;

}
RequestStorage::~RequestStorage(){}
