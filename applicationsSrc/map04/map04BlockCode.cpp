/*
 * Map04BlockCode.cpp
 *
 *  Created on: may 22 2015
 *      Author: Vincent
 *
 * New version of the map02 prgram, with consideration of memory
 * not yet finished
 */

#include <iostream>
#include <sstream>
#include "map04BlockCode.h"
#include "scheduler.h"
#include "robotBlocksEvents.h"
#include <boost/shared_ptr.hpp>

const int COM_DELAY=700000;
using namespace std;
using namespace RobotBlocks;

Map04BlockCode::Map04BlockCode(RobotBlocksBlock *host):RobotBlocksBlockCode(host) {
	cout << "Map04BlockCode constructor" << endl;
	scheduler = RobotBlocks::getScheduler();
	robotBlock = (RobotBlocksBlock*)hostBlock;
	
	// initialize object deleted in destructor
	
}

Map04BlockCode::~Map04BlockCode() {
	cout << "Map04BlockCode destructor" << endl;
}

RobotBlocks::RobotBlocksBlockCode* Map04BlockCode::buildNewBlockCode(RobotBlocksBlock *host) {
	return(new Map04BlockCode(host));
}

void Map04BlockCode::startup() {
	stringstream info;
	NbOfWaitEnd = 0;
	NbOfWaitFrame = 0;
	buffer_c = 0;

	if(robotBlock->isMaster) {

        RobotBlocksWorld *wrl = RobotBlocksWorld::getWorld();
        received = 1;

        x = 0;
        y = 0;
        z = 0;
        //got a variable x,y,z in each block who depend of the master

        toMaster = NULL;
        reachMaster = 1;
        // I am the master so i can reach the master 

        SendGoMap(NULL,x,y,z);

	}else{

		received = 0;
        reachMaster = 0;
        // I am not in the map

	}
}

void Map04BlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	MessagePtr message;

	switch (pev->eventType) {

        case EVENT_MOTION_END:

	        robotBlock->setColor(LIGHTBLUE);
	        info.str("");
	        info << robotBlock->blockId << " rec.: EVENT_MOTION_END";
	        scheduler->trace(info.str(),hostBlock->blockId);

        break;

		case EVENT_NI_RECEIVE:

			message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
			P2PNetworkInterface * recvInterface = message->destinationInterface;

			switch(message->id){

				case GO_MAP_ID:{

					if(!received){

						GoMap_ptr recvMessage = boost::static_pointer_cast<GoMap>(message);
						robotBlock->setColor(BLUE);
						toMaster = recvInterface;
						NbOfWaitFrame++;
						reachMaster = 1;
						received = 1;

						x = recvMessage->x;
						y = recvMessage->y;
						z = recvMessage->z;
						//save the coordinate

						info.str("");
						info << "go map received" << recvMessage->blockId << ":" << x << "," << y << "," << z;
						scheduler->trace(info.str(),robotBlock->blockId,GREEN);

						SendGoMap(toMaster, x, y, z);

					}else{

						SendEndMap(recvInterface);

					}

				}break;

				case FRAME_ID:{

					if(!robotBlock->isMaster){
					
						Frame_ptr recvMessage = boost::static_pointer_cast<Frame>(message);
						robotBlock->setColor(LIGHTBLUE);
						info.str("");
						info << "received frame ";
						scheduler->trace(info.str(),robotBlock->blockId,GREEN);

					}else{

						Frame_ptr recvMessage = boost::static_pointer_cast<Frame>(message);

						if(buffer_c < SIZE){

							// the SIZE of the buffer is definied in the .h
							buffer[buffer_c][0] = recvMessage->blockId;
							buffer[buffer_c][1] = recvMessage->x;
							buffer[buffer_c][2] = recvMessage->y;
							buffer[buffer_c][3] = recvMessage->z;
							buffer_c++;

							SendBufferAck(recvInterface, recvMessage->blockId);

						}else{

							SendBufferNAck(recvInterface, recvMessage->blockId);

						}
					}

				}break;

				case END_MAP_ID:

					info.str("");
					NbOfWaitEnd--;
					info << "status NbOfWaitEnd : " << NbOfWaitEnd;
					scheduler->trace(info.str(),robotBlock->blockId,GREEN);

					if(((!robotBlock->isMaster) && (NbOfWaitEnd == 0 && NbOfWaitFrame > 0))){

						SendFrame(toMaster);

					}

					if(robotBlock->isMaster){

						if(NbOfWaitEnd == 0){

							robotBlock->setColor(YELLOW);

						}

					}

				break;

        		default:
					cerr << "Block " << hostBlock->blockId << " received an unrecognized message from " << message->sourceInterface->hostBlock->blockId << endl;
				break;
			}

		break;

	}
}

void Map04BlockCode::SendGoMap(P2PNetworkInterface *except, int &x_, int &y_, int &z_){

	P2PNetworkInterface *p2p;
	stringstream info;

	for (int i = 0; i < 6; ++i){

		info.str("");

		if(i == 0){p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Top);}
		else if(i == 1){p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Bottom);}
		else if(i == 2){p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Right);}
		else if(i == 3){p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Left);}
		else if(i == 4){p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Front);}
		else if(i == 5){p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Back);}
		else{p2p = NULL;}

		if(p2p->connectedInterface && p2p != except){

			NbOfWaitEnd++;

			int x = x_;
			int y = y_;
			int z = z_;

			if(i == 0){z = z+1;}
			else if(i == 1){z = z-1;}
			else if(i == 2){x = x+1;}
			else if(i == 3){x = x-1;}
			else if(i == 4){y = y+1;}
			else if(i == 5){y = y-1;}

			//same modificaiton than map02 but with less code

			info << "send go map message";
			GoMap *message = new GoMap(robotBlock->blockId, x, y, z);
			scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
			scheduler->trace(info.str(),robotBlock->blockId,BLUE);

		}

	}

	if(NbOfWaitEnd == 0){

		robotBlock->setColor(GREEN);
		SendFrame(toMaster);
	
	}

}

void Map04BlockCode::SendBufferAck(P2PNetworkInterface *send, int &buffer_s_){

	stringstream info;
	info.str("");

	if(send->connectedInterface){

		info << "send buffer ack";
		BufferAck *message = new BufferAck(buffer_s_);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, send));

	}

	scheduler->trace(info.str(),robotBlock->blockId);

}

void Map04BlockCode::SendBufferNAck(P2PNetworkInterface *send, int &buffer_s_){

	stringstream info;
	info.str("");

	if(send->connectedInterface){

		info << "send buffer Nack";
		BufferNAck *message = new BufferNAck(buffer_s_);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, send));

	}

	scheduler->trace(info.str(),robotBlock->blockId);

}

void Map04BlockCode::SendFrame(P2PNetworkInterface *send){
	
	stringstream info;
	info.str("");

	if(send->connectedInterface){

		info << "send frame";
		Frame *message = new Frame(robotBlock->blockId, x, y, z);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, send));

	}

	SendEndMap(send);
	scheduler->trace(info.str(),robotBlock->blockId,ORANGE);

}

void Map04BlockCode::SendEndMap(P2PNetworkInterface *send){

	stringstream info;
	info.str("");

	if(send -> connectedInterface){

		info << "send end map";
		EndMap *message = new EndMap();
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, send));
		
	}

	scheduler->trace(info.str(),robotBlock->blockId,RED);

}

BufferAck::BufferAck(int &buffer_s_):Message(){

	id = BUFFER_ACK_ID;
	buffer_s = buffer_s_;

}BufferAck::~BufferAck(){}

BufferNAck::BufferNAck(int &buffer_s_):Message(){

	id = BUFFER_ACK_ID;
	buffer_s = buffer_s_;

}BufferNAck::~BufferNAck(){}

Frame::Frame(int &blockId_, int &x_, int &y_, int &z_):Message(){

	id = FRAME_ID;

	blockId = blockId_;
	x = x_; y = y_; z = z_;

}Frame::~Frame(){}

GoMap::GoMap(int &blockId_, int &x_, int &y_, int &z_):Message(){

	id = GO_MAP_ID;

	blockId = blockId_;
	x = x_; y = y_; z = z_;

}GoMap::~GoMap(){}

EndMap::EndMap():Message(){

	id = END_MAP_ID;

}EndMap::~EndMap(){}