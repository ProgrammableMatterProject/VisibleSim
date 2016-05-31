/*
 * Robot02BlockCode.cpp
 *
 *  Created on: 30 mars 2015
 *      Author: Vincent
 *
 * 	This code was our first code, and poorly organized (sorry about that)
 *	it demonstrate the diffusion of the map and how to stored in the master but without consideration of memory
 */

#include <iostream>
#include <sstream>
#include "map02BlockCode.h"
#include "scheduler.h"
#include "robotBlocksEvents.h"
#include <boost/shared_ptr.hpp>

const int COM_DELAY=1200000;

using namespace std;
using namespace RobotBlocks;

Robot02BlockCode::Robot02BlockCode(RobotBlocksBlock *host):RobotBlocksBlockCode(host) {
	cout << "Robot02BlockCode constructor" << endl;
	scheduler = RobotBlocks::getScheduler();
	robotBlock = (RobotBlocksBlock*)hostBlock;
	
	// initialize object deleted in destructor
}

Robot02BlockCode::~Robot02BlockCode() {
	cout << "Robot02BlockCode destructor" << endl;
}

RobotBlocks::RobotBlocksBlockCode* Robot02BlockCode::buildNewBlockCode(RobotBlocksBlock *host) {
	return(new Robot02BlockCode(host));
}

void Robot02BlockCode::startup() {
	stringstream info;
	info << "start #" << robotBlock->blockId;

	nbOfEnd = 0;
    nbOfFrame = 0; 
    lockEnd = false;
    //Initialization of variables 

	if(robotBlock->isMaster) {

        RobotBlocksWorld *wrl = RobotBlocksWorld::getWorld();

        info << "(Master Block at " << robotBlock->position[0] << "," << robotBlock->position[1] << "," << robotBlock->position[2] << ")";
        scheduler->trace(info.str(),robotBlock->blockId,YELLOW);

        int c = 0;
        recieved = true;
        SendGoMapMessage(NULL,robotBlock->blockId,c,c,c);

        //The master initialize the mapping

	}else{
		recieved = false;
    	scheduler->trace(info.str(),robotBlock->blockId,BLUE);
	}
}

void Robot02BlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	MessagePtr message;

	switch (pev->eventType){

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

				case GO_MAP_MSG_ID:

					if(!recieved){

						myColor = BLUE;
						robotBlock->setColor(myColor);
	        			recieved = true;
	        			//Now we received the message

						GoMapMessage_ptr recvMessage = boost::static_pointer_cast<GoMapMessage>(message);
						info.str("");

						tab[nbOfFrame][0] = hostBlock->blockId;
						tab[nbOfFrame][1] = recvMessage->x;
						tab[nbOfFrame][2] = recvMessage->y;
						tab[nbOfFrame][3] = recvMessage->z;
						//Save the data in a array

						pmid = recvInterface;
						nbOfFrame++;

		        		info << " recu : " << pmid->connectedInterface->hostBlock->blockId << " MAP : " << tab[0][1] << "," << tab[0][2] << "," << tab[0][3] << " FRAME " << nbOfFrame;
		        		scheduler->trace(info.str(),hostBlock->blockId);
	        			SendGoMapMessage(recvInterface, tab[0][0],tab[0][1],tab[0][2],tab[0][3]);
	        			//Send back information with same modification

	        		}else{
	        			info.str("");
	        			info << "SNED END MAP INSTANT TO " << recvInterface->connectedInterface->hostBlock->blockId;
		        		scheduler->trace(info.str(),hostBlock->blockId);
	        			SendEndMapMessage(recvInterface);
	        		}

				break;

				case GO_FRAME_MSG_ID:{

					if(!robotBlock->isMaster){

						GoFrameMessage_ptr recvMessage = boost::static_pointer_cast<GoFrameMessage>(message);

						tab[nbOfFrame][0] = recvMessage->mid;
						tab[nbOfFrame][1] = recvMessage->x;
						tab[nbOfFrame][2] = recvMessage->y;
						tab[nbOfFrame][3] = recvMessage->z;

						//Store in "buffer"

						nbOfFrame++;
						info.str("");
						info << "FRAME from " << recvMessage->mid << " : " << recvMessage->x << "," << recvMessage->y << "," << recvMessage->z << "!" << nbOfFrame;
						scheduler->trace(info.str(),hostBlock->blockId);

					}else{

						if(robotBlock->isMaster){

							GoFrameMessage_ptr recvMessage = boost::static_pointer_cast<GoFrameMessage>(message);

							int id = recvMessage->mid;

							tab[id][0] = recvMessage->x;
							tab[id][1] = recvMessage->y;
							tab[id][2] = recvMessage->z;

							// save the data

							info.str("");
							info << "RECU " << id << " : " << tab[id][0] << "," << tab[id][1] << "," << tab[id][2];
							scheduler->trace(info.str(),hostBlock->blockId);

						}

					}
				}
				break;

				case END_MAP_MSG_ID:

					nbOfEnd--;
					// We received the equivalent of an ACK
					info.str("");
					info << "END MAP MSG : " << nbOfEnd << " FROM " << message->sourceInterface->hostBlock->blockId;
					scheduler->trace(info.str(),hostBlock->blockId);

					if(nbOfEnd == 0){

						myColor = ORANGE;
						info.str("");
						scheduler->trace(info.str(),0,RED);
						robotBlock->setColor(myColor);

					}

				break;

        		default:
					cerr << "Block " << hostBlock->blockId << " received an unrecognized message from " << message->sourceInterface->hostBlock->blockId << endl;
				break;
			}

		break;

	}

	if(nbOfEnd == 0 && recieved	== true && !robotBlock->isMaster){

		if(nbOfFrame > 0){

			int nbOfFrameLock = nbOfFrame;

			for (int i = 0; i < nbOfFrameLock; ++i)
			{
				SendGoFrameMessage(pmid, tab[i][0],tab[i][1],tab[i][2],tab[i][3]);
				myColor = PINK;
				robotBlock->setColor(myColor);
			}

		}

		if(nbOfEnd == 0 && !lockEnd && nbOfFrame == 0){
			SendEndMapMessage(pmid);
			lockEnd = true;
		}
	}

}

void Robot02BlockCode::SendGoFrameMessage(P2PNetworkInterface *p2pExcept, int &mid_, int &x_, int &y_, int &z_){
	P2PNetworkInterface *p2p;
	stringstream info;
	p2p = p2pExcept;

	if(p2p->connectedInterface){

		int mid = mid_;
		int x = x_;
		int y = y_;
		int z = z_;

		GoFrameMessage *message = new GoFrameMessage(mid, x, y ,z);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
		nbOfFrame--;
	}
	info.str("");
	info << "DEBUG :" << p2p->connectedInterface->hostBlock->blockId << ": END : " << nbOfEnd << " FRAME : " << nbOfFrame;
	scheduler->trace(info.str(),hostBlock->blockId);
}

GoFrameMessage::GoFrameMessage(int &mid_, int &x_, int &y_, int &z_):Message(){

	id = GO_FRAME_MSG_ID;
	mid = mid_;
	x = x_;
	y = y_;
	z = z_;

}

void Robot02BlockCode::SendEndMapMessage(P2PNetworkInterface *send){

	P2PNetworkInterface *p2p;
	stringstream info;

	p2p = send;
	if(p2p -> connectedInterface){

		EndMapMessage *message = new EndMapMessage();
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
		info.str("");
		info << "SEND ENDMAP : " << send->connectedInterface->hostBlock->blockId;
		scheduler->trace(info.str(),hostBlock->blockId);

	}

}

void Robot02BlockCode::SendGoMapMessage(P2PNetworkInterface *p2pExcept, int &mid_, int &x_, int &y_, int &z_){
	P2PNetworkInterface *p2p;
	stringstream info;

	int mid = mid_;

	//This for was really reduced in map04

	for(int i = 0; i < 6; i++){

		switch(i){

			case 0:
	    		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Top);

	    		if(p2p->connectedInterface && p2p!=p2pExcept){
	    			int x = x_;
					int y = y_;
					int z = z_+1;
					GoMapMessage *message = new GoMapMessage(mid, x, y ,z);
					scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
					nbOfEnd++;
	    		}
			break;

			case 1:
	    		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Bottom);

	    		if(p2p->connectedInterface && p2p!=p2pExcept){
	    			int x = x_;
					int y = y_;
					int z = z_-1;
					GoMapMessage *message = new GoMapMessage(mid, x, y ,z);
					scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
					nbOfEnd++;
	    		}
			break;

			case 2:
	    		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Left);

	    		if(p2p->connectedInterface && p2p!=p2pExcept){
	    			int x = x_-1;
					int y = y_;
					int z = z_;
					GoMapMessage *message = new GoMapMessage(mid, x, y ,z);
					scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
					nbOfEnd++;
	    		}
			break;

			case 3:
	    		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Right);

	    		if(p2p->connectedInterface && p2p!=p2pExcept){
	    			int x = x_+1;
					int y = y_;
					int z = z_;
					GoMapMessage *message = new GoMapMessage(mid, x, y ,z);
					scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
					nbOfEnd++;
	    		}
			break;

			case 4:
	    		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Front);

	    		if(p2p->connectedInterface && p2p!=p2pExcept){
	    			int x = x_;
					int y = y_+1;
					int z = z_;
					GoMapMessage *message = new GoMapMessage(mid, x, y ,z);
					scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
					nbOfEnd++;
	    		}
			break;

			case 5:
	    		p2p = robotBlock->getInterface(RobotBlocks::NeighborDirection::Back);

	    		if(p2p->connectedInterface && p2p!=p2pExcept){
	    			int x = x_;
					int y = y_-1;
					int z = z_;
					GoMapMessage *message = new GoMapMessage(mid, x, y ,z);
					scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
					nbOfEnd++;
	    		}
			break;

		}
	}
}

GoMapMessage::GoMapMessage(int &mid_, int &x_, int &y_, int &z_):Message(){

	id = GO_MAP_MSG_ID;
	mid = mid_;
	x = x_;
	y = y_;
	z = z_;

}

EndMapMessage::EndMapMessage(){
	id = END_MAP_MSG_ID;
}

EndMapMessage::~EndMapMessage(){
}
GoMapMessage::~GoMapMessage(){
}
GoFrameMessage::~GoFrameMessage(){
}
