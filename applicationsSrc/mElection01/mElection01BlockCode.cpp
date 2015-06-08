/*
 * MElection01BlockCode.cpp
 *
 *  Created on: may 22 2015
 *      Author: Vincent
 */

#include <iostream>
#include <sstream>
#include "mElection01BlockCode.h"
#include "scheduler.h"
#include "robotBlocksEvents.h"
#include <boost/shared_ptr.hpp>

const int COM_DELAY=700000;
using namespace std;
using namespace RobotBlocks;

MElection01BlockCode::MElection01BlockCode(RobotBlocksBlock *host):RobotBlocksBlockCode(host) {
	cout << "MElection01BlockCode constructor" << endl;
	scheduler = RobotBlocks::getScheduler();
	robotBlock = (RobotBlocksBlock*)hostBlock;
	
	// initialize object deleted in destructor
}

MElection01BlockCode::~MElection01BlockCode() {
	cout << "MElection01BlockCode destructor" << endl;
}

RobotBlocks::RobotBlocksBlockCode* MElection01BlockCode::buildNewBlockCode(RobotBlocksBlock *host) {
	return(new MElection01BlockCode(host));
}

void MElection01BlockCode::startup() {
	stringstream info;

	nbOfWaitStart = 0;
	bestId = robotBlock->blockId;
	AmIMaster = 0;
	lock = 0;

	SendStartMaster();

	if(robotBlock->isMaster) {

        RobotBlocksWorld *wrl = RobotBlocksWorld::getWorld();

	}else{

	}

}

void MElection01BlockCode::processLocalEvent(EventPtr pev) {
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

				case START_MASTER_ID:{

					StartMaster_ptr recvMessage = boost::static_pointer_cast<StartMaster>(message);

					if(recvMessage->blockId < bestId){

						bestId = recvMessage->blockId;

						info.str("");
						info << recvMessage->blockId;
						scheduler->trace(info.str(),robotBlock->blockId,LIGHTBLUE);
						SendStartAckMaster(recvInterface);

					}else{

						SendStartNAckMaster(recvInterface);

						if(countNeighbors(recvInterface) == 0){

							SendAskNeighbor(robotBlock->blockId);

						}

					}
					

				}break;

				case START_ACK_ID:

					robotBlock->setColor(GREEN);
					nbOfWaitStart--;
					if(nbOfWaitStart == 0){
						SendAskNeighbor(robotBlock->blockId);
					}
					
				break;

				case START_NACK_ID:

					robotBlock->setColor(RED);
					nbOfWaitStart--;

					if(nbOfWaitStart == 0){

						SendAskNeighbor(robotBlock->blockId);

					}

				break;

				case ASK_NEIGHBOR:{

					AskNeighbor_ptr recvMessage = boost::static_pointer_cast<AskNeighbor>(message);
					info.str("");

					if(recvMessage->applicantId < bestId){

						lock = 0;

					}

					if(recvMessage->applicantId <= bestId){

						bestId = recvMessage->applicantId;
						info << "diffusion : " << bestId << " lock " << lock;

						if(countNeighbors(recvInterface) > 0 && lock == 0){

							answer = recvInterface;
							SendAskNeighbor(bestId);

						}
						if(countNeighbors(recvInterface) == 0 && lock == 0){

							SendAnswerNeighbor(recvInterface,bestId);

						}

						robotBlock->setColor(LIGHTBLUE);

					}else{

						info << "DROP : " << recvMessage->applicantId;
						robotBlock->setColor(ORANGE);

					}

					if(bestId == recvMessage->applicantId){

						lock++;

						if(lock > 1){
							
							SendAnswerNeighbor(recvInterface,bestId);

						}

					}

					scheduler->trace(info.str(),robotBlock->blockId,GREEN);

				}break;

				case ANSWER_NEIGHBOR:{

					AnswerNeighbor_ptr recvMessage = boost::static_pointer_cast<AnswerNeighbor>(message);
					stringstream info;
					info.str("");

					if(bestId == recvMessage->applicantId){
						lock--;
						info << "same id : " << lock;

					}

					scheduler->trace(info.str(),robotBlock->blockId,RED);

					if(bestId == recvMessage->applicantId && lock == 0){

						SendAnswerNeighbor(answer, bestId);

					}


				}break;

        		default:
					cerr << "Block " << hostBlock->blockId << " received an unrecognized message from " << message->sourceInterface->hostBlock->blockId << endl;
				break;
			}

		break;

	}
}

void MElection01BlockCode::SendAskNeighbor(int &applicantId_){

	P2PNetworkInterface *p2p;
	stringstream info;

	for (int i = 0; i < 6; ++i){

		p2p = robotBlock->getInterface((RobotBlocks::NeighborDirection::Direction)i);
		if(p2p -> connectedInterface && p2p != answer){

			AskNeighbor *message = new AskNeighbor(applicantId_);
			scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
			AmIMaster++;

		}

	}
	
}

void MElection01BlockCode::SendStartMaster(){

	P2PNetworkInterface *p2p;
	stringstream info;
	for (int i = 0; i < 6; ++i){
		
		p2p = robotBlock->getInterface((RobotBlocks::NeighborDirection::Direction)i);

		if(p2p->connectedInterface){

			StartMaster *message = new StartMaster(robotBlock->blockId);
			scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));

			nbOfWaitStart++;
			info.str("");
			info << "send a start message : " << nbOfWaitStart;
			scheduler->trace(info.str(),robotBlock->blockId,BLUE);

		}
	}

}

void MElection01BlockCode::SendAnswerNeighbor(P2PNetworkInterface *send, int &applicantId_){

	stringstream info;
	info.str("");
	
	if(send->connectedInterface){
		info << "send answer";
		AnswerNeighbor *message = new AnswerNeighbor(applicantId_);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, send));

	}

	scheduler->trace(info.str(),robotBlock->blockId,PINK);

}

void MElection01BlockCode::SendStartAckMaster(P2PNetworkInterface *send){

	if(send->connectedInterface){

		StartAckMaster *message = new StartAckMaster();
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, send));

	}

}

void MElection01BlockCode::SendStartNAckMaster(P2PNetworkInterface *send){

	if(send->connectedInterface){

		StartNAckMaster *message = new StartNAckMaster();
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, send));

	}

}

int MElection01BlockCode::countNeighbors(P2PNetworkInterface *except){

	P2PNetworkInterface *p2p;

	int c = 0;
	for (int i = 0; i < 6; ++i)
	{
		p2p = robotBlock->getInterface((RobotBlocks::NeighborDirection::Direction)i);
		if(p2p -> connectedInterface && p2p != except){
			c++;
		}
	}

	return c;

}

AnswerNeighbor::AnswerNeighbor(int &applicantId_):Message(){

	id = ANSWER_NEIGHBOR;
	applicantId = applicantId_;

}AnswerNeighbor::~AnswerNeighbor(){}

AskNeighbor::AskNeighbor(int &applicantId_):Message(){

	id = ASK_NEIGHBOR;
	applicantId = applicantId_;

}AskNeighbor::~AskNeighbor(){}

StartMaster::StartMaster(int &blockId_):Message(){

	id = START_MASTER_ID;
	blockId = blockId_;

}StartMaster::~StartMaster(){}

StartAckMaster::StartAckMaster():Message(){

	id = START_ACK_ID;

}StartAckMaster::~StartAckMaster(){}

StartNAckMaster::StartNAckMaster():Message(){

	id = START_NACK_ID;

}StartNAckMaster::~StartNAckMaster(){}