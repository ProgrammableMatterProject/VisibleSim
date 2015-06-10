/*
 * MElection01BlockCode.cpp
 *
 *  Created on: may 22 2015
 *      Author: Vincent
 *
 * New system of election of the master
 * but still in development and really bugged for the moment
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

	nbOfWaitDiffusion = 0;
	bestId = robotBlock->blockId;
	AmIMaster = 0;
	lock = 0;
	DoIBroadcast = 1;

	StartIdDiffusion(bestId);

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

				case ID_DUFFUSION:{

					IdDiffusion_ptr recvMessage = boost::static_pointer_cast<IdDiffusion>(message);
					stringstream info;
					info.str("");

					if(recvMessage->idBlock < bestId){

						bestId = recvMessage->idBlock;
						SendIdAck(recvInterface);

						info << "New Best ID : " << bestId;
						scheduler->trace(info.str(),robotBlock->blockId);

					}else{

						SendIdNAck(recvInterface);

					}

				}break;

				case ID_ACK:{

					robotBlock->setColor(GREEN);
					nbOfWaitDiffusion--;

				}break;

				case ID_NACK:{

					robotBlock->setColor(RED);
					nbOfWaitDiffusion--;
					
					DoIBroadcast = 0;

					

				}break;

        		default:
					cerr << "Block " << hostBlock->blockId << " received an unrecognized message from " << message->sourceInterface->hostBlock->blockId << endl;
				break;
			}

		break;

	}
}

void MElection01BlockCode::StartIdDiffusion(int &idBlock_){

	stringstream info;
	P2PNetworkInterface *p2p;

	info.str("");

	for (int i = 0; i < 6; ++i)
	{

		p2p = robotBlock->getInterface((RobotBlocks::NeighborDirection::Direction)i);

		if(p2p->connectedInterface){

			IdDiffusion *message = new IdDiffusion(idBlock_);
			scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
			nbOfWaitDiffusion++;

		}

	}

	info << "broadcast a diffusion message NB : " << nbOfWaitDiffusion;
	scheduler->trace(info.str(),robotBlock->blockId,LIGHTBLUE);

}

void MElection01BlockCode::SendIdAck(P2PNetworkInterface *send){

	stringstream info;
	info.str("");

	if(send->connectedInterface){

		IdAck *message = new IdAck();
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, send));

	}

	info << "send ACK";
	scheduler->trace(info.str(),robotBlock->blockId,GREEN);

}

void MElection01BlockCode::SendIdNAck(P2PNetworkInterface *send){

	stringstream info;
	info.str("");

	if(send->connectedInterface){

		IdNAck *message = new IdNAck();
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, send));

	}

	info << "send NACK";
	scheduler->trace(info.str(),robotBlock->blockId,RED);

}

int MElection01BlockCode::CountNeighbor(P2PNetworkInterface *except){

	P2PNetworkInterface *p2p;
	int nb = 0;

	for (int i = 0; i < 6; ++i)
	{
		p2p = robotBlock->getInterface((RobotBlocks::NeighborDirection::Direction)i);
		if(p2p->connectedInterface && p2p != except){
			nb++;
		}
	}

	return nb;

}

IdDiffusion::IdDiffusion(int &idBlock_){

	id = ID_DUFFUSION;
	idBlock = idBlock_;

}IdDiffusion::~IdDiffusion(){}

IdAck::IdAck(){

	id = ID_ACK;

}IdAck::~IdAck(){}

IdNAck::IdNAck(){

	id = ID_NACK;

}IdNAck::~IdNAck(){}