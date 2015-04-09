/*
 * BbCycleBlockCode.cpp
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <sstream>
#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp> 
#include "scheduler.h"
#include "network.h"
#include "bbCycleBlockCode.h"

#include "trace.h"

using namespace std;
using namespace BlinkyBlocks;

#define COLOR_CHANGE_PERIOD_USEC (2*1000*1000)
#define SIMULATION_DURATION_USEC (10*60*1000*1000)

BbCycleBlockCode::BbCycleBlockCode(BlinkyBlocksBlock *host): BlinkyBlocksBlockCode(host) {
	OUTPUT << "BbCycleBlockCode constructor" << endl;
}

BbCycleBlockCode::~BbCycleBlockCode() {
	OUTPUT << "BbCycleBlockCode destructor" << endl;
}

void BbCycleBlockCode::init() {
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	stringstream info;
	
	uint64_t time = 0;
	while (time<SIMULATION_DURATION_USEC) {
		uint64_t globalTime =  bb->getSchedulerTimeForLocalTime(time);
		uint64_t syncTime = globalTime + delay;
		block2Answer=NULL;
		Color c = getColor(time/COLOR_CHANGE_PERIOD_USEC);
		received=false; 
		if (bb->blockId==1){ 
			received=true;
			sendClockToNeighbors(NULL,1,bb->getTime());
		}
		BlinkyBlocks::getScheduler()->schedule(new SetColorEvent(syncTime,bb,c));
		time += COLOR_CHANGE_PERIOD_USEC;
		BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
	}
}

void BbCycleBlockCode::startup() {
	stringstream info;
	delay=0;
	info << "  Starting BbCycleBlockCode in block " << hostBlock->blockId;
	init();
}

void BbCycleBlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	MessagePtr message;
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	info.str("");
	
	OUTPUT << bb->blockId << " processLocalEvent: date: "<< BaseSimulator::getScheduler()->now() << " process event " << pev->getEventName() << "(" << pev->eventType << ")" << ", random number : " << pev->randomNumber << endl;
	
	switch (pev->eventType) {
		case EVENT_SET_COLOR:
			{
			Color color = (boost::static_pointer_cast<SetColorEvent>(pev))->color;
			bb->setColor(color);
			info << "set color "<< color << endl;
			}
			break;
		case EVENT_NI_RECEIVE:
			{
			message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message; 
			P2PNetworkInterface * recvInterface = message->destinationInterface;
			switch(message->id){
				case SYNC_MSG_ID : 
					{
					SynchroMessage_ptr recvMessage = boost::static_pointer_cast<SynchroMessage>(message);
					if (!received){
						received=true;
						//delay = bb->getTime() - (recvMessage->time) + 6000*(recvMessage->nbhop); //How do I change the time ?
						delay = 10000000*(recvMessage->nbhop);
						block2Answer=recvInterface;
						recvMessage->nbhop++;
						sendClockToNeighbors(block2Answer,recvMessage->nbhop,recvMessage->time);
						info<<"synchronized"<< delay << endl;
						}
					}
					break;
				default:
					break;
				}
			}
		default:
			ERRPUT << "*** ERROR *** : unknown local event" << endl;
			break;
		}
		BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
}


Color BbCycleBlockCode::getColor(uint64_t time) {
	Color colors[6] = {RED,GREEN,YELLOW,BLUE,GREY,PINK};
	int c = time%6;
	return colors[c];
}

BlinkyBlocks::BlinkyBlocksBlockCode* BbCycleBlockCode::buildNewBlockCode(BlinkyBlocksBlock *host) {
	return(new BbCycleBlockCode(host));
}


void BbCycleBlockCode::sendClockToNeighbors (P2PNetworkInterface *p2pExcept, int hop, uint64_t clock){
	P2PNetworkInterface * p2p;
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	
	for (int i=0; i<6 ; i++) {
	p2p = bb->getInterface(NeighborDirection::Direction(i));
		if (p2p->connectedInterface && p2p!=p2pExcept){
			SynchroMessage *message = new SynchroMessage(clock, hop);
			BlinkyBlocks::getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent (BlinkyBlocks::getScheduler()->now(), message, p2p));
		}
	}
}

SynchroMessage::SynchroMessage(uint64_t t, int hop) :Message(){
	id = SYNC_MSG_ID;
	time = t;
	nbhop = hop;
}

SynchroMessage::~SynchroMessage(){
}
