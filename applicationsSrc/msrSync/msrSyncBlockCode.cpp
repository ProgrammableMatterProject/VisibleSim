/*
 * msrSyncBlockCode.cpp
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <sstream>
#include <boost/asio.hpp> 
#include "scheduler.h"
#include "network.h"
#include "msrSyncBlockCode.h"
#include "msrSyncMessages.h"
#include "msrSyncEvents.h"
#include "configStat.h"

#include "trace.h"

using namespace std;
using namespace BlinkyBlocks;

#define COLOR_CHANGE_PERIOD_USEC (2*1000*1000)
#define SIMULATION_DURATION_USEC (10*60*1000*1000)

#define SYNCHRONIZATION
#define SYNC_PERIOD (10*1000*1000)
#define COM_DELAY (6*1000*1000)

msrSyncBlockCode::msrSyncBlockCode(BlinkyBlocksBlock *host): BlinkyBlocksBlockCode(host) {
	a = 1;
	b = 0;
	round = 0;
	
	OUTPUT << "msrSyncBlockCode constructor" << endl;
}

msrSyncBlockCode::~msrSyncBlockCode() {
	OUTPUT << "msrSyncBlockCode destructor" << endl;
}

void msrSyncBlockCode::init() {
	//BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	stringstream info;
	
	/*uint64_t time = 0;
	while (time<SIMULATION_DURATION_USEC) {
		uint64_t globalTime =  bb->getSchedulerTimeForLocalTime(time);
		Color c = getColor(time/COLOR_CHANGE_PERIOD_USEC);
		BlinkyBlocks::getScheduler()->schedule(new SetColorEvent(globalTime,bb,c));
		time += COLOR_CHANGE_PERIOD_USEC;
	}*/
	
#ifdef SYNCHRONIZATION
	if(hostBlock->blockId == 1) { // Time leader
		round = 1;
		BlinkyBlocks::getScheduler()->schedule(new MsrSyncEvent(BaseSimulator::getScheduler()->now(),hostBlock));
	}
#endif
}

void msrSyncBlockCode::startup() {
	stringstream info;
	//BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	
	info << "  Starting msrSyncBlockCode in block " << hostBlock->blockId;
	init();
}

void msrSyncBlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
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
		case EVENT_MSRSYNC:
			{
				//BlinkyBlocks::getScheduler()->schedule(new MsrSyncEvent(BaseSimulator::getScheduler()->now,hostBlock));
				synchronize(NULL);
				BlinkyBlocks::getScheduler()->schedule(new MsrSyncEvent(hostBlock->getTime()+SYNC_PERIOD,hostBlock));
			}
			break;
		case EVENT_NI_RECEIVE:
			{
			MessagePtr message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
			P2PNetworkInterface * recvInterface = message->destinationInterface;
			switch(message->id) {
				case SYNC_MSG_ID : {
					SyncMessagePtr recvMessage = boost::static_pointer_cast<SyncMessage>(message);
					uint64_t esimatedTime = recvMessage->getTime() + COM_DELAY;
					if (recvMessage->getRound() > round) { 
						// estimatedTime
						adjust();
						synchronize(recvInterface);
					}
				}
				break;
				default: 
					ERRPUT << "*** ERROR *** : unknown message" << endl;
			}
			}
			break;
		default:
			ERRPUT << "*** ERROR *** : unknown local event" << endl;
			break;
		}
		
		BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
}


Color msrSyncBlockCode::getColor(uint64_t time) {
	Color colors[6] = {RED,GREEN,YELLOW,BLUE,GREY,PINK};
	int c = time%6;
	return colors[c];
}

uint64_t msrSyncBlockCode::getTime() {
	return a*hostBlock->getTime() + b;
}

void msrSyncBlockCode::synchronize(P2PNetworkInterface *exception) {
	list <P2PNetworkInterface*>::iterator it;
	for (it = hostBlock->getP2PNetworkInterfaceList().begin(); it !=hostBlock->getP2PNetworkInterfaceList().end(); it++) {
		if ((*it)->connectedInterface && (*it != exception)) {
			SyncMessage *message = new SyncMessage(getTime(),round);
            BaseSimulator::getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(BaseSimulator::getScheduler()->now(), message,*it));
		}
	}
}

void msrSyncBlockCode::adjust() {
	// https://github.com/claytronics/oldbb/blob/master/build/src-bobby/system/clock.bb
}

BlinkyBlocks::BlinkyBlocksBlockCode* msrSyncBlockCode::buildNewBlockCode(BlinkyBlocksBlock *host) {
	return(new msrSyncBlockCode(host));
}
