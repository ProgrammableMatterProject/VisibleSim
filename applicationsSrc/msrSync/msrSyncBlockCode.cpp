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

#define LIMIT_NUM_ROUNDS 10

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
	
	/*uint64_t time = 0;
	while (time<SIMULATION_DURATION_USEC) {
		uint64_t globalTime =  bb->getSchedulerTimeForLocalTime(time);
		Color c = getColor(time/COLOR_CHANGE_PERIOD_USEC);
		BlinkyBlocks::getScheduler()->schedule(new SetColorEvent(globalTime,bb,c));
		time += COLOR_CHANGE_PERIOD_USEC;
	}*/
	
#ifdef SYNCHRONIZATION
	if(hostBlock->blockId == 1) { // Time leader
		BlinkyBlocks::getScheduler()->schedule(new MsrSyncEvent(BaseSimulator::getScheduler()->now(),hostBlock));
	}
#endif
}

void msrSyncBlockCode::startup() {
	stringstream info;
	//BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	
	info << "  Starting msrSyncBlockCode in block " << hostBlock->blockId;
	BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
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
				round++;
				info << "MASTER sync " << round;
				synchronize(NULL);
				
				// schedule the next sync round
				if (round < LIMIT_NUM_ROUNDS) {
					uint64_t nextSync = hostBlock->getSchedulerTimeForLocalTime(hostBlock->getTime()+SYNC_PERIOD);
					//cout << nextSync << " " << BaseSimulator::getScheduler()->now() << endl;
					// or based on global time now ? BaseSimulator::getScheduler()->now()+SYNC_PERIOD
					BlinkyBlocks::getScheduler()->schedule(new MsrSyncEvent(nextSync,hostBlock));
				}
			}
			break;
		case EVENT_NI_RECEIVE:
			{
			MessagePtr message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
			P2PNetworkInterface * recvInterface = message->destinationInterface;
			switch(message->type) {
				case SYNC_MSG_ID : {
					SyncMessagePtr recvMessage = boost::static_pointer_cast<SyncMessage>(message);
					uint64_t globalTime = recvMessage->getTime() + COM_DELAY;
					info << "sync msg " << recvMessage->getRound();
					//cout << "@" << hostBlock->blockId << ": " << getTime() << "/" << globalTime << endl;
					error.push_back(abs(getTime()-globalTime));
					if (recvMessage->getRound() > round) {
						round = recvMessage->getRound();
						// global time (hardware)
						uint64_t localTime = hostBlock->getTime();
						// window of 5 last measures
						syncPoints.push_back(make_pair(localTime,globalTime));
						if (syncPoints.size() > 5) {
							syncPoints.erase(syncPoints.begin());
						}
						adjust();
						synchronize(recvInterface);
					}
					
					if (round == LIMIT_NUM_ROUNDS) {
						// display error vector
						cout << "@" << hostBlock->blockId << " error: ";
						for (vector<uint64_t>::iterator it = error.begin() ; it != error.end(); it++){
							cout << *it << " ";
						}
						cout << endl;
					}
				}
				break;
				default: 
					ERRPUT << "*** ERROR *** : unknown message" << message->id << endl;
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
	// Linear regression (same as in hardware bb)
	// https://github.com/claytronics/oldbb/blob/master/build/src-bobby/system/clock.bb
	// x: local time
	// y: global time
	double xAvg = 0, yAvg = 0;
	double sum1 = 0, sum2 = 0;
	
	if (syncPoints.size() == 0) {
		a = 1;
		return;
	}
	
	if (syncPoints.size() == 1) {
		a = syncPoints.begin()->second / syncPoints.begin()->first;
		//a = 1;
		return;
	}
	
	for (vector<pair<uint64_t,uint64_t> >::iterator it = syncPoints.begin() ; it != syncPoints.end(); it++){
		xAvg += it->first;
		yAvg += it->second;
	}
	
	xAvg = xAvg/syncPoints.size();
	yAvg = yAvg/syncPoints.size();
	for (vector<pair<uint64_t,uint64_t> >::iterator it = syncPoints.begin() ; it != syncPoints.end(); it++){
        sum1 += (it->first - xAvg) * (it->second - yAvg);
		sum2 += powf(it->first - xAvg,2);
	}

	a = sum1/sum2;
	cout << "@" << hostBlock->blockId << " a: " << a << endl;
	// b ?
}

BlinkyBlocks::BlinkyBlocksBlockCode* msrSyncBlockCode::buildNewBlockCode(BlinkyBlocksBlock *host) {
	return(new msrSyncBlockCode(host));
}
