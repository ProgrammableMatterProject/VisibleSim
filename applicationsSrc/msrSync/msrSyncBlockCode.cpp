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
#include <fstream>

using namespace std;
using namespace BlinkyBlocks;

#define COLOR_CHANGE_PERIOD_USEC (2*1000*1000)
#define SIMULATION_DURATION_USEC ((uint64_t)24*60*60*1000*1000)

#define SYNCHRONIZATION
#define SYNC_PERIOD_US ((uint64_t)10*1000*1000)
#define COM_DELAY_US ((uint64_t)6*1000)

#define LIMIT_NUM_ROUNDS (SIMULATION_DURATION_USEC/SYNC_PERIOD_US)

//#define PRINT_NODE_INFO
//#define INFO_NODE_ID 200

#define PRINT_DATA_2_FILE

msrSyncBlockCode::msrSyncBlockCode(BlinkyBlocksBlock *host): BlinkyBlocksBlockCode(host) {
  y0 = 1;
  x0 = 0;
  round = 0;
	
  OUTPUT << "msrSyncBlockCode constructor" << endl;
}

msrSyncBlockCode::~msrSyncBlockCode() {
  OUTPUT << "msrSyncBlockCode destructor" << endl;
}

void msrSyncBlockCode::init() {
  BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	
  /*uint64_t time = 0;
    while (time<SIMULATION_DURATION_USEC) {
    uint64_t globalTime =  bb->getSchedulerTimeForLocalTime(time);
    Color c = getColor(time/COLOR_CHANGE_PERIOD_USEC);
    BlinkyBlocks::getScheduler()->schedule(new SetColorEvent(globalTime,bb,c));
    time += COLOR_CHANGE_PERIOD_USEC;
    }*/
  // empty the file if it exists
  ofstream file;
  string name = "data/"+to_string(bb->blockId)+".dat";
  file.open(name.c_str());
  file.close();

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
      Color color = (std::static_pointer_cast<SetColorEvent>(pev))->color;
      bb->setColor(color);
      info << "set color "<< color << endl;
    }
    break;
  case EVENT_MSRSYNC:
    {
      round++;
      info << "MASTER sync " << round;
#ifdef PRINT_NODE_INFO
      // cout << "MASTER SYNC " << getTime() << endl;
#endif
      synchronize(NULL,getTime());
      // schedule the next sync round
      if (round < LIMIT_NUM_ROUNDS) {
	uint64_t nextSync = hostBlock->getSchedulerTimeForLocalTime(hostBlock->getTime()+SYNC_PERIOD_US);
	//cout << nextSync << " " << BaseSimulator::getScheduler()->now() << endl;
	// or based on global time now ? BaseSimulator::getScheduler()->now()+SYNC_PERIOD
	BlinkyBlocks::getScheduler()->schedule(new MsrSyncEvent(nextSync,hostBlock));
      }
    }
    break;
  case EVENT_NI_RECEIVE:
    {
      MessagePtr message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
      P2PNetworkInterface * recvInterface = message->destinationInterface;
      switch(message->type) {
      case SYNC_MSG_ID : {
	SyncMessagePtr recvMessage = std::static_pointer_cast<SyncMessage>(message);
	info << "sync msg " << recvMessage->getRound();
	//cout << "@" << hostBlock->blockId << ": " << getTime() << "/" << globalTime << endl;
	if (recvMessage->getRound() > round) {
	  round = recvMessage->getRound();
	  uint64_t globalTime = recvMessage->getTime() + COM_DELAY_US;
	  uint64_t localTime = hostBlock->getTime();
	  // window of 5 last measures
	  syncPoints.push_back(make_pair(localTime,globalTime));
#ifdef PRINT_NODE_INFO
	  if (hostBlock->blockId == INFO_NODE_ID) {
	    cout << "Reception time: " << BaseSimulator::getScheduler()->now()/1000 << endl;
	    cout <<  "x0= " << x0 << ", y0= " << y0 << endl;
	    cout << "estimation: " << getTime()/1000 << "(" << hostBlock->getTime()/1000 << ")" << ", reception: " << recvMessage->getTime()/1000 << ", => " << globalTime/1000 << endl; 
	  }
#endif
#ifdef PRINT_DATA_2_FILE
	  uint64_t realTime = BaseSimulator::getScheduler()->now();
	  ofstream file;
	  string name = "data/"+to_string(bb->blockId)+".dat";
	  file.open(name.c_str(), fstream::app);
	  file << realTime << " " << globalTime << " " << localTime << " " << getTime() << endl;
	  file.close();
#endif

	  error.push_back(abs(((double)getTime()-(double)globalTime)/1000));
	  if (syncPoints.size() > 5) {
	    syncPoints.erase(syncPoints.begin());
	  }
	  adjust();
#ifdef PRINT_NODE_INFO
	  if (hostBlock->blockId == INFO_NODE_ID) {
	    cout << "@" << hostBlock->blockId << " x0= " << x0 << ", y0= " << y0 << endl;
	  }
#endif
	  synchronize(recvInterface, globalTime);
	  
	  if (round == LIMIT_NUM_ROUNDS) {
	    // display error vector
#ifdef PRINT_NODE_INFO
	    // if (hostBlock->blockId == INFO_NODE_ID) {
	    cout << "@" << hostBlock->blockId << " error: ";
	    for (vector<uint64_t>::iterator it = error.begin() ; it != error.end(); it++){
	      cout << *it << " ";
	    }
	    cout << endl;
	    // }
#endif
	  }
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
		
  if (info.str() != "") {
    BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
  }
}


Color msrSyncBlockCode::getColor(uint64_t time) {
  Color colors[6] = {RED,GREEN,YELLOW,BLUE,GREY,PINK};
  int c = time%6;
  return colors[c];
}

uint64_t msrSyncBlockCode::getTime() {
  return y0*(double)hostBlock->getTime() + x0;
}

void msrSyncBlockCode::synchronize(P2PNetworkInterface *exception, uint64_t globalTime) {
  list <P2PNetworkInterface*>::iterator it;
  for (it = hostBlock->getP2PNetworkInterfaceList().begin(); it !=hostBlock->getP2PNetworkInterfaceList().end(); it++) {
    if ((*it)->connectedInterface && (*it != exception)) {
      SyncMessage *message = new SyncMessage(globalTime,round);
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
    y0 = 1;
    return;
  }
	
  if (syncPoints.size() == 1) {
    if (syncPoints.begin()->first != 0) {
      y0 = syncPoints.begin()->second / syncPoints.begin()->first;
    } else {
      y0 = 1;
    }
    //A = 1;
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
    sum2 += pow(it->first - xAvg,2);
  }

  y0 = sum1/sum2;
  x0 = yAvg - y0 * xAvg;
}

BlinkyBlocks::BlinkyBlocksBlockCode* msrSyncBlockCode::buildNewBlockCode(BlinkyBlocksBlock *host) {
  return(new msrSyncBlockCode(host));
}
