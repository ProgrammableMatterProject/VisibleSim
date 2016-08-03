/*
 * msrSyncBlockCode.cpp
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <sstream>

#include "scheduler.h"
#include "network.h"
#include "msrSyncBlockCode.h"
#include "msrSyncMessages.h"
#include "msrSyncEvents.h"
#include "configStat.h"
#include "trace.h"
#include <fstream>

#include "qclock.h"
#include "clockNoise.h"
#include <cmath>

using namespace std;
using namespace BlinkyBlocks;

#define MY_COUT cout << "@" << hostBlock->blockId << ": "

#define MASTER_SLAVE
//#define PEER_TO_PEER

#define ONE_MICROSECOND ((uint64_t)1)
#define ONE_MILLISECOND ((uint64_t)1000*ONE_MICROSECOND)
#define ONE_SECOND ((uint64_t)1000*ONE_MILLISECOND)
#define ONE_MINUTE ((uint64_t)60*ONE_SECOND)
#define ONE_HOUR ((uint64_t)60*ONE_MINUTE)

#define COLOR_CHANGE_PERIOD_USEC ((uint64_t)2*ONE_SECOND)

#define SIMULATION_START ONE_MINUTE
//#define SIMULATION_DURATION_USEC ((uint64_t)2*ONE_MINUTE)
#define SIMULATION_DURATION_USEC ONE_HOUR
//#define SIMULATION_DURATION_USEC ((uint64_t)24*ONE_HOUR)

// 1%
#define MESSAGE_LOSS_PROBABILITY (1.0/100.0)
#define COEFFCIENT_AFTER_LOSS (1.0/10.0)
#define NUMBER_OF_TRANSMISSION_TRIALS 5

//#define MEAN_TIME_TO_HANDLE_MESSAGE ((uint64_t)20*ONE_MICROSECOND)
//#define SD_TIME_TO_HANDLE_MESSAGE ((uint64_t)220*ONE_MICROSECOND)
//#define MIN_TIME_TO_HANDLE_MESSAGE ((uint64_t)10*ONE_MICROSECOND)

//#define MEAN_TIME_TO_HANDLE_MESSAGE ((uint64_t)ONE_MILLISECOND)
//#define SD_TIME_TO_HANDLE_MESSAGE ((uint64_t)50*ONE_MICROSECOND)

//old
#define MIN_TIME_TO_HANDLE_MESSAGE ((uint64_t)20*ONE_MICROSECOND)
#define MAX_TIME_TO_HANDLE_MESSAGE ((uint64_t)600*ONE_MICROSECOND)

//#define TIME_TO_HANDLE_MESSAGE() (getNormalRandomUint(MEAN_TIME_TO_HANDLE_MESSAGE,SD_TIME_TO_HANDLE_MESSAGE) + MIN_TIME_TO_HANDLE_MESSAGE)
#define TIME_TO_HANDLE_MESSAGE() getRandomUint(MIN_TIME_TO_HANDLE_MESSAGE,MAX_TIME_TO_HANDLE_MESSAGE)

#define TIME_BEFORE_RETRANSMISSION ((uint64_t)50*ONE_MILLISECOND)
//#define MIN_TIME_TO_HANDLE_RETRANSMISSION ((uint64_t)25*ONE_MICROSECOND)
//#define MAX_TIME_TO_HANDLE_RETRANSMISSION ((uint64_t)100*ONE_MICROSECOND)
//#define MEAN_TIME_TO_HANDLE_RETRANSMISSION  MEAN_TIME_TO_HANDLE_MESSAGE
//#define SD_TIME_TO_HANDLE_RETRANSMISSION  SD_TIME_TO_HANDLE_MESSAGE
#define TIME_TO_HANDLE_RETRANSMISSION() TIME_TO_HANDLE_MESSAGE() 
//#define TIME_TO_HANDLE_RETRANSMISSION() (getNormalRandomUint(MEAN_TIME_TO_HANDLE_RETRANSMISSION,SD_TIME_TO_HANDLE_RETRANSMISSION) + MIN_TIME_TO_HANDLE_MESSAGE)

#define SYNCHRONIZATION
#define SYNC_PERIOD_US ((uint64_t)5*ONE_SECOND)
#define COM_DELAY_US ((uint64_t)6*ONE_MILLISECOND)

#define LIMIT_NUM_ROUNDS (SIMULATION_DURATION_USEC/SYNC_PERIOD_US)

//#define PRINT_NODE_INFO
//#define INFO_NODE_ID 200
//#define DEBUG_PROTOCOL

#define PRINT_DATA_2_FILE
#define PRINT_NODE_ID "@" << hostBlock->blockId
static msrSyncBlockCode* timeMaster = NULL;

#define GET_GLOBAL_TIME timeMaster->getLocalTime

msrSyncBlockCode::msrSyncBlockCode(BuildingBlock *host): BlockCode(host) {
  y0 = 1;
  x0 = 0;
  round = 0;

  random_device rd;
  generator = ranlux48(rd());
  dis = uniform_int_distribution<>(0, 50 * host->blockId);
  
  // set BB clock
  DNoiseQClock* localClock = DNoiseQClock::createXMEGA_RTC_OSC1K_CRC(host->blockId);
  host->setClock(localClock);
  
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
  // empty the log files if they exist (for every block)
  if(hostBlock->blockId != 1) {
    ofstream file;
    string name = "data/"+to_string(bb->blockId)+".dat";
    file.open(name.c_str());
    file.close();
  }
  
#ifdef SYNCHRONIZATION
  if(hostBlock->blockId == 1) { // Time leader
    timeMaster = this;
	// BaseSimulator::getScheduler()->now()
    BaseSimulator::getScheduler()->schedule(new MsrSyncEvent(SIMULATION_START,hostBlock));
    MY_COUT << "time master" << endl;
  }
#endif
}

void msrSyncBlockCode::startup() {
  stringstream info;
  //BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	
  info << "  Starting msrSyncBlockCode in block " << hostBlock->blockId;
  BaseSimulator::getScheduler()->trace(info.str(),hostBlock->blockId);
  init();
}

uint64_t msrSyncBlockCode::getLocalTime(bool msResolution = true) {
  uint64_t simTime = BaseSimulator::getScheduler()->now();
  return getLocalTime(simTime,msResolution);
}

uint64_t msrSyncBlockCode::getLocalTime(uint64_t simTime, bool msResolution = true) {
  uint64_t localTime = hostBlock->getLocalTime(simTime);
  //MY_COUT << "Local Time: " << localTime << endl;
  if (msResolution) {
    localTime = localTime - localTime%ONE_MILLISECOND;
  }
  //MY_COUT << "Returned local Time: " << localTime << endl;
  return localTime;
}

uint64_t msrSyncBlockCode::getSimTime(uint64_t localTime) {
  uint64_t simTime = hostBlock->getSimulationTime(localTime);
  return simTime;
}

/**** Synchronized clock ****/
uint64_t msrSyncBlockCode::getTime() {
  return getTime(getLocalTime());
}

uint64_t msrSyncBlockCode::getTime(uint64_t localTime) {
  return y0*(double)localTime + x0;
}

uint msrSyncBlockCode::getRandomUint(uint _min, uint _max) {
  uint r = dis(generator);
  uint bounded_r = r%(_max-_min) + _min;
  return bounded_r;
}

uint msrSyncBlockCode::getNormalRandomUint(uint m, uint s) {
  unsigned int seed = getLocalTime() * hostBlock->blockId;
  mt19937 uGenerator(seed);
  normal_distribution<double> normalDist(m,s);
  auto gen = std::bind(normalDist, uGenerator);
  int v = (int) std::round(gen());
  return max(0,v);
}

double msrSyncBlockCode::getRandomDouble() {
  unsigned int seed = getLocalTime() * hostBlock->blockId;
  mt19937 gen(seed);
  uniform_real_distribution<> dis(0, 1);
  double r = dis(generator);
  return r;
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
      synchronize(NULL,getTime(),1);
      // schedule the next sync round
      if (round < LIMIT_NUM_ROUNDS) {
	// Should be 
	//uint64_t nextSync = getSimTime(getLocalTime(false)+SYNC_PERIOD_US);
	// But this is faster (although a little bit wrong)
	uint64_t simTime = BaseSimulator::getScheduler()->now();
	uint64_t nextSync = simTime + SYNC_PERIOD_US;
	
	BaseSimulator::getScheduler()->schedule(new MsrSyncEvent(nextSync,hostBlock));
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
	  uint64_t localTime = getLocalTime();
	  uint hop = recvMessage->getHop();
	  // window of 5 last measures
	  syncPoints.push_back(make_pair(localTime,globalTime));
#ifdef PRINT_NODE_INFO
	  if (hostBlock->blockId == INFO_NODE_ID) {
	    cout << "Reception time: " << BaseSimulator::getScheduler()->now()/1000 << endl;
	    cout <<  "x0= " << x0 << ", y0= " << y0 << endl;
	    cout << "estimation: " << getTime()/1000 << "(" << getLocalTime()/1000 << ")" << ", reception: " << recvMessage->getTime()/1000 << ", => " << globalTime/1000 << endl; 
	  }
#endif
#ifdef PRINT_DATA_2_FILE
	  uint64_t realTime = BaseSimulator::getScheduler()->now();
	  uint64_t realGlobalTime = GET_GLOBAL_TIME();
	  ofstream file;
	  string name = "data/"+to_string(bb->blockId)+".dat";
	  
#ifdef DEBUG_PROTOCOL
	  double estimationError = (double)realGlobalTime - (double)globalTime;
	  cout << PRINT_NODE_ID << "reception at " << realTime << ", estimated G: " << globalTime << " vs real global time: " 
	       << realGlobalTime << endl;
	  if (abs(estimationError) >= 50*ONE_MILLISECOND) {
	    cout << "Error! " << estimationError << endl;
	    getchar();
	  }
#endif	  
	  file.open(name.c_str(), fstream::app);
	  file << realTime << " " << realGlobalTime << " " << globalTime << " " << localTime << " " << getTime() << " " << hop << endl;
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
	  synchronize(recvInterface, globalTime, hop+1);
	  
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
    BaseSimulator::getScheduler()->trace(info.str(),hostBlock->blockId);
  }
}


Color msrSyncBlockCode::getColor(uint64_t time) {
  Color colors[6] = {RED,GREEN,YELLOW,BLUE,GREY,PINK};
  int c = time%6;
  return colors[c];
}

void msrSyncBlockCode::synchronize(P2PNetworkInterface *exception, uint64_t estimatedGlobalTime, uint hop) {
  uint64_t timeOfResidence = 0;
  // processing + interrupt times
  timeOfResidence += TIME_TO_HANDLE_MESSAGE();
  // time in case of retransmission(s)
  double pError = MESSAGE_LOSS_PROBABILITY;
  for (int i=0; i<=NUMBER_OF_TRANSMISSION_TRIALS;i++) {
    if (i == NUMBER_OF_TRANSMISSION_TRIALS) {
      return;
    }
    double p = getRandomDouble();
    //cout << "i= " << i <<  "pError= " << pError << ", p= " << p << endl; 
    if (p < pError) {
      timeOfResidence += TIME_BEFORE_RETRANSMISSION + TIME_TO_HANDLE_RETRANSMISSION();
      pError = pError *  COEFFCIENT_AFTER_LOSS;
    } else {
      break; 
    }
  }
  // timeOfResidence duration, sim scale != module scale
  uint64_t s1 = BaseSimulator::getScheduler()->now();
  uint64_t l1 = getLocalTime(false); // us resolution
  uint64_t s2 = getSimTime(l1+timeOfResidence);
  //MY_COUT << "Synchronize neighbors at " << s2 << "(now local: " << l1 << ", now sim: " << s1 << ")" << endl;
  uint64_t l2 = getLocalTime(s2,false);

#ifdef DEBUG_PROTOCOL
  double d = (double)l2 - (double)(l1+timeOfResidence);
  if (d != 0) {
    cout << "ERROR: l2 != l1+timeOfResidence: " << endl;
    cout
      << "d: " << d
      << ", timeOfResidence: " << timeOfResidence 
      << ", s1: " << s1
      << ", l1: " << l1
      << ", s2: " << s2
      << ", l2: " << l2
      << endl;
    //    getchar();
  }
  if (l2 < l1) { cout << "ERROR" << endl; }
#endif

  uint64_t t = l2-l1;
  uint64_t timeOfResidenceMSResolution = t - t%ONE_MILLISECOND; 
  uint64_t globalTime = (double)estimatedGlobalTime + y0*((double)timeOfResidenceMSResolution);

#ifdef DEBUG_PROTOCOL
  cout << PRINT_NODE_ID << "BlockCode: Sent Global Time = " << globalTime 
       << "(e=" << estimatedGlobalTime << ",residence=" 
       << timeOfResidence << "(" << timeOfResidenceMSResolution << ")" 
       << ",y0=" << y0
       << ",x0= " << x0
       << ")"
       << " sent at " << s2
       << endl;
#endif

  vector <P2PNetworkInterface*>& interfaces = hostBlock->getP2PNetworkInterfaces();
  vector <P2PNetworkInterface*>::iterator it;
  
  for (it = interfaces.begin(); it !=interfaces.end(); it++) {
    if ((*it)->connectedInterface && (*it != exception)) {
      SyncMessage *message = new SyncMessage(globalTime,round,hop);
      BaseSimulator::getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(s2, message,*it));
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
      y0 = (double)syncPoints.begin()->second / (double)syncPoints.begin()->first;
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
    sum1 += ((double)it->first - xAvg) * ((double)it->second - yAvg);
    sum2 += pow((double)it->first - xAvg,2);
  }

  y0 = sum1/sum2;
  x0 = yAvg - y0 * xAvg;
#ifdef DEBUG_PROTOCOL
  if (y0 > 2.0) {
    cout << "Error: y0 (=" << y0 << ") may be to high ? " << " x0= " << x0 << endl;
    for (vector<pair<uint64_t,uint64_t> >::iterator it = syncPoints.begin() ; it != syncPoints.end(); it++){
      double x = it->first;
      double y =  it->second;
      double o = x-y;
      cout << "\t(l=" << x << "," << y << ")" << o << endl;
    }
    getchar();
  }
#endif
}

BlockCode* msrSyncBlockCode::buildNewBlockCode(BuildingBlock *host) {
  return(new msrSyncBlockCode(host));
}
