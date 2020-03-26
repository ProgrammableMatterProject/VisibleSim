/*
 * catom2D1BlockCode.cpp
 *
 *  Created on: April 2015
 *      Author: Andre
 */

#include <iostream>
#include <sstream>
#include <memory>
#include <float.h>

#include "catom2D1BlockCode.h"
#include "scheduler.h"
#include "events.h"
#include "rotation2DEvents.h"
#include "rate.h"

#include "c2srMsg.h"
#include "c2srEvents.h"

using namespace std;
using namespace Catoms2D;

#define ASSUME_COORDINATES // otherwise, construction

//#define RECONFIGURATION_DEBUG

//#define INIHIBIT_C2SR

#define START_C2SR_MIN_DELAY 1 //300
#define START_C2SR_MAX_DELAY 0 //1500

SimulationParameters Catoms2D1BlockCode::simParams;

Catoms2D1BlockCode::Catoms2D1BlockCode(Catoms2DBlock *host):Catoms2DBlockCode(host) {
  scheduler = getScheduler();
  catom2D = (Catoms2DBlock*)hostBlock;
  map = new Map(host);
  c2sr = new C2SR(host,map);
}

Catoms2D1BlockCode::~Catoms2D1BlockCode() {
  static bool statsPrinted = false;

  if (!statsPrinted) {
    C2SRMsg::printHopCountStats();
    statsPrinted = true;
  }

  delete c2sr;
  delete map;
}

void Catoms2D1BlockCode::startup() {
  stringstream info;
  info << "Starting ";
  scheduler->trace(info.str(),hostBlock->blockId);

  setSimulationParameters();

#ifdef ASSUME_COORDINATES
  map->assumeCoordinates();
  //cout << "@" << catom2D->blockId << " at " << map->position << endl;
  //c2sr->start();
#ifndef INIHIBIT_C2SR
  scheduleC2SRStart();
#endif
#else
  if (!map->isConnected && (catom2D->position[2] == 0)) {
    map->connectToHost();
#ifdef MAP_DEBUG
    catom2D->setColor(RED);
#endif
  }
#endif
}

void Catoms2D1BlockCode::processLocalEvent(EventPtr pev) {

  stringstream info;
  switch (pev->eventType) {
  case EVENT_NI_RECEIVE: {
    MessagePtr message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
    P2PNetworkInterface * recv_interface = message->destinationInterface;
    switch(message->type) {
    case GO_MAP_MSG:
    case BACK_MAP_MSG: {
      bool finished = map->handleMessage(message);
      if (finished) {
    scheduleC2SRStart();
    if (map->connectedToHost) {
      cout << "@" << catom2D->blockId << " has created the coordinate system" << endl;
    }
      }
    }
      break;
    case C2SR_MSG: {
      c2sr->handle(message);
    }
      break;
    default:
      cerr << "unknown message type" << endl;
    }
  }
    break;
  case EVENT_START_C2SR: {
    c2sr->start();
  }
    break;
  case  EVENT_ROTATION2D_END: {
    #ifdef RECONFIGURATION_DEBUG
    cout << "@" << catom2D->blockId << " motion end: " << catom2D->position << endl;
    #endif
    c2sr->handleStopMovingEvent();
  }
    break;
  }
}

void Catoms2D1BlockCode::setSimulationParameters() {
  // Set communication rate for all interfaces
  setCommunicationRate();
  // Set motion speed
  setMotionSpeed();
}

void Catoms2D1BlockCode::setCommunicationRate() {
  double mean = simParams.commRateMean;

  if (mean > 0) {
    double sd = mean*DEFAULT_SD_FACTOR;
    const vector<P2PNetworkInterface*>& interfaces = catom2D->getP2PNetworkInterfaces();
    vector<P2PNetworkInterface*>::const_iterator it;
    for (it = interfaces.begin() ; it != interfaces.end(); ++it) {
      P2PNetworkInterface* p2p = *it;
      doubleRNG g = Random::getNormalDoubleRNG(catom2D->getRandomUint(),mean,sd);
      RandomRate *rate = new RandomRate(g);
      p2p->setDataRate(rate);
    }
  }
}

void Catoms2D1BlockCode::scheduleC2SRStart() {
  Time d = 0;
  if (START_C2SR_MAX_DELAY > 0) {
    d = START_C2SR_MIN_DELAY + catom2D->getRandomUint()%(START_C2SR_MAX_DELAY-START_C2SR_MIN_DELAY);
  }
  Time t = scheduler->now()+d;
  scheduler->schedule(new StartC2SREvent(t,hostBlock));
}

void Catoms2D1BlockCode::setMotionSpeed() {
  double mean = simParams.motionSpeedMean;
  if (mean > 0) {
    double sd = mean*DEFAULT_SD_FACTOR;
    doubleRNG g = Random::getNormalDoubleRNG(catom2D->getRandomUint(),mean,sd);
    RandomRate *speed = new RandomRate(g);
    catom2D->motionEngine->setSpeed(speed);
  }
}

BlockCode* Catoms2D1BlockCode::buildNewBlockCode(BuildingBlock *host) {
  return(new Catoms2D1BlockCode((Catoms2DBlock*)host));
}
