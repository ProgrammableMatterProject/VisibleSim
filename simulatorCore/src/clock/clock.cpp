#include "clock.h"
#include "scheduler.h"
#include "buildingBlock.h"

namespace BaseSimulator {

//===========================================================================================================
//
//          Clock  (class)
//
//===========================================================================================================

Time Clock::getTime() {
    return getTime(getScheduler()->now());
}

//===========================================================================================================
//
//          PerfectBlock  (class)
//
//===========================================================================================================

PerfectClock::PerfectClock(): Clock() {}

PerfectClock::~PerfectClock() {}

Time PerfectClock::getTime(Time simTime) {
  return simTime;
}
  
Time PerfectClock::getSimulationTime(Time localTime) {
  return localTime;
}
 
}


