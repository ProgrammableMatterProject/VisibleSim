#include "clock.h"
#include "scheduler.h"
#include <limits>

namespace BaseSimulator {

  Clock::Clock(ClockType clockType, BuildingBlock *h) {
    hostBlock = h;
    type = clockType;
    Clock::setClockProperties(clockType);
  }
	
  void Clock::setClockProperties(ClockType clockType) {
    switch (clockType) {
    case XMEGA_RTC_OSC1K_ULPRC:
      resolution = RESOLUTION_1MS;
      accuracy = ACCURACY_320000PPM;
      break;
    case XMEGA_RTC_OSC1K_CRC:
      resolution = RESOLUTION_1MS;
      accuracy = ACCURACY_10000PPM;
      break;
    case XMEGA_RTC_OSC32K_EXT:
      resolution = RESOLUTION_1MS;
      accuracy = ACCURACY_100PPM;
      break;
    default:
      cerr << "Undefined clock resolution" << endl;
    }	
  }

  uint64_t Clock::getTime() {
    return getTime(getScheduler()->now());
  }

  uint64_t Clock::getTime(uint64_t simTime) {
    switch (resolution) {
    case RESOLUTION_1US:
      return getTimeUS(simTime);
      break;
    case RESOLUTION_31US: {
      uint64_t time = getTimeUS(simTime);
      return floor(time/31) * 31;
    }			
      break;
    case RESOLUTION_1MS:
      return getTimeMS(simTime)*1000;
      break;
    default:
      cerr << "Undefined clock resolution" << endl;
      return 0;
    }
  }
  
  LinearDriftClock::LinearDriftClock(Clock::ClockType clockType, BuildingBlock *h) : Clock(clockType,h) {
    double accuracyPercentage = 0;
    double maxA = 0;
    double minA = 0;
  
    accuracyPercentage = (float)accuracy/pow(10,6); // ppm to %
    maxA = 1 + accuracyPercentage;
    minA = 1 - accuracyPercentage;
    
    startTime = BaseSimulator::getScheduler()->now();
    generator =  boost::rand48(hostBlock->blockId);
    a = (generator()/(double)RAND_MAX)*(maxA-minA) + minA;
    b = (double) startTime * a;
	
    setClockProperties(clockType);
  }
  
  void LinearDriftClock::setClockProperties(Clock::ClockType clockType) {
    switch (clockType) {
    case Clock::XMEGA_RTC_OSC1K_CRC: {
      double mean = 0; //1.06605 * pow(10,10);
      double variance = 0;//3.60608 * pow(10,20);
      boost::mt19937 uGenerator(hostBlock->blockId);
      boost::normal_distribution<double> normalDist(mean,sqrt(variance));
      boost::variate_generator<boost::mt19937&, boost::normal_distribution<double> > generator(uGenerator, normalDist);
      
      double sigma2 = 0;
      do {
	sigma2 =  generator();
      } while (sigma2 < 0);
      sigma = sqrt(sigma2);
    }
      break;
    case Clock::XMEGA_RTC_OSC1K_ULPRC:
      cerr << "Noise variation not defined for clock XMEGA_RTC_OSC1K_ULPRC" << endl;
      sigma = 0; 
      break;
    case Clock::XMEGA_RTC_OSC32K_EXT:
      cerr <<  "Noise variation not defined for clock XMEGA_RTC_OSC32K_EXT" << endl;
      sigma = 0;
      break;
    default:
      cerr << "Undefined clock resolution" << endl;
    }
  }

  uint64_t LinearDriftClock::getTimeMS(uint64_t simTime) {
    return getTimeUS(simTime)/1000;
  }

  uint64_t LinearDriftClock::getTimeUS(uint64_t simTime) {
    double localTime = 0;
    double noise = 0;

    boost::mt19937 uGenerator(simTime*hostBlock->blockId);
    boost::normal_distribution<double> normalDist(0,sigma);
    boost::variate_generator<boost::mt19937&, boost::normal_distribution<double> > generator(uGenerator, normalDist);
	
    double minL = 0;
    double maxL = numeric_limits<double>::max();
    list<ReferencePoint>::iterator it;

    cleanReferencePoints();
    for (it = referencePoints.begin(); it != referencePoints.end(); it++) {
      if (it->simulation == simTime) {
	return it->local;
      }	  
      if (it->simulation < simTime) {
	minL = max(minL,(double)it->local);
      }	  
      if (it->simulation > simTime) {
	maxL = min(maxL,(double)it->local);
	break; //sorted list
      }
    }
	
    noise = generator();

    localTime = a*((double)simTime) + b + noise;
    localTime = max(minL,localTime);
    localTime = min(maxL,localTime);

    //do {
    //  noise = generator();
    //  localTime = a*(simulationTime-startTime) + b + noise;
    //  cout << localTime << endl;
    //} while (! ((localTime <= minL) && (localTime >= maxL)));

    ReferencePoint p = ReferencePoint((uint64_t)localTime,simTime); 
    if (it == referencePoints.end()) {
      referencePoints.push_back(p);
    } else {
      referencePoints.insert(it,p);
    }

    return (uint64_t)localTime;
  }
  
  uint64_t LinearDriftClock::getSchedulerTimeForLocalTime(uint64_t localTime) {
    double noise = 0;
    double simTime = 0;

    boost::mt19937 uGenerator(localTime*hostBlock->blockId);
    boost::normal_distribution<double> normalDist(0,sigma);
    boost::variate_generator<boost::mt19937&, boost::normal_distribution<double> > generator(uGenerator, normalDist);
  
    double minL = 0;
    double maxL = numeric_limits<double>::max();
    list<ReferencePoint>::iterator it;

    cleanReferencePoints();
    for (it = referencePoints.begin(); it != referencePoints.end(); it++) {
      if (it->local == localTime) {
	return it->simulation;
      }
	  
      if (it->local < localTime) {
	minL = max(minL,(double)it->simulation);
      }
	  
      if (it->local > localTime) {
	maxL = min(maxL,(double)it->simulation);
	break; //sorted list
      }
    }
  
    noise = generator();
  
    simTime = ((double)localTime - b - noise)/a; 
    simTime = max(minL,simTime);
    simTime = min(maxL,simTime);

    ReferencePoint p = ReferencePoint(localTime,(uint64_t)simTime); 
    if (it == referencePoints.end()) {
      referencePoints.push_back(p);
    } else {
      referencePoints.insert(it,p);
    }

    return (uint64_t) simTime;
  }
  
  void LinearDriftClock::cleanReferencePoints() {
    uint64_t simTime = getScheduler()->now();

    for (list<ReferencePoint>::iterator it = referencePoints.begin();
	 it != referencePoints.end(); it++) {
      if (it->simulation >= simTime) {
	if (it != referencePoints.begin()) {
	  it--;
	  referencePoints.erase(referencePoints.begin(),it);
	}
	break;
      }
    }
    for (list<ReferencePoint>::iterator it = referencePoints.begin();
	 it != referencePoints.end(); it++) {
    }
  }
}

void Clock::pause(uint64_t delay, uint64_t start){
	while (BaseSimulator::getScheduler()->now() < start+delay){}
}
