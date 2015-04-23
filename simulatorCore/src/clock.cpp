#include "clock.h"
#include "scheduler.h"
#include <limits>

namespace BaseSimulator {

  Clock::Clock(ClockType clockType, BuildingBlock *h) {
    hostBlock = h;
    type = clockType;
    setClockProperties(clockType);

    x0 = BaseSimulator::getScheduler()->now();
    generator =  boost::rand48(hostBlock->blockId);
  }
	
  void Clock::setClockProperties(ClockType clockType) {
    switch (clockType) {
    case XMEGA_RTC_OSC1K_ULPRC:
      resolution = RESOLUTION_1MS;
      accuracy = ACCURACY_320000PPM;
      D=0;
      y0=1;
      cerr << "warning XMEGA_RTC_OSC1K_ULPRC oscillator not supported yet!" << endl;
      break;
    case XMEGA_RTC_OSC1K_CRC:
      {
      resolution = RESOLUTION_1MS;
      accuracy = ACCURACY_10000PPM;
      
      // D, y0, x0, errorSD
      double mean[3] = { -1.179717*pow(10,-11), 0.9922277, 2080.197};
      double sd[3] = {3.060884*pow(10,-12), 0.001851285, 294.832};
      double v[3];
      
      for (int i = 0; i < 3; i++) {
	boost::mt19937 uGenerator(hostBlock->blockId);
	boost::normal_distribution<double> normalDist(mean[i],sd[i]);
	boost::variate_generator<boost::mt19937&, boost::normal_distribution<double> > generator(uGenerator, normalDist);
	v[i] = generator();
      }
      D = v[0];
      y0 = v[1];
      sigma = v[2];
      }
      break;
    case XMEGA_RTC_OSC32K_EXT:
      resolution = RESOLUTION_1MS;
      accuracy = ACCURACY_100PPM;
      D=0;
      y0=1;
      cerr << "warning  XMEGA_RTC_OSC32K_EXT oscillator not supported yet!" << endl;
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
  
  uint64_t Clock::getTimeMS(uint64_t simTime) {
    return getTimeUS(simTime)/1000;
  }

  uint64_t Clock::getTimeUS(uint64_t simTime) {
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

    localTime = (1.0/2.0)*D*pow((double)simTime,2) + y0*((double)simTime) + x0 + noise;
    localTime = max(minL,localTime);
    localTime = min(maxL,localTime);

    ReferencePoint p = ReferencePoint((uint64_t)localTime,simTime); 
    if (it == referencePoints.end()) {
      referencePoints.push_back(p);
    } else {
      referencePoints.insert(it,p);
    }

    return (uint64_t)localTime;
  }
  
  uint64_t Clock::getSchedulerTimeForLocalTime(uint64_t localTime) {
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

    double delta =  pow(y0,2) - 4 * (1.0/2.0)*D * (x0+noise-(double)localTime);
    if (delta > 0) {
      double s1 = (-y0 + sqrt(delta)) / D;
      double s2 = (-y0 - sqrt(delta)) / D;
      // we take the value closest to localTime
      if (abs(s1-localTime) < abs(s2-localTime)) {
	simTime = s1;
      } else {
	simTime = s2;
      }
    } else if (delta == 0) {
      simTime = -y0 / D;
    } else {
      cerr << "delta should be positive!" << endl;
      simTime = minL;
    }

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
  
  void Clock::cleanReferencePoints() {
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

