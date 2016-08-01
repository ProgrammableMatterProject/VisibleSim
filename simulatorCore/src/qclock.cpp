#include "qclock.h"
#include "scheduler.h"

using namespace BaseSimulator;

//==================================================================================================
//
//          QClock  (class)
//
//==================================================================================================

QClock::QClock(double _d, double _y0, double _x0): Clock() {
  d = _d;
  y0 = _y0;
  x0 = _x0;
}

QClock::QClock(double mean[], double sd[], unsigned int seed): Clock() {
  double values[2];
  for (int i = 0; i < 2; i++) {
    mt19937 uGenerator(seed);
    normal_distribution<double> normalDist(mean[i],sd[i]);
    auto generator = std::bind(normalDist, uGenerator);
    values[i] = generator();
  }
  d = values[0];
  y0 = values[1];
  x0 = BaseSimulator::getScheduler()->now();
}

uint64_t QClock::getTime(uint64_t simTime) {
  uint64_t localTime = (1.0/2.0)*d*pow((double)simTime,2) + y0*((double)simTime) + x0;
  return localTime;
}

uint64_t QClock::getSchedulerTimeForLocalTime(uint64_t localTime) {
  double simTime = 0;
  double delta =  pow(y0,2) - 4 * (1.0/2.0)*d * (x0-(double)localTime);
  if (delta > 0) {
    double s1 = (-y0 + sqrt(delta)) / d;
    double s2 = (-y0 - sqrt(delta)) / d;
    // we take the value closest to localTime
    if (abs(s1-localTime) < abs(s2-localTime)) {
      simTime = s1;
    } else {
      simTime = s2;
    }
  } else if (delta == 0) {
        simTime = -y0 / d;
  } else {
    cerr << "delta should be positive!" << endl;
    simTime = 0;
  }
  
  simTime = max(0.0,simTime);
  simTime = min(numeric_limits<double>::max(),simTime);
  return (uint64_t) simTime;
}

//==============================================================================
//
//          GNoiseQClock  (class)
//
//==============================================================================

GNoiseQClock::GNoiseQClock(double _d, double _y0, double _x0, double _sigma, unsigned int _seed): QClock(_d,_y0,_x0) {
  noise = new GClockNoise(_seed,0,_sigma);
}

GNoiseQClock::GNoiseQClock(double mean[], double sd[], unsigned int _seed): QClock(mean,sd,_seed) {
  mt19937 uGenerator(_seed);
  normal_distribution<double> normalDist(mean[2],sd[2]);
  auto generator = std::bind(normalDist, uGenerator);
  double sigma = generator();
  noise = new GClockNoise(_seed,0,sigma);
}

GNoiseQClock::~GNoiseQClock () {
  delete noise;
}

uint64_t GNoiseQClock::getTime(uint64_t simTime) {
  double localTime = 0;
  double noise_SimTime = 0;
  
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

  noise_SimTime = noise->getNoise(simTime);
  
  localTime = (1.0/2.0)*d*pow((double)simTime,2) + y0*((double)simTime) + x0 + noise_SimTime;
  localTime = max(minL,localTime);
  localTime = min(maxL,localTime);

  insertReferencePoint((uint64_t)localTime,simTime,it);

  return (uint64_t)localTime;
}

uint64_t GNoiseQClock::getSchedulerTimeForLocalTime(uint64_t localTime) {
  double noise_LocalTime = 0;
  double simTime = 0;
    
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
 
  noise_LocalTime = noise->getNoise(localTime);
  
  double delta =  pow(y0,2) - 4 * (1.0/2.0)*d * (x0+noise_LocalTime-(double)localTime);
  if (delta > 0) {
    double s1 = (-y0 + sqrt(delta)) / d;
    double s2 = (-y0 - sqrt(delta)) / d;
    // we take the value closest to localTime
    if (abs(s1-localTime) < abs(s2-localTime)) {
      simTime = s1;
    } else {
      simTime = s2;
    }
  } else if (delta == 0) {
    simTime = -y0 / d;
  } else {
    cerr << "delta should be positive!" << endl;
    simTime = minL;
  }

  simTime = max(minL,simTime);
  simTime = min(maxL,simTime);

  insertReferencePoint(localTime,(uint64_t)simTime,it);

  return (uint64_t) simTime;
}

void GNoiseQClock::insertReferencePoint(uint64_t local, uint64_t simulation,
					list<ReferencePoint>::iterator pos) {
 ReferencePoint p = ReferencePoint(local,simulation); 
  if (pos == referencePoints.end()) {
    referencePoints.push_back(p);
  } else {
    referencePoints.insert(pos,p);
  }
}

void GNoiseQClock::cleanReferencePoints() {
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

//==============================================================================
//
//          DNoiseQClock  (class)
//
//==============================================================================

DNoiseQClock::DNoiseQClock(double _d, double _y0, double _x0, unsigned int _seed): QClock(_d,_y0,_x0) {
  noise = new DClockNoise(_seed);
}

DNoiseQClock::DNoiseQClock(double mean[], double sd[], unsigned int _seed): QClock(mean,sd,_seed) {
  noise = new DClockNoise(_seed);
}

DNoiseQClock::~DNoiseQClock() {
  delete noise;
}

void loadNoiseData(vector<string> &noiseData) {
  DClockNoise::loadData(noiseData);
}

uint64_t DNoiseQClock::getTime(uint64_t simTime) {
  uint64_t noise_SimTime = noise->getNoise(simTime);
  uint64_t localTime = (1.0/2.0)*d*pow((double)simTime,2) + y0*((double)simTime) + x0 + noise_SimTime;
  return localTime;
}

uint64_t DNoiseQClock::getSchedulerTimeForLocalTime(uint64_t localTime) {
  uint64_t l = localTime;
  uint64_t s = QClock::getSchedulerTimeForLocalTime(localTime);

  while (l < localTime) {
    s++;
    l = getTime(s);
    if (l >= localTime) {
      return s;
    }
  }
  
  while(l > localTime) {
    s--;
    l = getTime(s);
    if (l < localTime) {
      return s+1;
    }
  }
  return s;
}
