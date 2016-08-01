#include <iostream>
#include <fstream>
#include <random>
#include "clockNoise.h"


using namespace std;
using namespace BaseSimulator;


//==============================================================================
//
//          ClockNoise  (class)
//
//==============================================================================

ClockNoise::ClockNoise() {}

ClockNoise::~ClockNoise() {}

//==============================================================================
//
//          GClockNoise  (class)
//
//==============================================================================

GClockNoise::GClockNoise(unsigned int _seed, double _mean, double _sd) {
  seed = _seed;
  mean = _mean;
  sd = _sd;
}

GClockNoise::~GClockNoise() {}

clockNoise_t GClockNoise::getNoise(uint64_t simTime) {
  mt19937 uGenerator(simTime*seed);
  normal_distribution<double> normalDist(mean,sd);
  return normalDist(uGenerator);
}

//==============================================================================
//
//          DNoiseQClock  (class)
//
//==============================================================================

DClockNoise::DClockNoise(unsigned int _seed) {
  id = _seed % noiseSignals.size();
}

DClockNoise::~DClockNoise() {}

void DClockNoise::loadData(vector<string> &files) {
  uint64_t time;
  clockNoise_t noise;

  noiseSignals.clear();
  for (unsigned int i = 0; i < files.size(); i++) {
    ifstream file (files[i].c_str());
    noiseSignal_t signal;
    if (file.is_open()) {
      while(file >> time >> noise) {
	referencePt_t p = make_pair(time,noise);
	signal.push_back(p);
	//cout << time << " " << noise << endl;
      }
      file.close();
      noiseSignals.push_back(signal);
    } else {
      cout << "Unable to open file" << endl;
    }
  }
}

void DClockNoise::print() {
  int i = 0;
  for (vector<noiseSignal_t>::iterator it1 = noiseSignals.begin();
       it1 != noiseSignals.end(); it1++) {
    noiseSignal_t &signal = *it1;
    cout << "noise: " << i << endl;
    i++;
    for (vector<referencePt_t>::iterator it2 = signal.begin();
	 it2 != signal.end(); it2++) {
      referencePt_t &p = *it2;
      uint64_t t = p.first;
      clockNoise_t n = p.second;
      cout << "\t" << t << " " << n << endl;
    }
  }
}

clockNoise_t DClockNoise::getNoise(uint64_t simTime, referencePt_t p1, referencePt_t p2) {
  // assume linear noise between interval points [p1;p2]
  double t1,n1,t2,n2;
  t1 = p1.first;
  n1 = p1.second;
  t2 = p2.first;
  n2 = p2.second;
  double a = (n2-n1)/(t2-t1); 
  double b = n2 - a*t2;
  return round(a*(double)simTime + b);
}

clockNoise_t DClockNoise::getNoise(uint64_t simTime) {
  if (id >= noiseSignals.size()) {
    cerr << "ERROR: wrong noise id (" << id << ")" << endl;
    return 0;
  }
  noiseSignal_t &signal = noiseSignals[id];
  // identify interval "time" belongs to.

  if (signal.size() == 0) {
    cerr << "No signal for this noise id" << endl;
    return 0;
  }

  referencePt_t p1;
  referencePt_t p2;
  //vector<referencePt_t>::iterator it2;
  vector<referencePt_t>::iterator it;
  for (it = signal.begin();
       it != signal.end(); it++) {
    referencePt_t &p = *it;
    uint64_t t = p.first;
    if(t >= simTime) {
      break;
    }
  }
  
#ifdef DEBUG_NOISE  
  int cas = 0;
#endif
  if (it == signal.begin()) {
    p1 = make_pair(0,0);
    p2 = signal.front();
#ifdef DEBUG_NOISE 
    cas = 1;
#endif
  } else if (it == signal.end()) {
    cerr << "Warning node signal shorter than the simulation" << endl;
    p1 = signal.back();
    p2 = make_pair(UINT64_MAX,0);
#ifdef DEBUG_NOISE 
    cas = 2;
#endif    
  } else if (it->first == simTime) {
    return it->second;
  } else {
    p2 = *it;
    it--;
    p1 = *it;
#ifdef DEBUG_NOISE 
    cas = 4;
#endif
  }
  
  clockNoise_t noise = getNoise(simTime,p1,p2);
#ifdef DEBUG_NOISE   
  cout << "getNoise(" << id << "," << simTime << ") = "  << noise 
       << ", time in [" << p1.first << "," << p2.first << "]"
       << ", noise in [" << p1.second << "," << p2.second << "]"
       << "(cas:" << cas
       << endl;
#endif
  return noise;
}
