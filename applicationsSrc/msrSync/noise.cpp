#include "noise.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>

using namespace std;

#define NOISE_NB 6
//#define DIRECTORY "noise-test/"
#define DIRECTORY "noise-data/"

//#define DEBUG_NOISE

Noise::Noise() {
  uint64_t time;
  noise_t noise;

  for (int i = 1; i <= NOISE_NB; i++) {
    stringstream path;
    path << DIRECTORY << i << ".dat";
    ifstream file (path.str().c_str());
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
  //print();
  //test();
}

Noise::~Noise() {
}

unsigned int Noise::getNoiseId(unsigned int r) {
  return r%noiseSignals.size();
}

void Noise::print() {
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
      noise_t n = p.second;
      cout << "\t" << t << " " << n << endl;
    }
  }
}

noise_t Noise::getNoise(uint64_t time, referencePt_t p1, referencePt_t p2) {
  // assume linear noise between interval points [p1;p2]
  double t1,n1,t2,n2;
  t1 = p1.first;
  n1 = p1.second;
  t2 = p2.first;
  n2 = p2.second;
  double a = (n2-n1)/(t2-t1); 
  double b = n2 - a*t2;
  return round(a*(double)time + b);
}

noise_t Noise::getNoise(unsigned int id, uint64_t time) {
  if (id >= noiseSignals.size()) {
    cerr << "Illegal noise id: " << id << endl;
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
    if(t >= time) {
      break;
    }
  }
  
  int cas = 0; 
  if (it == signal.begin()) {
    p1 = make_pair(0,0);
    p2 = signal.front();
    cas = 1;
  } else if (it == signal.end()) {
    cerr << "Warning node signal shorter than the simulation" << endl;
    p1 = signal.back();
    p2 = make_pair(UINT64_MAX,0);
    cas = 2;
  } else if (it->first == time) {
    return it->second;
  } else {
    p2 = *it;
    it--;
    p1 = *it;
    cas = 4;
  }
  
  noise_t noise = getNoise(time,p1,p2);
#ifdef DEBUG_NOISE   
  cout << "getNoise(" << id << "," << time << ") = "  << noise 
       << ", time in [" << p1.first << "," << p2.first << "]"
       << ", noise in [" << p1.second << "," << p2.second << "]"
       << "(cas:" << cas
       << endl;
#endif
  return noise;
}

void Noise::test() {
  noise_t noise;
  uint64_t time;
  unsigned int id = 0;

  time = 2;
  noise = getNoise(id,time);
  cout << "getNoise(" << time << ")" << " = " << noise << endl; 
  
  time = 5;
  noise = getNoise(id,time);
  cout << "getNoise(" << time << ")" << " = " << noise << endl;

  time = 7;
  noise = getNoise(id,time);
  cout << "getNoise(" << time << ")" << " = " << noise << endl;
  
  time = 1;
  noise = getNoise(id,time);
  cout << "getNoise(" << time << ")" << " = " << noise << endl;
  
  time = 3;
  noise = getNoise(id,time);
  cout << "getNoise(" << time << ")" << " = " << noise << endl;
  
  time = 6;
  noise = getNoise(id,time);
  cout << "getNoise(" << time << ")" << " = " << noise << endl; 

  time = 9;
  noise = getNoise(id,time);
  cout << "getNoise(" << time << ")" << " = " << noise << endl;
}
