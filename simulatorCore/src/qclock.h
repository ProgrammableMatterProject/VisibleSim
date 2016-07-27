/*
 * qclock.h
 *
 *  Created on: 24 July 2016
 *      Author: Andre
 */

#ifndef QCLOCK_H_
#define QCLOCK_H_

#include <stdint.h> 
#include <random>
#include <list>

#include "clock.h"

using namespace std;

namespace BaseSimulator {
/**
 * \brief class to simulate clock with quadratic model: x(t) = (1/2)*d*t^2 + y0*t + x0
 */
class QClock: public Clock {
protected:
  double d;
  double y0;
  double x0;

public:
  QClock(double _d, double _y0, double _x0);
  QClock(double mean[], double sd[], unsigned int seed);
  ~QClock() {};

  virtual uint64_t getTime(uint64_t simTime);
  virtual uint64_t getSchedulerTimeForLocalTime(uint64_t localTime);
};

 class ReferencePoint {
 public:
   uint64_t local;
   uint64_t simulation;
   
   ReferencePoint(uint64_t l, uint64_t s) {local = l; simulation = s;}
   ReferencePoint(const ReferencePoint &p) {local = p.local; simulation = p.simulation;}
   ~ReferencePoint() {};
  };
 
/**
 * \brief class to simulate clock with quadratic model and gaussian noise 
 * x(t) = (1/2)*d*t^2 + y0*t + x0 + N(0,sigma)
 * Simulation Method: 
 */
class GNoiseQClock : public QClock {  
protected:
  unsigned int seed;
  double sigma;
  list<ReferencePoint> referencePoints;
  ranlux48 noiseGenerator;
  uniform_int_distribution<> dis;

public:
  GNoiseQClock(double _d, double _y0, double _x0, double _sigma, unsigned int _seed);
  GNoiseQClock(double mean[], double sd[], unsigned int _seed);
  ~GNoiseQClock() {};

  uint64_t getTime(uint64_t simTime);
  uint64_t getSchedulerTimeForLocalTime(uint64_t localTime);

 protected:
  void cleanReferencePoints();
  void insertReferencePoint(uint64_t local, uint64_t simulation, list<ReferencePoint>::iterator pos);
};
 
 
}


#endif
