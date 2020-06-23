/*
 * qclock.h
 *
 *  Created on: 24 July 2016
 *      Author: Andre
 */

#ifndef QCLOCK_H_
#define QCLOCK_H_

#include <stdint.h>
#include <list>
#include <vector>
#include <string>

#include "clock.h"
#include "clockNoise.h"
#include "../utils/random.h"

using namespace std;

namespace BaseSimulator {

/**
 * \brief class to simulate clock with quadratic model: x(t) = (1/2)*d*t^2 + y0*t + x0
 */
class QClock: public Clock {
protected:
  double d; //!< d parameter of the quadratic clock model
  double y0; //!< y0 parameter of the quadratic clock model
  double x0; //!< x0 parameter of the quadratic clock model

public:

  /**
   * @brief QClock constructor.
   * @para _d d parameter of the clock model
   * @para _y0 y0 parameter of the clock model
   * @para _x0 x0 parameter of the clock model
   */
  QClock(double _d, double _y0, double _x0);

  /**
   * @brief QClock constructor. Parameters d and y0 of the clock model are
   * randomly generated using a Gaussian distribution.
   * @para mean array of the mean of the parameters d and y0 (in that order).
   * @para sd array of the standard deviation of the parameters d and y0 (in
   that order).
   * @para seed seed to use in the random generation of d and y0.
   */
  //QClock(double mean[], double sd[], unsigned int seed);

  /**
   * QClock destructor.
   */
  virtual ~QClock() {};

  virtual Time getTime(Time simTime) override;
  virtual Time getSimulationTime(Time localTime) override;
};

/**
 * \brief class to simulate clock with quadratic model and gaussian noise
 * x(t) = (1/2)*d*t^2 + y0*t + x0 + N(0,sigma)
 * Simulation Method:
 */
class GNoiseQClock : public QClock {
  /**
   * \brief class to record reference point that associate local time to
   * simulation time
   */
  class ReferencePoint {
  public:
    Time local;
    Time simulation;


    ReferencePoint(Time l, Time s) {local = l; simulation = s;}
    ReferencePoint(const ReferencePoint &p) {local = p.local; simulation = p.simulation;}
    ~ReferencePoint() {};
  };

protected:
  GClockNoise *noise; //!< Gaussian noise
  list<ReferencePoint> referencePoints; //!< list of reference points that associate local time to simulation time

public:

  /**
   * @brief GNoiseQClock constructor.
   * @para _d d parameter of the clock model
   * @para _y0 y0 parameter of the clock model
   * @para _x0 x0 parameter of the clock model
   * @para _sigma sigma parameter of the clock model
   * @para _seed seed to use in random noise generation
   */
  GNoiseQClock(double _d, double _y0, double _x0, double _sigma, ruint _seed);

  /**
   * @brief GNoiseClock constructor. Parameters d, y0 and sigma of the clock
   * model are  randomly generated using a Gaussian distribution.
   * @para mean array of the mean of the parameters d, y0 and sigma (in that
   * order).
   * @para sd array of the standard deviation of the parameters d, y0 and sigma
   * (in that order).
   * @para seed seed to use in the random generation of d, y0, sigma and
   * noise(t) ~ N(0,sigma).
   */
  //GNoiseQClock(double mean[], double sd[], ruint _seed);

  /**
   * @brief GNoiseClock destructor.
   */
  ~GNoiseQClock();

  Time getTime(Time simTime) override;
  Time getSimulationTime(Time localTime) override;

 protected:
   /**
   * @brief Remove past reference points that are now uselessy.
   */
  void cleanReferencePoints();

  /**
   * @brief Insert a new reference point.
   * @para local local time.
   * @para simulation simulation time.
   * @para pos position where to insert the reference point in referencePoints
   */
  void insertReferencePoint(Time local, Time simulation, list<ReferencePoint>::iterator pos);
};

/**
 * \brief class to simulate clock with quadratic model and noise replayed
 * from data: x(t) = (1/2)*d*t^2 + y0*t + x0 + noise(t)
 */
class DNoiseQClock: public QClock {

 protected:
  DClockNoise *noise; //!< Noise replay from data

public:

  /**
   * @brief DNoiseQClock constructor.
   * @para _d d parameter of the clock model
   * @para _y0 y0 parameter of the clock model
   * @para _x0 x0 parameter of the clock model
   * @para _seed seed to use in the random choice of the noise signal to replay
   */
  DNoiseQClock(double _d, double _y0, double _x0, ruint _seed);

  /**
   * @brief DNoiseClock constructor. Parameters d and y0 of the clock
   * model are randomly generated using a Gaussian distribution.
   * @para mean array of the mean of the parameters d and y0 (in that
   * order).
   * @para sd array of the standard deviation of the parameters d and y0
   * (in that order).
   * @para seed seed to use in the random generation of d, y0 and the choice
   * of the noise signal to replay.
   */
  //DNoiseQClock(double mean[], double sd[], ruint _seed);

  /**
   * @brief DNoiseClock destructor.
   */
  ~DNoiseQClock();

  Time getTime(Time simTime) override;
  Time getSimulationTime(Time localTime) override;

  /**
   * @brief Load the noise signal data from files.
   * @para files path to noise signal files.
   */
  static void loadNoiseData(vector<string> &files);

  /**
   * @brief Create a DNoiseQClock that simulates the XMEGA_RTC_OSC1K_CRC
   * hardware clock. XMEGA_RTC_OSC1K_CRC: Calibrated RC Oscillator, 1 ms
   * 1% accuracy (10 000 ppm) at 3V and 25°C.
   * @para seed seed use to randomly select the noise signal
   */
  static DNoiseQClock* createXMEGA_RTC_OSC1K_CRC(ruint seed);
};

}


#endif
