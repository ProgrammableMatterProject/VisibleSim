/*
 * clock.h
 *
 *  Created on: 26 march 2015
 *      Author: andre
 */

#ifndef CLOCK_H_
#define CLOCK_H_

#include "buildingBlock.h"
#include <stdint.h>
#include <iostream> 
#include <boost/random.hpp>
#include <list>

using namespace std;

/**
 * Abstract class clock
 * Simuate RTC (Real-Time Counter) behaviour
 */
namespace BaseSimulator {

class BuildingBlock;

class ReferencePoint {
public:
  uint64_t local;
  uint64_t simulation;

  ReferencePoint(uint64_t l, uint64_t s) {local = l; simulation = s;}
  ReferencePoint(const ReferencePoint &p) {local = p.local; simulation = p.simulation;}
  ~ReferencePoint() {};
};
	
class Clock {
public:
	/**
	 * Clock Types:
	 * XMEGA_RTC_OSC1K_ULPRC : Ultra-Low Power RC Oscillator, 1 ms resolution, 30% accuracy (300 000 ppm)
	 * XMEGA_RTC_OSC1K_CRC : Calibrated RC Oscillator, 1 ms resolution, 1% accuracy (10 000 ppm) at 3V and 25°C
	 * XMEGA_RTC_OSC32K_EXT :  External 32.768kHz oscillator, 31 us resolution, 0.01% accuracy (100ppm) (FOR A TYPICAL CRYSTAL)
	 * Sources: http://www.atmel.com/images/doc8072.pdf
	 * 			http://www.atmel.com/Images/doc8047.pdf
	 * 			http://www.atmel.com/images/Atmel-8387-8-and16-bit-AVR-Microcontroller-XMEGA-A4U_Datasheet.pdf 0.5% accuracy for XMEGA_RTC_OSC1K_CRC ?
	 */ 
	enum ClockType {XMEGA_RTC_OSC1K_ULPRC = 0, XMEGA_RTC_OSC1K_CRC = 1, XMEGA_RTC_OSC32K_EXT};
	
	/**
	 * returns the current local time for the concerned block
	 */ 
	uint64_t getTime(uint64_t simTime);
	uint64_t getTime(); 
	
	uint64_t getSchedulerTimeForLocalTime(uint64_t localTime);
	
	Clock(ClockType clockType, BuildingBlock *h);
	virtual ~Clock() {};

private:
	enum Resolution {RESOLUTION_1MS = 0, RESOLUTION_1US = 1, RESOLUTION_31US = 2};
	enum Accuracy {ACCURACY_320000PPM = 320000, ACCURACY_10000PPM = 10000, ACCURACY_100PPM = 100, ACCURACY_20PPM = 20};
		
protected:
	BuildingBlock *hostBlock;
	ClockType type;
	Accuracy accuracy;
	Resolution resolution;
	double D;
	double y0;
	double x0;
	double sigma;
	boost::rand48 generator;
	list<ReferencePoint> referencePoints;

	void setClockProperties(ClockType clockType);
	uint64_t getTimeMS(uint64_t simTime);
	uint64_t getTimeUS(uint64_t simTime);

	void cleanReferencePoints();
};

}
#endif // CLOCK_H_
