#include "clock.h"
#include "scheduler.h"

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
	switch (resolution) {
		case RESOLUTION_1US:
			return getTimeUS();
		break;
		case RESOLUTION_31US: {
			uint64_t time = getTimeUS();
			return floor(time/31) * 31;
		}			
		break;
		case RESOLUTION_1MS:
			return getTimeMS()*1000;
		break;
		default:
			cerr << "Undefined clock resolution" << endl;
			return 0;
	}
}


LinearDriftClock::LinearDriftClock(Clock::ClockType clockType, int seed) {
	double accuracyPercentage = 0;
	double maxDrift = 0;
	double minDrift = 0;
	
	setClockProperties(clockType);
	
	accuracyPercentage = (float)accuracy/pow(10,6); // ppm to %
	maxDrift = 1 + accuracyPercentage;
	minDrift = 1 - accuracyPercentage;
	
	startTime = BaseSimulator::getScheduler()->now();
	generator =  boost::rand48(seed);
	driftFactor = (generator()/(double)RAND_MAX)*(maxDrift-minDrift) + minDrift;
	
	lastLocalTimeRead = 0;
}

uint64_t LinearDriftClock::getTimeMS() {
	return getTimeMS()/1000;
}

uint64_t LinearDriftClock::getTimeUS() {
	double localTime = 0;
	double realTime = (double)BaseSimulator::getScheduler()->now();

	
	/* see how to add a random offset
	 * double accuracyPercentage = (float)accuracy/pow(10,-6); // ppm to %
	 * int sign = 1;
	 * if (generator()%2) {
		sign = -1;
	}*/
	
	/*do {
		//double offset = sign * generator() % ;
		double offset = 0;
		localTime = (driftFactor*realTime+offset) - startTime;
	} while ((abs(localTime/realTime-1) > accuracyPercentage) && (localTime < lastLocalTimeRead));
	lastLocalTimeRead = localTime;
	*/
	
	localTime = driftFactor*(realTime-startTime);
	return localTime;
}
	
uint64_t LinearDriftClock::getSchedulerTimeForLocalTime(uint64_t localTime) {
	return (double)localTime/driftFactor + startTime;
}
