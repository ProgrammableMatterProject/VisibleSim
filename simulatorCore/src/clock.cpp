#include "clock.h"
#include "scheduler.h"

namespace BaseSimulator {

Clock::Clock(ClockType clockType, BuildingBlock *b) {
	hostBlock = b;
	setClockProperties(clockType);
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

LinearDriftClock::LinearDriftClock(Clock::ClockType clockType, BuildingBlock *b) : Clock(clockType,b) {
	double accuracyPercentage = 0;
	double maxA = 0;
	double minA = 0;
	
	accuracyPercentage = (float)accuracy/pow(10,6); // ppm to %
	maxA = 1 + accuracyPercentage;
	minA = 1 - accuracyPercentage;
	
	startTime = BaseSimulator::getScheduler()->now();
	generator =  boost::rand48(hostBlock->blockId);
	a = (generator()/(double)RAND_MAX)*(maxA-minA) + minA;
	b = 0;
}

uint64_t LinearDriftClock::getTimeMS() {
	return getTimeUS()/1000;
}

uint64_t LinearDriftClock::getTimeUS() {
	double localTime = 0;
	double realTime = (double)BaseSimulator::getScheduler()->now();
	double noise = 0;
	
	localTime = a*(realTime-startTime) + b + noise;
	return max((uint64_t)0,(uint64_t)localTime);
}
	
uint64_t LinearDriftClock::getSchedulerTimeForLocalTime(uint64_t localTime) {
	double noise = 0;
	return ((double)localTime - b - noise)/a + startTime;
}
}
