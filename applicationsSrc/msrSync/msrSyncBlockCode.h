/*
 *msrSyncBlockCode.h
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#ifndef MSRSYNCBLOCKCODE_H_
#define MSRSYNCBLOCKCODE_H_

#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"
#include <random>
#include <vector>

#include "color.h"
#include "network.h"

class msrSyncBlockCode : public BlockCode {
private:
	// globalTime = y0 * localTime + x0
        float y0; // frequency drift
	float x0; // time offset
	uint round;
	
	vector<pair<Time,Time> > syncPoints;
	vector<Time> error;
       	ranlux48 generator; 
	uniform_int_distribution<> dis;
        
public:
	msrSyncBlockCode(BuildingBlock *host);
	~msrSyncBlockCode();

	//static void initialization();
	  
	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	Color getColor(Time time);

	// Synchronized clock
	Time getTime(); // estimated global time
	Time getTime(Time localTime); //estimated global time at local time t

	// Internal hardware clock
	Time getLocalTime(bool msResolution);
	Time getLocalTime(Time simTime, bool msResolution);
	Time getSimTime(Time localTime);

	void synchronize(P2PNetworkInterface *exception, 
			 Time estimatedGlobalTime,
			 uint hop);
	void adjust();

	uint getRandomUint(uint _min, uint _max);
	uint getNormalRandomUint(uint m, uint s);
	double getRandomDouble();
	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* MSRSYNCBLOCKCODE_H_ */
