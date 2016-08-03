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
	
	vector<pair<uint64_t,uint64_t> > syncPoints;
	vector<uint64_t> error;
       	ranlux48 generator; 
	uniform_int_distribution<> dis;
        
public:
	msrSyncBlockCode(BuildingBlock *host);
	~msrSyncBlockCode();

	//static void initialization();
	  
	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	Color getColor(uint64_t time);

	// Synchronized clock
	uint64_t getTime(); // estimated global time
	uint64_t getTime(uint64_t localTime); //estimated global time at local time t

	// Internal hardware clock
	uint64_t getLocalTime(bool msResolution);
	uint64_t getLocalTime(uint64_t simTime, bool msResolution);
	uint64_t getSimTime(uint64_t localTime);

	void synchronize(P2PNetworkInterface *exception, 
			 uint64_t estimatedGlobalTime,
			 uint hop);
	void adjust();

	uint getRandomUint(uint _min, uint _max);
	uint getNormalRandomUint(uint m, uint s);
	double getRandomDouble();
	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* MSRSYNCBLOCKCODE_H_ */
