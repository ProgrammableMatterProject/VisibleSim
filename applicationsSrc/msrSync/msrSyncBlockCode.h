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
#include <boost/random.hpp>
#include "color.h"
#include "network.h"
#include <vector>

class msrSyncBlockCode : public BlinkyBlocks::BlinkyBlocksBlockCode {
private:
	// globalTime = a * localTime + b
	float a;
	float b;
	int round;
	vector<pair<uint64_t,uint64_t> > syncPoints;
	vector<uint64_t> error;
	
public:
	msrSyncBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
	~msrSyncBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	Color getColor(uint64_t time);
	
	uint64_t getTime(); // estimated global time
	void synchronize(P2PNetworkInterface *exception, uint64_t globalTime);
	void adjust();
	
	static BlinkyBlocks::BlinkyBlocksBlockCode *buildNewBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
};

#endif /* MSRSYNCBLOCKCODE_H_ */
