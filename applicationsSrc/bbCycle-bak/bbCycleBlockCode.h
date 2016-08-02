/*
 *bbCycleBlockCode.h
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#ifndef BBCYCLEBLOCKCODE_H_
#define BBCYCLEBLOCKCODE_H_

#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"

#include "color.h"

class BbCycleBlockCode : public BlinkyBlocks::BlinkyBlocksBlockCode {
public:
	BbCycleBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
	~BbCycleBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	Color getColor(uint64_t time);
	
	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* BBCYCLEBLOCKCODE_H_ */
