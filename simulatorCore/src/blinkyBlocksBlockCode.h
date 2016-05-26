/*
 * blinkyBlocksBlockCode.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef BLINKYBLOCKSBLOCKCODE_H_
#define BLINKYBLOCKSBLOCKCODE_H_

#include "blockCode.h"
#include "blinkyBlocksBlock.h"

namespace BlinkyBlocks {

class BlinkyBlocksBlock;

class BlinkyBlocksBlockCode : public BaseSimulator::BlockCode {
public:
	
	BlinkyBlocksBlockCode(BlinkyBlocksBlock *host);
	virtual ~BlinkyBlocksBlockCode();

	virtual void handleCommand(VMCommand &command) {};
	virtual void handleDeterministicMode(VMCommand &command) {} ;
	virtual void init() {};
	
	//static BlinkyBlocksBlockCode* buildNewBlockCode(BlinkyBlocksBlock *host);
	virtual void processLocalEvent(EventPtr pev) = 0;
};

}

#endif /* BLINKYBLOCKSBLOCKCODE_H_ */
