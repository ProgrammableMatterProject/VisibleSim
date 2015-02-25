/*
 * smartBlocksBlockCode.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef SMARTBLOCKSBLOCKCODE_H_
#define SMARTBLOCKSBLOCKCODE_H_

#include "blockCode.h"
#include "smartBlocksBlock.h"

namespace SmartBlocks {

class SmartBlocksBlock;

class SmartBlocksBlockCode : public BaseSimulator::BlockCode {
public:
	SmartBlocksBlockCode(SmartBlocksBlock *host);
	virtual ~SmartBlocksBlockCode();

	//static SmartBlocksBlockCode* buildNewBlockCode(SmartBlocksBlock *host);
	virtual void processLocalEvent(EventPtr pev) = 0;
	SmartBlocksBlockCode* buildNewBlockCode(SmartBlocksBlock *host);
};

}

#endif /* SMARTBLOCKSBLOCKCODE_H_ */
