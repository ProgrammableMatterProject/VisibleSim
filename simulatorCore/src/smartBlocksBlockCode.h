/*
 * smartBlocksBlockCode.h
 *
 *  Created on: 23 mars 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSBLOCKCODE_H_
#define SMARTBLOCKSBLOCKCODE_H_

#include "blockCode.h"
#include "smartBlocksBlock.h"
#include "network.h"
#include "events.h"

using namespace BaseSimulator;

namespace SmartBlocks {

class SmartBlocksBlock;

class SmartBlocksBlockCode : public BlockCode {
public:
	SmartBlocksBlockCode(SmartBlocksBlock *host);
	virtual ~SmartBlocksBlockCode();

	virtual void processLocalEvent(EventPtr pev);
	virtual void onMotionEnd() {};
};

}

#endif /* SMARTBLOCKSBLOCKCODE_H_ */
