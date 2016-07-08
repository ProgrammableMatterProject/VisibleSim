/*
 * robotBlocksBlockCode.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef ROBOTBLOCKSBLOCKCODE_H_
#define ROBOTBLOCKSBLOCKCODE_H_

#include "blockCode.h"
#include "robotBlocksBlock.h"

namespace RobotBlocks {

class RobotBlocksBlock;

class RobotBlocksBlockCode : public BaseSimulator::BlockCode {
public:	
	RobotBlocksBlockCode(RobotBlocksBlock *host);
	virtual ~RobotBlocksBlockCode();

	virtual void processLocalEvent(EventPtr pev) = 0;
};

}

#endif /* ROBOTBLOCKSBLOCKCODE_H_ */
