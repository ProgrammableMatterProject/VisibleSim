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
#include "network.h"
#include "events.h"

namespace RobotBlocks {

class RobotBlocksBlock;

class RobotBlocksBlockCode : public BaseSimulator::BlockCode {
public:
    RobotBlocksBlockCode(RobotBlocksBlock *host);
    virtual ~RobotBlocksBlockCode();
    virtual void processLocalEvent(EventPtr pev) override;
};

}

#endif /* ROBOTBLOCKSBLOCKCODE_H_ */
