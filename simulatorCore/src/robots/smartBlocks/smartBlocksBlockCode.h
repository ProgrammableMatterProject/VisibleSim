/*
 * smartBlocksBlockCode.h
 *
 *  Created on: 23 mars 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSBLOCKCODE_H_
#define SMARTBLOCKSBLOCKCODE_H_

#include "../../base/blockCode.h"
#include "smartBlocksBlock.h"
#include "../../comm/network.h"
#include "../../events/events.h"

using namespace BaseSimulator;

namespace SmartBlocks {

class SmartBlocksBlock;

class SmartBlocksBlockCode : public BlockCode {
public:
    SmartBlocksBlockCode(SmartBlocksBlock *host);
    virtual ~SmartBlocksBlockCode();

    virtual void processLocalEvent(EventPtr pev) override;
};

}

#endif /* SMARTBLOCKSBLOCKCODE_H_ */
