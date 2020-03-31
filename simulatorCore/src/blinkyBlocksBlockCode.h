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
#include "network.h"
#include "events.h"

namespace BlinkyBlocks {

class BlinkyBlocksBlock;

class BlinkyBlocksBlockCode : public BaseSimulator::BlockCode {
public:

    BlinkyBlocksBlockCode(BlinkyBlocksBlock *host);
    virtual ~BlinkyBlocksBlockCode() override;

    virtual void processLocalEvent(EventPtr pev) override {
        // Do not remove, used by sample blockcode
        BlockCode::processLocalEvent(pev);
    }
};

}

#endif /* BLINKYBLOCKSBLOCKCODE_H_ */
