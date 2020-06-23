/*
 * slidingCubesBlockCode.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef SLIDINGCUBESBLOCKCODE_H_
#define SLIDINGCUBESBLOCKCODE_H_

#include "../../base/blockCode.h"
#include "slidingCubesBlock.h"
#include "../../comm/network.h"
#include "../../events/events.h"

namespace SlidingCubes {

class SlidingCubesBlock;

class SlidingCubesBlockCode : public BaseSimulator::BlockCode {
public:
    SlidingCubesBlockCode(SlidingCubesBlock *host);
    virtual ~SlidingCubesBlockCode();
    virtual void processLocalEvent(EventPtr pev) override;
};

}

#endif /* SLIDINGCUBESBLOCKCODE_H_ */
