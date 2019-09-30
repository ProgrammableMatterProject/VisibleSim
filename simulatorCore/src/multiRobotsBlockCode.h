/*
 * @file multiRobotsBlockCode.h
 *
 *  Created on: 14/07/2016
 *      Author: pthalamy
 */

#ifndef MULTIROBOTSBLOCKCODE_H_
#define MULTIROBOTSBLOCKCODE_H_

#include "blockCode.h"
#include "multiRobotsBlock.h"

namespace MultiRobots {

class MultiRobotsBlock;

class MultiRobotsBlockCode : public BaseSimulator::BlockCode {
public:

    MultiRobotsBlockCode(MultiRobotsBlock *host);
    virtual ~MultiRobotsBlockCode();

    virtual void init() override {}

    virtual void processLocalEvent(EventPtr pev) override = 0;
};

}

#endif /* MULTIROBOTSBLOCKCODE_H_ */
