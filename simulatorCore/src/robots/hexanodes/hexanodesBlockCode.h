/**
 * @file   nodeBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:56:52 2019
 *
 * @brief
 *
 *
 */

#ifndef HEXANODEBLOCKCODE_H_
#define HEXANODEBLOCKCODE_H_

#include <ostream>

#include "blockCode.h"
#include "hexanodeBlock.h"
#include "network.h"
#include "scheduler.h"

using namespace BaseSimulator;

namespace Hexanode {

class HexanodeBlock;

class HexanodeBlockCode : public BaseSimulator::BlockCode {
public:

    HexanodeBlockCode(HexanodeBlock *host);
    virtual ~HexanodeBlockCode();

    virtual void processLocalEvent(EventPtr pev) override;
    virtual void onMotionEnd() override {};

    void addDebugAttributes(Scheduler* scheduler) override;
};

}

#endif /* HEXANODEBLOCKCODE_H_ */
