/**
 * @file   nodeBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:56:52 2019
 *
 * @brief
 *
 *
 */

#ifndef HEXANODESBLOCKCODE_H_
#define HEXANODESBLOCKCODE_H_

#include <ostream>

#include "base/blockCode.h"
#include "robots/hexanodes/hexanodesBlock.h"
#include "comm/network.h"
#include "events/scheduler.h"

using namespace BaseSimulator;

namespace Hexanodes {

class HexanodesBlock;

class HexanodesBlockCode : public BaseSimulator::BlockCode {
public:

    HexanodesBlockCode(HexanodesBlock *host);
    virtual ~HexanodesBlockCode();

    virtual void processLocalEvent(EventPtr pev) override;
    virtual void onMotionEnd() override {};

    void addDebugAttributes(Scheduler* scheduler) override;
};

}

#endif /* HEXANODESBLOCKCODE_H_ */
