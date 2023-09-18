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

#include "../../base/blockCode.h"
#include "hexanodesBlock.h"
#include "../../comm/network.h"
#include "../../events/scheduler.h"


using namespace BaseSimulator;

namespace Hexanodes {

class HexanodesBlock;

class HexanodesBlockCode : public BaseSimulator::BlockCode {
public:

    explicit HexanodesBlockCode(HexanodesBlock *host);
    ~HexanodesBlockCode() override;

    void processLocalEvent(EventPtr pev) override;
    void onMotionEnd() override {};

    bool canMove(Hexanodes::motionDirection dir);
    void moveTo(Hexanodes::motionDirection dir,uint32_t delay=200000);

    void addDebugAttributes(Scheduler* scheduler) override;
};

}

#endif /* HEXANODESBLOCKCODE_H_ */
