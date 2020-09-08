/**
 * @file   nodes2DBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:56:52 2019
 *
 * @brief
 *
 *
 */

#ifndef NODES2DBLOCKCODE_H_
#define NODES2DBLOCKCODE_H_

#include <ostream>

#include "../../base/blockCode.h"
#include "nodes2DBlock.h"
#include "../../comm/network.h"
#include "../../events/scheduler.h"

using namespace BaseSimulator;

namespace Nodes2D {

class Nodes2DBlock;

class Nodes2DBlockCode : public BaseSimulator::BlockCode {
public:

    Nodes2DBlockCode(Nodes2DBlock *host);
    virtual ~Nodes2DBlockCode();

    virtual void processLocalEvent(EventPtr pev) override;
    virtual void onMotionEnd() override {};

    void addDebugAttributes(Scheduler* scheduler) override;
};

}

#endif /* NODES2DBLOCKCODE_H_ */
