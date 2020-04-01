/**
 * @file   nodeBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:56:52 2019
 *
 * @brief
 *
 *
 */

#ifndef NODEBLOCKCODE_H_
#define NODEBLOCKCODE_H_

#include <ostream>

#include "blockCode.h"
#include "nodeBlock.h"
#include "network.h"
#include "scheduler.h"

using namespace BaseSimulator;

namespace Node {

class NodeBlock;

class NodeBlockCode : public BaseSimulator::BlockCode {
public:

    NodeBlockCode(NodeBlock *host);
    virtual ~NodeBlockCode();

    virtual void processLocalEvent(EventPtr pev) override;
    virtual void onMotionEnd() override {};

    void addDebugAttributes(Scheduler* scheduler) override;
};

}

#endif /* NODEBLOCKCODE_H_ */
