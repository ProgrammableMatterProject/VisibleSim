/*
 * \file okteenBlockCode.h
 * \brief okteen Block
 * \date 12/05/2017
 * \author Benoît Piranda
 */

#ifndef OKTEENBLOCKCODE_H_
#define OKTEENBLOCKCODE_H_

#include <ostream>

#include "../../base/blockCode.h"
#include "okteenBlock.h"
#include "../../comm/network.h"
#include "../../events/scheduler.h"

using namespace BaseSimulator;

namespace Okteen {

class OkteenBlock;

class OkteenBlockCode : public BaseSimulator::BlockCode {
public:

    OkteenBlockCode(OkteenBlock *host);
    virtual ~OkteenBlockCode();

    virtual void processLocalEvent(EventPtr pev) override;

    void addDebugAttributes(Scheduler* scheduler) override;
};

}

#endif /* OKTEENBLOCKCODE_H_ */
