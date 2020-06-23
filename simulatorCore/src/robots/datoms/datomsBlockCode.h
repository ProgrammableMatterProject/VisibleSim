/*!
 * \file datomsBlockCode.h
 * \brief deformable atoms BlockCode
 * \date 28/01/2018
 * \author Benoît Piranda
 */

#ifndef DATOMSBLOCKCODE_H_
#define DATOMSBLOCKCODE_H_

#include <ostream>

#include "../../base/blockCode.h"
#include "datomsBlock.h"
#include "../../comm/network.h"
#include "../../events/scheduler.h"

using namespace BaseSimulator;

namespace Datoms {

class DatomsBlock;

class DatomsBlockCode : public BaseSimulator::BlockCode {
public:

    DatomsBlockCode(DatomsBlock *host);
    virtual ~DatomsBlockCode();

//	virtual void processLocalEvent(EventPtr pev) = 0;

    void addDebugAttributes(Scheduler* scheduler) override;
    virtual void processLocalEvent(EventPtr pev) override;

};

}

#endif /* DATOMSBLOCKCODE_H_ */
