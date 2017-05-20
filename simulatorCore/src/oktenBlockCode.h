/*
 * \file oktenBlockCode.h
 * \brief okten Block
 * \date 12/05/2017
 * \author Benoît Piranda
 */

#ifndef OKTENBLOCKCODE_H_
#define OKTENBLOCKCODE_H_

#include <ostream>

#include "blockCode.h"
#include "oktenBlock.h"
#include "network.h"
#include "scheduler.h"

using namespace BaseSimulator;

namespace Okten {

class OktenBlock;

class OktenBlockCode : public BaseSimulator::BlockCode {
public:

	OktenBlockCode(OktenBlock *host);
	virtual ~OktenBlockCode();

	virtual void processLocalEvent(EventPtr pev);
	virtual void onMotionEnd() {};

    void addDebugAttributes(Scheduler* scheduler);
};

}

#endif /* OKTENBLOCKCODE_H_ */
