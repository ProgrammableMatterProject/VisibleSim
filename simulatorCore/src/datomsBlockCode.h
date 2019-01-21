/*!
 * \file datomsBlockCode.h
 * \brief deformable atoms BlockCode
 * \date 28/01/2018
 * \author Benoît Piranda
 */

#ifndef DATOMSBLOCKCODE_H_
#define DATOMSBLOCKCODE_H_

#include <ostream>

#include "blockCode.h"
#include "datomsBlock.h"
#include "network.h"
#include "scheduler.h"

using namespace BaseSimulator;

namespace Datoms {

class DatomsBlock;

class DatomsBlockCode : public BaseSimulator::BlockCode {
public:

	DatomsBlockCode(DatomsBlock *host);
	virtual ~DatomsBlockCode();

//	virtual void processLocalEvent(EventPtr pev) = 0;

    void addDebugAttributes(Scheduler* scheduler);
	virtual void processLocalEvent(EventPtr pev);
	virtual void onMotionEnd() { cout << "onMotionEnd must be overloaded!" << endl; };

};

}

#endif /* DATOMSBLOCKCODE_H_ */
