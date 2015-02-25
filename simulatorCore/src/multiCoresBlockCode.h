/*
 * multiCoresBlockCode.h
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#ifndef MULTICORESBLOCKCODE_H_
#define MULTICORESBLOCKCODE_H_

#include "blockCode.h"
#include "multiCoresBlock.h"

namespace MultiCores {

class MultiCoresBlock;

class MultiCoresBlockCode : public BaseSimulator::BlockCode {
public:
	MultiCoresBlockCode(MultiCoresBlock *host);
	virtual ~MultiCoresBlockCode();

	//static MultiCoresBlockCode* buildNewBlockCode(MultiCoresBlock *host);
	virtual void processLocalEvent(EventPtr pev) = 0;
};

}
#endif /* MULTICORESBLOCKCODE_H_ */
