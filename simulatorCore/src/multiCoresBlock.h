/*
 * multiCoresBlock.h
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#ifndef MULTICORESBLOCK_H_
#define MULTICORESBLOCK_H_

#include "buildingBlock.h"
#include "multiCoresBlockCode.h"

namespace MultiCores {

class MultiCoresBlockCode;

class MultiCoresBlock : public BaseSimulator::BuildingBlock {
public:
	MultiCoresBlockCode *(*buildNewBlockCode)(MultiCoresBlock*);

	MultiCoresBlock(int bId, MultiCoresBlockCode *(*multiCoreBlockCodeBuildingFunction)(MultiCoresBlock *));
	~MultiCoresBlock();
};

}

#endif /* MULTICORESBLOCK_H_ */
