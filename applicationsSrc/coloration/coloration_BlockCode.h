/*
 * coloration_BlockCode.h
 *
 *  Created on: 21 avril 2013
 *      Author: nico
 */

#ifndef coloration_BlockCode_H_
#define coloration_BlockCode_H_

#include "smartBlocksBlockCode.h"
#include "smartBlocksSimulator.h"
#include "smartBlocksBlock.h"
#include "colors.h"

class Coloration_BlockCode : public SmartBlocks::SmartBlocksBlockCode {
	color my_color;
	bool colored;

public:
	Scheduler *scheduler;
	SmartBlocks::SmartBlocksBlock *smartBlock;

	Coloration_BlockCode( SmartBlocks::SmartBlocksBlock *host );
	~Coloration_BlockCode();

	void startup();
	void processLocalEvent( EventPtr pev );

	static BlockCode *buildNewBlockCode(BuildingBlock *host );
};
#endif /* coloration_BlockCode_H_ */
