/*
 * smartBlocksBlockCode.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef SMARTBLOCKSBLOCKCODE_H_
#define SMARTBLOCKSBLOCKCODE_H_

#include "blockCode.h"
#include "smartBlocksBlock.h"
#include "network.h"

using namespace BaseSimulator;

namespace SmartBlocks {

class SmartBlocksBlock;

class SmartBlocksBlockCode : public BlockCode {
public:
	SmartBlocksBlockCode(SmartBlocksBlock *host) : BlockCode((BuildingBlock*)host) {};
	virtual ~SmartBlocksBlockCode() {};
};

}

#endif /* SMARTBLOCKSBLOCKCODE_H_ */
