/*
 * catoms2DBlockCode.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS2DBLOCKCODE_H_
#define CATOMS2DBLOCKCODE_H_

#include "blockCode.h"
#include "catoms2DBlock.h"

namespace Catoms2D {

class Catoms2DBlock;

class Catoms2DBlockCode : public BaseSimulator::BlockCode {
public:
	
	Catoms2DBlockCode(Catoms2DBlock *host);
	virtual ~Catoms2DBlockCode();

	Catoms2DBlockCode* buildNewBlockCode(Catoms2DBlock *host);
	virtual void processLocalEvent(EventPtr pev) = 0;

};

}

#endif /* CATOMS2DBLOCKCODE_H_ */
