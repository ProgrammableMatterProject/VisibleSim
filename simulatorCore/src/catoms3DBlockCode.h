/*
 * catoms3DBlockCode.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS3DBLOCKCODE_H_
#define CATOMS3DBLOCKCODE_H_

#include "blockCode.h"
#include "catoms3DBlock.h"

namespace Catoms3D {

class Catoms3DBlock;

class Catoms3DBlockCode : public BaseSimulator::BlockCode {
public:
	
	Catoms3DBlockCode(Catoms3DBlock *host);
	virtual ~Catoms3DBlockCode();

	Catoms3DBlockCode* buildNewBlockCode(Catoms3DBlock *host);
	virtual void processLocalEvent(EventPtr pev) = 0;

};

}

#endif /* CATOMS3DBLOCKCODE_H_ */
