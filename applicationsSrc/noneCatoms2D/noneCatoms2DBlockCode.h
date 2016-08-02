/*
 * catom2D1BlockCode.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef CATOM2D1BLOCKCODE_H_
#define CATOM2D1BLOCKCODE_H_

#include "catoms2DBlockCode.h"
#include "catoms2DSimulator.h"
#include "catoms2DBlock.h"

class NoneCatoms2DBlockCode : public Catoms2D::Catoms2DBlockCode {
   
public:

	Scheduler *scheduler;
	Catoms2D::Catoms2DBlock *catom2D;

	NoneCatoms2DBlockCode (Catoms2D::Catoms2DBlock *host);
	~NoneCatoms2DBlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);

	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* CATOM2DBLOCKCODE_H_ */
