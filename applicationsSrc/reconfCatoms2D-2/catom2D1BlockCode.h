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
#include "catoms2DScheduler.h"
#include "catoms2DBlock.h"

class Catoms2D1BlockCode : public Catoms2D::Catoms2DBlockCode {
   
public:

	Scheduler *scheduler;
	Catoms2D::Catoms2DBlock *catom2D;

	Catoms2D1BlockCode (Catoms2D::Catoms2DBlock *host);
	~Catoms2D1BlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);

	static Catoms2D::Catoms2DBlockCode *buildNewBlockCode(Catoms2D::Catoms2DBlock *host);
};

#endif /* CATOM2DBLOCKCODE_H_ */
