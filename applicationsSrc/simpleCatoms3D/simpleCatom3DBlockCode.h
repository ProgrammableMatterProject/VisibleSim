/*
 * simpleCatom3DBlockCode.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef SIMPLECATOM3DBLOCKCODE_H_
#define SIMPLECATOM3DBLOCKCODE_H_

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DScheduler.h"
#include "catoms3DEvents.h"
#include "catoms3DBlock.h"

class SimpleCatom3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
	Catoms3D::Catoms3DScheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;

    double potentiel;

	SimpleCatom3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~SimpleCatom3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);
	/* virtual bool getAttribute(const string &att,ostringstream &sout); */

	static Catoms3D::Catoms3DBlockCode *buildNewBlockCode(Catoms3D::Catoms3DBlock *host);

};


#endif /* SIMPLECATOM3DBLOCKCODE_H_ */
