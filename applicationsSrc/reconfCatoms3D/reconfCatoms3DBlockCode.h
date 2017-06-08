/*
 *  reconfCatoms3DBlockCode.h
 *
 *  Created on: 17 October 2016
 *  Author: Thadeu
 */

#ifndef RECONFCATOMS3DBLOCKCODE_H_
#define RECONFCATOMS3DBLOCKCODE_H_

#include "catoms3DBlockCode.h"
#include "directions.h"
#include "reconf.h"
#include "neighborhood/neighborhood.h"
#include "neighborhood/neighborMessages.h"
#include "sync/sync.h"
#include "syncCCW/syncCCW.h"

class ReconfCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
	Scheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
    Vector3D worldPosition;

    Neighborhood *neighborhood;
    NeighborMessages *neighborMessages;

    // Reconfiguration Variables
    Reconf *reconf;
    Sync *sync;
    SyncCCW *syncCCW;

	ReconfCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~ReconfCatoms3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);

	static BlockCode *buildNewBlockCode(BuildingBlock *host);
private:
    bool cellHasBlock(const Cell3DPosition &pos);
};


#endif /* RECONFCATOMS3DBLOCKCODE_H_ */
