/*
 *  reconfCatoms3DBlockCode.h
 *
 *  Created on: 17 October 2016
 *  Author: Thadeu
 */

#ifndef RECONFCATOMS3DBLOCKCODE_H_
#define RECONFCATOMS3DBLOCKCODE_H_

#include "catoms3DBlockCode.h"
#include "CSG/csgUtils.h"
#include "directions.h"
#include "reconf.h"
#include "neighbor/neighbor.h"
#include "sync/sync.h"

class SyncRoute;

class ReconfCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
	Scheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
    Vector3D worldPosition;

    CsgUtils csgUtils;
    Neighbor *neighbor;
    
    // Reconfiguration Variables
    Reconf *reconf;
    Sync *sync;

	ReconfCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~ReconfCatoms3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);
    void debug();
    void catomReady(MessagePtr);

	static BlockCode *buildNewBlockCode(BuildingBlock *host);
private:
    bool cellHasBlock(const Cell3DPosition &pos);
};


#endif /* RECONFCATOMS3DBLOCKCODE_H_ */
