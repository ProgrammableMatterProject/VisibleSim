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
#include "sync/syncNext.h"
#include "sync/syncPrevious.h"
#include "sync/syncPlane.h"
#include "sync/syncPlaneManager.h"

class ReconfCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
	Scheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;

    Neighborhood *neighborhood;
    NeighborMessages *neighborMessages;

    // Reconfiguration Variables
    Reconf *reconf;
    SyncNext *syncNext;
    SyncPrevious *syncPrevious;
    SyncPlane *syncPlane;
    SyncPlaneManager *syncPlaneManager;

	ReconfCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~ReconfCatoms3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);

    void planningRun();
    void stochasticRun();

    void syncNextMessage(shared_ptr<Sync_message> recv_message);
    void syncPreviousMessage(shared_ptr<Sync_message> recv_message);
    void syncResponse(shared_ptr<Sync_response_message> recv_message);
    void planeFinishedAck();
    void startTree();
    void tryAddNextPlane();
    void planeFinished();
    void setSeedNextPlaneCentralized();
    void planeContinue();
    void removeSeed();
    void canFill();
    void getStats();

	static BlockCode *buildNewBlockCode(BuildingBlock *host);
private:
    bool cellHasBlock(const Cell3DPosition &pos);
};

#endif /* RECONFCATOMS3DBLOCKCODE_H_ */
