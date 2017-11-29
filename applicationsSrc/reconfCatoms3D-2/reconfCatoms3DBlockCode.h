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

class ReconfCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
	Scheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
    Vector3D worldPosition;

    Neighborhood *neighborhood;
    NeighborMessages *neighborMessages;

    // Reconfiguration Variables
    Reconf *reconf;

	ReconfCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~ReconfCatoms3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);

    void planningRun();
    void stochasticRun();

    void getStats();

	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* RECONFCATOMS3DBLOCKCODE_H_ */
