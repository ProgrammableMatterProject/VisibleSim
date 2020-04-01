/*
 *  syncPlaneManager.h
 *
 *  Created on: 30 August 2017
 *  Author: Thadeu
 */

#ifndef SYNCPLANEMANAGER_H_
#define SYNCPLANEMANAGER_H_

#include "robots/catoms3D/catoms3DBlockCode.h"
#include "../reconf.h"
#include "syncPlane.h"
#include "../neighborhood/neighborhood.h"
#include "../neighborhood/neighborMessages.h"
#include <vector>

class SyncPlaneManager {
private:

protected:
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;
    SyncPlane *syncPlane;
    NeighborMessages *neighborMessages;
    Neighborhood *neighborhood;

public:
    SyncPlaneManager(Catoms3D::Catoms3DBlock *c, Reconf *r, SyncPlane *sp, Neighborhood *neighborhood, NeighborMessages *nb);
    ~SyncPlaneManager();

    void planeFinished();
    void planeFinishedAck();
    void tryAddNextPlane();
    void setSeedNextPlaneCentralized();
};

#endif /* SYNCPLANEMANAGER_H_ */
