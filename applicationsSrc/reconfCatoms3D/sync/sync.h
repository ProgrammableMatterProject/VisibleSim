/*
 *  sync.h
 *
 *  Created on: 28 February 2017
 *  Author: Thadeu
 */

#ifndef SYNC_H_
#define SYNC_H_
#include "catoms3DBlockCode.h"
#include "syncRequest.h"
#include "syncResponse.h"
#include "../reconf.h"

class Sync {
    Catoms3D::Catoms3DBlock *catom;

    SyncRequest *syncRequest;
    SyncResponse *syncResponse;
    map<bID, SyncRoute> syncRoutes;

public:
    Sync(Catoms3D::Catoms3DBlock *c);
    void handleLookupNeighborMessage(MessagePtr message, Reconf*);
    void handleLookupLineMessage(MessagePtr message, Reconf*);
};


#endif /* SYNC_H_ */

