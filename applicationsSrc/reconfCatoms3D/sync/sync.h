/*
 *  sync.h
 *
 *  Created on: 28 February 2017
 *  Author: Thadeu
 */

#ifndef SYNC_H_
#define SYNC_H_
#include "catoms3DBlockCode.h"
#include "syncLeft.h"
#include "syncResponse.h"
#include "../reconf.h"
#include "syncData.h"

class Sync {
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;
    SyncData *syncData;

public:
    SyncLeft *syncLeft;
    SyncResponse *syncResponse;

    Sync(Catoms3D::Catoms3DBlock *c, Reconf *r);
    ~Sync();
    void sync();
    void handleResponse(MessagePtr message);
    void handleLookupForwardMessage(MessagePtr message, Reconf*);
    void handleLookupLineMessage(MessagePtr message, Reconf*);
    bool isSyncOK() { return syncData->isSyncOK(); }
    void setSyncOK() { syncData->setSyncOK(); }
};


#endif /* SYNC_H_ */

