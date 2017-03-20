/*
 *  sync.h
 *
 *  Created on: 28 February 2017
 *  Author: Thadeu
 */

#ifndef SYNC_H_
#define SYNC_H_
#include "catoms3DBlockCode.h"
#include "../reconf.h"
#include "syncLeft.h"
#include "syncResponse.h"
#include "syncModel.h"

class Sync {
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;
    SyncResponseModel *syncResponseModel;

public:
    SyncLeft *syncLeft;
    SyncResponse *syncResponse;

    Sync(Catoms3D::Catoms3DBlock *c, Reconf *r);
    ~Sync();
    void sync();
    void handleResponse(MessagePtr message);
    void handleLookupForwardMessage(MessagePtr message);
    void handleLookupLineMessage(MessagePtr message);
    bool isSyncOK() { return syncResponseModel->isSyncOK(); }
    void setSyncOK() { syncResponseModel->setSyncOK(); }
};


#endif /* SYNC_H_ */

