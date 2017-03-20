/*
 *  syncData.h
 *
 *  Created on: 16 March 2017
 *  Author: Thadeu
 */

#ifndef SYNCDATA_H_
#define SYNCDATA_H_

#include "../directions.h"
#include <map>

class SyncResponseModel {

    bool syncOK;
public:
    map<bID, SyncRoute> routes;

    SyncResponseModel() {
        syncOK = false;
    }

    void setSyncOK() {
        syncOK = true;
    }

    bool isSyncOK() {
        return syncOK;
    }
};

class SyncModel {
public:
    bID requestCatomID;
    Cell3DPosition requestPosition;
    SyncModel(bID requestCatom, Cell3DPosition requestPos) : requestCatomID(requestCatom), requestPosition(requestPos){ };
};

#endif /* SYNCDATA_H_ */
