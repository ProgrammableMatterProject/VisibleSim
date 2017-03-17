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

class SyncData {

    bool syncOK;
public:
    map<bID, SyncRoute> routes;

    SyncData() {
        syncOK = false;
    }

    void setSyncOK() {
        syncOK = true;
    }

    bool isSyncOK() {
        return syncOK;
    }
};
#endif /* SYNCDATA_H_ */
