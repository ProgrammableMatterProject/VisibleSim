/*
 *  syncCW.h
 *
 *  Created on: 29 May 2017
 *  Author: Thadeu
 */

#ifndef SYNCCCW_H_
#define SYNCCCW_H_
#include "catoms3DBlockCode.h"
#include "../reconf.h"

#define SYNCCCW_MESSAGE_ID    8201
#define SYNCCCW_RESPONSE_MESSAGE_ID    8202

class SyncCCW_message;
class SyncCCW_response_message;

class SyncCCW {
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;

public:
    SyncCCW(Catoms3D::Catoms3DBlock *c, Reconf *r);
    void sync();
    void response();
    bool canContinueLeftSeed();
    void handleMessage(shared_ptr<SyncCCW_message> message);
    void handleResponseMessage(shared_ptr<SyncCCW_response_message> message);
};

class SyncCCW_message : public Message {
public:
    int idx;
    Cell3DPosition goal;
    SyncCCW_message(int idx, Cell3DPosition goal) {
        id = SYNCCCW_MESSAGE_ID;
        this->idx = idx;
        this->goal = goal;
    }
};

class SyncCCW_response_message : public Message {
public:
    int idx;
    Cell3DPosition goal;
    SyncCCW_response_message(int idx, Cell3DPosition goal) {
        id = SYNCCCW_RESPONSE_MESSAGE_ID;
        this->idx = idx;
        this->goal = goal;
    }
};

#endif /* SYNCCCW_H_ */
