/*
 *  syncNext.h
 *
 *  Created on: 29 May 2017
 *  Author: Thadeu
 */

#ifndef SYNCNEXT_H_
#define SYNCNEXT_H_

#include "sync.h"

class SyncNext : public Sync {
public:
    SyncNext(Catoms3D::Catoms3DBlock *c, Reconf *r) : Sync(c,r) {};
    ~SyncNext(){};
    void sync() override;
    void response(Cell3DPosition origin)override;
    void handleMessage(shared_ptr<Message> message) override;
    void handleMessageResponse(shared_ptr<Message> message) override;
    bool needSyncToLeft() override;
    bool needSyncToRight() override;
};

class SyncNext_message : public Sync_message {
public:
    SyncNext_message(int idx, Cell3DPosition goal, Cell3DPosition origin) : Sync_message(idx, goal, origin) {
        this->id = SYNCNEXT_MESSAGE_ID;
    }
};

class SyncNext_response_message : public Sync_response_message {
public:
    SyncNext_response_message(int idx, Cell3DPosition origin) : Sync_response_message(idx, origin) {
        this->id = SYNCNEXT_RESPONSE_MESSAGE_ID;
    }
};

#endif /* SYNCNEXT_H_ */
