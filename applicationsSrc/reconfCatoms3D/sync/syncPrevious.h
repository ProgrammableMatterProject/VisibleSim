/*
 *  syncPrevious.h
 *
 *  Created on: 29 May 2017
 *  Author: Thadeu
 */

#ifndef SYNCPREVIOUS_H_
#define SYNCPREVIOUS_H_

#include "sync.h"

class SyncPrevious : public Sync {
public:
    SyncPrevious(Catoms3D::Catoms3DBlock *c, Reconf *r) : Sync(c,r) {};
    ~SyncPrevious(){};
    void sync();
    void response(Cell3DPosition origin);
    void handleMessage(shared_ptr<Message> message);
    void handleMessageResponse(shared_ptr<Message> message);
};

class SyncPrevious_message : public Sync_message {
public:
    SyncPrevious_message(int idx, Cell3DPosition goal, Cell3DPosition origin) : Sync_message(idx, goal, origin) {
        this->id = SYNCPREVIOUS_MESSAGE_ID;
    }
};

class SyncPrevious_response_message : public Sync_response_message {
public:
    SyncPrevious_response_message(int id, Cell3DPosition goal) : Sync_response_message(id, goal) {
        this->id = SYNCPREVIOUS_RESPONSE_MESSAGE_ID;
    }
};

#endif /* SYNCPREVIOUS_H_ */

