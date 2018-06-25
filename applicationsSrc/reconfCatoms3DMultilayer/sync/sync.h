/*
 *  sync.h
 *
 *  Created on: 15 Juin 2017
 *  Author: Thadeu
 */

#ifndef SYNC_H_
#define SYNC_H_

#include "catoms3DBlockCode.h"
#include "../reconf.h"

#define SYNCNEXT_MESSAGE_ID    8201
#define SYNCNEXT_RESPONSE_MESSAGE_ID    8202
#define SYNCPREVIOUS_MESSAGE_ID    8203
#define SYNCPREVIOUS_RESPONSE_MESSAGE_ID    8204

class Sync {
private:
    int getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos);

protected:
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;
    vector<pair<int, int>> ccw_order = {{0,-1}, {1,0}, {0,1}, {-1,0}};
    vector<pair<int, int>> cw_order = {{0,-1}, {-1,0}, {0,1}, {1,0}};

public:
    static int nMessagesSync;
    static int nMessagesSyncResponse;
    Sync(Catoms3D::Catoms3DBlock *c, Reconf *r);
    virtual ~Sync();

    virtual void sync() = 0;
    virtual void response(Cell3DPosition) = 0;
    virtual void handleMessage(shared_ptr<Message>) = 0;
    virtual void handleMessageResponse(shared_ptr<Message>) = 0;
    virtual bool needSyncToLeft() = 0;
    virtual bool needSyncToRight() = 0;

    bool isInternalBorder(int idx);
};

class Sync_message : public Message {
public:
    int idx;
    Cell3DPosition goal;
    Cell3DPosition origin;
    Sync_message(int idx, Cell3DPosition goal, Cell3DPosition origin) {
        this->idx = idx;
        this->goal = goal;
        this->origin = origin;
    }
};

class Sync_response_message : public Message {
public:
    int idx;
    Cell3DPosition origin;
    Sync_response_message(int idx, Cell3DPosition origin) {
        this->idx = idx;
        this->origin = origin;
    }
};

#endif /* SYNC_H_ */
