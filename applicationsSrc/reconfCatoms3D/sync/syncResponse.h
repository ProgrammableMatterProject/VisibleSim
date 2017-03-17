/*
 *  syncResponse.h
 *
 *  Created on: 22 February 2017
 *  Author: Thadeu
 */

#ifndef SYNCRESPONSE_H_
#define SYNCRESPONSE_H_

#define SYNC_RESPONSE_MESSAGE_ID   8101

#include "catoms3DBlock.h"
#include "../directions.h"
#include "syncData.h"

class Sync_response_message;

class SyncResponse {
	Catoms3D::Catoms3DBlock *catom;
    SyncData *syncData;

public:
    SyncResponse(Catoms3D::Catoms3DBlock *c, SyncData *d) : catom(c), syncData(d) {};
    void response(bID requestCatomID, Cell3DPosition requestPosition, DIRECTION, bool canSyncLine);
    void forwardResponse(shared_ptr<Sync_response_message> msg);
};

class Sync_response_message : public Message {
public:
    bID requestCatomID;
    Cell3DPosition requestPosition;
    bool canSyncLine; // 0 for completed but not allowed and 1 for allowed
//    bool messageFromParent; // can be from parent or from neighbor
    Sync_response_message(bID blockId, Cell3DPosition requestPosition, bool canSyncLine) : requestCatomID(blockId), requestPosition(requestPosition), canSyncLine(canSyncLine) { id = SYNC_RESPONSE_MESSAGE_ID; }
};

#endif /* SYNCRESPONSE_H_ */
