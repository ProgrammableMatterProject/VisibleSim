/*
 *  syncResponse.h
 *
 *  Created on: 22 February 2017
 *  Author: Thadeu
 */

#ifndef SYNCRESPONSE_H_
#define SYNCRESPONSE_H_

#define SYNC_RESPONSE_MESSAGE_ID   8101

#include "robots/catoms3D/catoms3DBlock.h"
#include "../directions.h"
#include "syncModel.h"

class Sync_response_message;

class SyncResponse {
    Catoms3D::Catoms3DBlock *catom;
    SyncResponseModel *syncResponseModel;

public:
    SyncResponse(Catoms3D::Catoms3DBlock *c, SyncResponseModel *d) : catom(c), syncResponseModel(d) {};
    void response(SyncModel syncModel, DIRECTION, bool canSyncLine, bool toLeft, bool finishRequest);
    void forwardResponse(shared_ptr<Sync_response_message> msg);
};

class Sync_response_message : public Message {
public:
    SyncModel syncModel;
    bool canSyncLine; // 0 for completed but not allowed and 1 for allowed
    bool toLeft;
    bool finishRequest;
    Sync_response_message(SyncModel syncModel, bool canSyncLine, bool toLeft, bool finishRequest) : syncModel(syncModel), canSyncLine(canSyncLine), toLeft(toLeft), finishRequest(finishRequest){ id = SYNC_RESPONSE_MESSAGE_ID; }
};

#endif /* SYNCRESPONSE_H_ */
