/*
 *  syncLeft.h
 *
 *  Created on: 09 February 2017
 *  Author: Thadeu
 */

#ifndef SYNCLEFT_H_
#define SYNCLEFT_H_

#define LOOKUP_NEIGHBOR_LEFT_SYNC_MESSAGE_ID    8001
#define LOOKUP_LINE_LEFT_SYNC_MESSAGE_ID   8002

#include <set>
#include "catoms3DBlock.h"
#include "../directions.h"
#include "../reconf.h"
#include "syncResponse.h"

class SyncLeft {
	Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;
    SyncResponseModel *syncResponseModel;
    SyncResponse *syncResponse;

    void syncSeedNext(SyncModel syncInfo);
    void syncSeedPrevious(SyncModel syncInfo );
    void syncNeighborNext(SyncModel syncModel, SIDE_DIRECTION sideDirection);
    void syncNeighborPrevious(SyncModel syncModel, SIDE_DIRECTION sideDirection);

    void sendNeighborMessage(SyncModel syncInfo, SIDE_DIRECTION side_direction, LINE_DIRECTION lineDirection);

    void sendSeedMessage(SyncModel syncInfo, LINE_DIRECTION line_direction);

public:
    SyncLeft(Catoms3D::Catoms3DBlock *c, Reconf *r, SyncResponseModel *d, SyncResponse *s) : catom(c), reconf(r), syncResponseModel(d), syncResponse(s) {};
    void syncSeed(SyncModel syncInfo, LINE_DIRECTION lineDirection);
    void syncNeighbor(SyncModel syncInfo, SIDE_DIRECTION sideDirection, LINE_DIRECTION lineDirection);

};

/*
 * Send request to neighbor (x+1 or x-1)
 */
class Lookup_forward_left_sync_message : public Message {
public:
    SyncModel syncInfo;
    SIDE_DIRECTION side_direction;
    LINE_DIRECTION line_direction;

    Lookup_forward_left_sync_message(SyncModel syncInfo, SIDE_DIRECTION direction, LINE_DIRECTION ldirection) : syncInfo(syncInfo), side_direction(direction), line_direction(ldirection) { id = LOOKUP_NEIGHBOR_LEFT_SYNC_MESSAGE_ID; };
};

/*
 * Send request to another line (y+1 or y-1)
 */
class Lookup_line_left_sync_message : public Message {
public:
    SyncModel syncInfo;
    LINE_DIRECTION lineDirection;

    Lookup_line_left_sync_message(SyncModel syncInfo, LINE_DIRECTION direction) : syncInfo(syncInfo), lineDirection(direction) { id = LOOKUP_LINE_LEFT_SYNC_MESSAGE_ID; }
};


#endif /* SYNCLEFT_H_*/
