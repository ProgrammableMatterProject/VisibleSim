/*
 *  syncRequest.h
 *
 *  Created on: 09 February 2017
 *  Author: Thadeu
 */

#ifndef SYNCREQUEST_H_
#define SYNCREQUEST_H_

#define LOOKUP_FORWARD_SYNC_MESSAGE_ID    8001
#define LOOKUP_LINE_SYNC_MESSAGE_ID   8002

#include <set>
#include "catoms3DBlock.h"
#include "../directions.h"
#include "../reconf.h"
#include "syncResponse.h"

class SyncLeft {
	Catoms3D::Catoms3DBlock *catom;
    SyncData *syncData;
    SyncResponse *syncResponse; 

    void syncSeedNext(bID requestCatomID, Cell3DPosition requestPosition, Reconf*);
    void syncSeedPrevious(bID requestCatomID, Cell3DPosition requestPosition, Reconf*);

    void sendNeighborMessage(bID requestCatomID, Cell3DPosition requestPosition, SIDE_DIRECTION side_direction, LINE_DIRECTION lineDirection);

    void sendSeedMessage(bID requestCatomID, Cell3DPosition requestPosition, LINE_DIRECTION line_direction);

public:
    SyncLeft(Catoms3D::Catoms3DBlock *c, SyncData *d, SyncResponse *s) : catom(c), syncData(d), syncResponse(s) {};
    void syncSeed(bID requestCatomID, Cell3DPosition requestPosition, Reconf*, LINE_DIRECTION lineDirection);
    void syncNeighbor(bID requestCatomID, Cell3DPosition requestPosition, Reconf*, SIDE_DIRECTION sideDirection, LINE_DIRECTION lineDirection);

};

/*
 * Send request to neighbor (x+1 or x-1)
 */
class Lookup_forward_sync_message : public Message {
public:
    bID requestCatomID;
    Cell3DPosition requestPosition;
    SIDE_DIRECTION side_direction;
    LINE_DIRECTION line_direction;

    Lookup_forward_sync_message(bID catomID, Cell3DPosition pos, SIDE_DIRECTION direction, LINE_DIRECTION ldirection) : requestCatomID(catomID), requestPosition(pos), side_direction(direction), line_direction(ldirection) { id = LOOKUP_FORWARD_SYNC_MESSAGE_ID; };
};

/*
 * Send request to another line (y+1 or y-1)
 */
class Lookup_line_sync_message : public Message {
public:
    bID requestCatomID;
    Cell3DPosition requestPosition;
    LINE_DIRECTION lineDirection;

    Lookup_line_sync_message(bID catomID, Cell3DPosition pos, LINE_DIRECTION direction) : requestCatomID(catomID), requestPosition(pos), lineDirection(direction) { id = LOOKUP_LINE_SYNC_MESSAGE_ID; }
};


#endif /* SYNCREQUEST_H_ */
