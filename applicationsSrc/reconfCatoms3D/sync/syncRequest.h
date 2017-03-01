/*
 *  syncRequest.h
 *
 *  Created on: 09 February 2017
 *  Author: Thadeu
 */

#ifndef SYNCREQUEST_H_
#define SYNCREQUEST_H_

#define LOOKUP_NEIGHBOR_SYNC_MESSAGE_ID    8001
#define LOOKUP_LINE_SYNC_MESSAGE_ID   8002

#include <set>
#include "catoms3DBlock.h"
#include "../directions.h"
#include "../reconf.h"

class SyncRequest {
	Catoms3D::Catoms3DBlock *catom;

    void sendNeighborMessage(bID requestCatomID, int requestLine, SIDE_DIRECTION side_direction);

    void sendSeedMessage(bID requestCatomID, int requestLine, LINE_DIRECTION line_direction);

public:
    void setCatom(Catoms3D::Catoms3DBlock *c) {catom = c;}
    void syncLineSeedToLeft(bID requestCatomID, int requestLine, const Reconf &reconf, LINE_DIRECTION lineDirection);
    void syncLineNeighborToLeft(bID requestCatomID, int requestLine, const Reconf &reconf, SIDE_DIRECTION sideDirection);

};

/*
 * Send request to neighbor (x+1 or x-1)
 */
class Lookup_neighbor_sync_message : public Message {
public:
    bID requestCatomID;
    int requestLine;
    SIDE_DIRECTION side_direction;

    Lookup_neighbor_sync_message(bID catomID, int line, SIDE_DIRECTION direction) : requestCatomID(catomID), requestLine(line), side_direction(direction) { id = LOOKUP_NEIGHBOR_SYNC_MESSAGE_ID; };
};

/*
 * Send request to another line (y+1 or y-1)
 */
class Lookup_line_sync_message : public Message {
public:
    bID requestCatomID;
    int requestLine;
    LINE_DIRECTION lineDirection;

    Lookup_line_sync_message(bID catomID, int line, LINE_DIRECTION direction) : requestCatomID(catomID), requestLine(line), lineDirection(direction) { id = LOOKUP_LINE_SYNC_MESSAGE_ID; }
};


#endif /* SYNCREQUEST_H_ */
