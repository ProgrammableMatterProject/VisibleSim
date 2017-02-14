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
#define SYNC_RESPONSE_MESSAGE_ID   8003

#include <set>
#include "catoms3DBlock.h"

enum SIDE_DIRECTION { TO_LEFT, TO_RIGHT };
enum LINE_DIRECTION { TO_NEXT, TO_PREVIOUS };
enum DIRECTION {DIRECTION_UP, DIRECTION_DOWN, DIRECTION_LEFT, DIRECTION_RIGHT};

class SyncRequest {
	Catoms3D::Catoms3DBlock *catom;

    void sendMessageToNeighbor(bID requestCatomID, int requestLine, SIDE_DIRECTION side_direction);

    void sendMessage(bID requestCatomID, int requestLine, LINE_DIRECTION line_direction);

public:
    void setCatom(Catoms3D::Catoms3DBlock *c) {catom = c;}
    void syncLineSeed(bID requestCatomID, int requestLine, set<bID> lineSeeds, bID lineParent, LINE_DIRECTION lineDirection = TO_PREVIOUS);
    void syncLine(bID requestCatomID, int requestLine, set<bID> lineSeeds, bID lineParent, SIDE_DIRECTION sideDirection);
    void syncResponse(bID requestCatomID, DIRECTION);

};

class Lookup_neighbor_sync_message : public Message {
public:
    bID requestCatomID;
    int requestLine;
    SIDE_DIRECTION side_direction;

    Lookup_neighbor_sync_message(bID catomID, int line, SIDE_DIRECTION direction) : requestCatomID(catomID), requestLine(line), side_direction(direction) { id = LOOKUP_NEIGHBOR_SYNC_MESSAGE_ID; };
};

class Lookup_line_sync_message : public Message {
public:
    bID requestCatomID;
    int requestLine;
    LINE_DIRECTION lineDirection;

    Lookup_line_sync_message(bID catomID, int line, LINE_DIRECTION direction) : requestCatomID(catomID), requestLine(line), lineDirection(direction) { id = LOOKUP_LINE_SYNC_MESSAGE_ID; }
};

class Sync_response_message : public Message {
public:
    bID requestCatomID;
    Sync_response_message(bID blockId) : requestCatomID(blockId) { id = SYNC_RESPONSE_MESSAGE_ID; }
};

#endif /* SYNCREQUEST_H_ */
