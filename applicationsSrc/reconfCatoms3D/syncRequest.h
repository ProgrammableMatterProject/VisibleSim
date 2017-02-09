/*
 *  syncResponse.h
 *
 *  Created on: 09 February 2017
 *  Author: Thadeu
 */

#ifndef SYNCREQUEST_H_
#define SYNCREQUEST_H_

#define FIND_LINE_PARENT_SYNC_MESSAGE	8001
#define FIND_LINE_SEED_SYNC_MESSAGE	    8002
#define LINE_DOWN_SYNC_MESSAGE_ID	    8003
#define LINE_UP_SYNC_MESSAGE_ID	        8004

#include <set>
#include "catoms3DBlock.h"

enum SIDE_DIRECTION { TO_LEFT, TO_RIGHT };
enum LINE_DIRECTION { TO_UP, TO_DOWN };

class SyncRequest {
	Catoms3D::Catoms3DBlock *catom;
    bool isRequestHandled;

public:
    SyncRequest() { isRequestHandled = false; }
    void setCatom(Catoms3D::Catoms3DBlock *c) {catom = c;}

    void requestSyncLineDown(bID requestCatomID, int requestLine, set<bID> visitedSeeds, set<bID> lineSeeds, bID lineParent);

    void requestSyncLineUp(bID requestCatomID, int requestLine, set<bID> visitedSeeds, set<bID> lineSeeds, bID lineParent);

    void sendMessageSyncLineDownFindLineParent(bID requestCatomID, int requestLine, set<bID> visitedSeeds, SIDE_DIRECTION side_direction);

    void sendMessageSyncLineUpFindLineSeeds(bID requestCatomID, int requestLine, set<bID> visitedSeeds, SIDE_DIRECTION side_direction);

    void sendMessageSyncLineDown(bID requestCatomID, int requestLine);

    void sendMessageSyncLineUp(bID requestCatomID, int requestLine);

};

class Find_line_parent_sync_message : public Message {
public:
    bID requestCatomID;
    int requestLine;
    set<bID> visitedSeeds;
    LINE_DIRECTION direction;

    Find_line_parent_sync_message(bID catomID, int line, set<bID> visitedSeeds, LINE_DIRECTION direction) : requestCatomID(catomID), requestLine(line), visitedSeeds(visitedSeeds), direction(direction) { id = FIND_LINE_PARENT_SYNC_MESSAGE; };
};

class Find_line_seed_sync_message : public Message {
public:
    bID requestCatomID;
    int requestLine;
    set<bID> visitedSeeds;
    LINE_DIRECTION direction;

    Find_line_seed_sync_message(bID catomID, int line, set<bID> visitedSeeds, LINE_DIRECTION direction) : requestCatomID(catomID), requestLine(line), visitedSeeds(visitedSeeds), direction(direction) { id = FIND_LINE_PARENT_SYNC_MESSAGE; };
};

class Line_down_sync_message : public Message {
public:
    bID requestCatomID;
    int requestLine;

    Line_down_sync_message(bID catomID, int line) : requestCatomID(catomID), requestLine(line)  { id = LINE_DOWN_SYNC_MESSAGE_ID; };

};

class Line_up_sync_message : public Message {
public:
    bID requestCatomID;
    int requestLine;

    Line_up_sync_message(bID catomID, int line) : requestCatomID(catomID), requestLine(line)  { id = LINE_UP_SYNC_MESSAGE_ID; };

};

#endif /* SYNCREQUEST_H_ */
