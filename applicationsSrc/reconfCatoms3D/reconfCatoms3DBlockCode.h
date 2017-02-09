/*
 * reconfCatoms3DBlockCode.cpp
 *
 *  Created on: 17 October 2016
 *  Author: Thadeu
 */

#ifndef RECONFCATOMS3DBLOCKCODE_H_
#define RECONFCATOMS3DBLOCKCODE_H_

#define NEW_CATOM_MSG_ID	9001
#define NEW_CATOM_RESPONSE_MSG_ID	9002
#define RIGHT_SIDE_COMPLETED_MSG_ID	9003
#define LEFT_SIDE_COMPLETED_MSG_ID	9004
#define FIND_LINE_PARENT_SYNC_MESSAGE	9005
#define FIND_LINE_SEED_SYNC_MESSAGE	9006
#define LINE_DOWN_SYNC_MESSAGE_ID	9007
#define LINE_UP_SYNC_MESSAGE_ID	9008

#include <queue>
#include <set>
#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"
#include "csgUtils.h"
#include "target.h"
#include "seed.h"

enum RECONF_STATUS { WAITING, READY };

enum SIDE_COMPLETED { LEFT, RIGHT };
enum SIDE_DIRECTION { TO_LEFT, TO_RIGHT };
enum LINE_DIRECTION { TO_UP, TO_DOWN };

class ReconfCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
	Scheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
    Vector3D worldPosition;

    // CSG variables
    static CSGNode *csgRoot;
    static BoundingBox boundingBox;
    CsgUtils csgUtils;
    
    // Reconfiguration Variables
    static Seed *root;
    set<bID> lineSeeds;
    bID lineParent;
    bool leftCompleted, rightCompleted;
    int currentLine;
    bool isRequestHandled;

	ReconfCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~ReconfCatoms3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);
    Vector3D getWorldPosition(Cell3DPosition gridPosition);
    void createCSG();
    void sendCSGMessage();
    void addNeighbor(Cell3DPosition pos);
    void addNeighbors();
    void addNeighborsOnXAxis();
    bool isSeed();
    void tryAddNextLineNeighbor();

    void sendMessageCompletedSide(SIDE_COMPLETED side);
    void sendMessageToGetNeighborInformation();

    void requestSyncLineDown(bID requestCatomID, int requestLine, set<bID> visitedSeeds);
    void requestSyncLineUp(bID requestCatomID, int requestLine, set<bID> visitedSeeds);
    void sendMessageSyncLineDown(bID requestCatomID, int requestLine);
    void sendMessageSyncLineUp(bID requestCatomID, int requestLine);

    void sendMessageSyncLineDownFindLineParent(bID requestCatomID, int requestLine, set<bID> visitedSeeds, SIDE_DIRECTION);
    void sendMessageSyncLineUpFindLineSeeds(bID requestCatomID, int requestLine, set<bID> visitedSeeds, SIDE_DIRECTION);

	static BlockCode *buildNewBlockCode(BuildingBlock *host);
private:
    bool cellHasBlock(const Cell3DPosition &pos);
};

class New_catom_message : public Message {
public:
    New_catom_message() { id = NEW_CATOM_MSG_ID; };
};

class New_catom_response_message : public Message {
public:
    int currentLine;
    bID lineParent;
    set<bID> lineSeeds;
    bool leftCompleted, rightCompleted;
    New_catom_response_message() { id = NEW_CATOM_RESPONSE_MSG_ID; };
};

class Right_side_completed_message : public Message {
public:
    set<bID> seeds;
    Right_side_completed_message(set<bID> s) : seeds(s) { id = RIGHT_SIDE_COMPLETED_MSG_ID; };
};

class Left_side_completed_message : public Message {
public:
    set<bID> seeds;
    Left_side_completed_message(set<bID> s) : seeds(s) { id = LEFT_SIDE_COMPLETED_MSG_ID; };
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

#endif /* RECONFCATOMS3DBLOCKCODE_H_ */
