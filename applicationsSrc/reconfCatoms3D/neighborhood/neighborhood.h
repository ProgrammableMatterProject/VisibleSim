/*
 *  neighbor.h
 *
 *  Created on: 28 February 2017
 *  Author: Thadeu
 */

#ifndef NEIGHBOR_H_
#define NEIGHBOR_H_

#define NEW_CATOM_MSG_ID	9001
#define NEW_CATOM_PARENT_MSG_ID	9002
#define NEW_CATOM_RESPONSE_MSG_ID	9003
#define NEW_CATOM_PARENT_RESPONSE_MSG_ID	9004
#define LEFT_SIDE_COMPLETED_MSG_ID	9005
#define RIGHT_SIDE_COMPLETED_MSG_ID	9006

#include "cell3DPosition.h"
#include "directions.h"
#include "../reconf.h"
#include "../sync/sync.h"
#include "../syncCCW/syncCCW.h"

class Neighborhood {
private:
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;
    Sync *sync;
    SyncCCW *syncCCW;
    BlockCodeBuilder blockCodeBuilder;
    bool leftCompleted;
    bool rightCompleted;
    void addNeighbor(Cell3DPosition pos);
    void sendMessageRightSideCompleted(int numberSeedsRight, bool isSeed);
    void sendMessageLeftSideCompleted(int numberSeedsLeft, bool isSeed);

    void checkLineCompleted();
    void addNeighborToLeft();
    void addNeighborToRight();

public:
    Neighborhood(Catoms3D::Catoms3DBlock *catom, Reconf *reconf, Sync *s, SyncCCW *sccw, BlockCodeBuilder blockCodeBuilder);

    void addNeighbors();
    void addNeighborsWithoutSync();
    void addNextLineNeighbor();
    void addAllNeighbors();

    void sendMessageToGetLineInfo();
    void sendMessageToGetParentInfo();
    void setLeftCompleted();
    void setRightCompleted();
    bool isLeftCompleted() { return leftCompleted; }
    bool isRightCompleted() { return rightCompleted; }
    bool isOnLeftBorder();
    bool isOnRightBorder();
    bool isFirstCatomOfLine();
    void init();
    void requestQueueHandler();

    void handleNewCatomMsg(MessagePtr msg);
    void handleNewCatomParentMsg(MessagePtr msg);
    void handleNewCatomResponseMsg(MessagePtr msg);
    void handleNewCatomParentResponseMsg(MessagePtr msg);
    void handleParentSeedMsg(MessagePtr msg);
    void handleLeftSideCompletedMsg(MessagePtr msg);
    void handleRightSideCompletedMsg(MessagePtr msg);
};

class New_catom_message : public Message {
public:
    SIDE_DIRECTION lineParentDirection;
    New_catom_message() { id = NEW_CATOM_MSG_ID; };
};
typedef shared_ptr<New_catom_message> New_catom_ptr;

class New_catom_parent_message : public Message {
public:
    New_catom_parent_message() { id = NEW_CATOM_PARENT_MSG_ID; };
};

class New_catom_parent_response_message : public Message {
public:
    queue<MessagePtr> requestQueue;
    New_catom_parent_response_message() { id = NEW_CATOM_PARENT_RESPONSE_MSG_ID;}
};
typedef shared_ptr<New_catom_parent_response_message> New_catom_parent_response_ptr;

class New_catom_response_message : public Message {
public:
    bID lineParent;
    bool leftCompleted, rightCompleted;
    int numberSeedsLeft, numberSeedsRight;
    SIDE_DIRECTION lineParentDirection;
    queue<MessagePtr> requestQueue;
    New_catom_response_message();
};
typedef shared_ptr<New_catom_response_message> New_catom_response_ptr;

class Left_side_completed_message : public Message {
public:
    int numberSeedsLeft;
    Left_side_completed_message(int nSeedsLeft) : numberSeedsLeft(nSeedsLeft) { id = LEFT_SIDE_COMPLETED_MSG_ID; };
};
typedef shared_ptr<Left_side_completed_message> Left_side_completed_ptr;

class Right_side_completed_message : public Message {
public:
    int numberSeedsRight;
    Right_side_completed_message(int nSeedsRight) : numberSeedsRight(nSeedsRight) { id = RIGHT_SIDE_COMPLETED_MSG_ID; };
};
typedef shared_ptr<Right_side_completed_message> Right_side_completed_ptr;
#endif /* NEIGHBOR_H_ */
