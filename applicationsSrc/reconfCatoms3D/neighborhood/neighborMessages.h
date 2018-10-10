/*
 *  neighborMessages.h
 *
 *  Created on: 05 Juin 2017
 *  Author: Thadeu
 */

#ifndef NEIGHBORMESSAGES_H_
#define NEIGHBORMESSAGES_H_

#include "../reconf.h"
#include "../sync/sync.h"
#include "../sync/syncPlane.h"
#include "neighborhood.h"
#include "blockCode.h"

#define NEW_CATOM_MSG_ID	9001
#define NEW_CATOM_PARENT_MSG_ID	9002
#define NEW_CATOM_RESPONSE_MSG_ID	9003
#define NEW_CATOM_PARENT_RESPONSE_MSG_ID	9004
#define LEFT_SIDE_COMPLETED_MSG_ID	9005
#define RIGHT_SIDE_COMPLETED_MSG_ID	9006
#define PLANE_FINISHED_MSG_ID	9007
#define PLANE_FINISHED_ACK_MSG_ID	9008
#define CAN_START_NEXT_PLANE_MSG_ID	9009
#define CONFIRMATION_CAN_START_NEXT_PLANE_MSG_ID	9010

class NeighborMessages
{
private:
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;
    Neighborhood *neighborhood;
    SyncNext *syncNext;
    SyncPrevious *syncPrevious;
    SyncPlane *syncPlane;

    void sendMessageRightSideCompleted(int numberSeedsRight, bool isSeed);
    void sendMessageLeftSideCompleted(int numberSeedsLeft, bool isSeed);

public:
    static int nMessagesGetInfo;
    static int nMessagesBorderMessage;

    NeighborMessages(Catoms3D::Catoms3DBlock *catom, Reconf *reconf, Neighborhood *n, SyncNext *sn, SyncPrevious *sp, SyncPlane *sPlane);

    void init();
    void requestQueueHandler();
    void sendMessageToGetLineInfo();
    void sendMessageToGetParentInfo();

    void checkLineParent();

    void handleNewCatomMsg(MessagePtr msg);
    void handleNewCatomParentMsg(MessagePtr msg);
    void handleNewCatomResponseMsg(MessagePtr msg);
    void handleNewCatomParentResponseMsg(MessagePtr msg);
    void handleParentSeedMsg(MessagePtr msg);

    void sendMessagePlaneFinished();
    void sendMessagePlaneFinishedAck();
    void sendMessagesOnQueue(Cell3DPosition pos);
    void sendMessageCanStartNextPlane(Cell3DPosition);
    void sendMessageConfirmationCanStartNextPlane(Cell3DPosition);
    void broadcastConfirmationCanStartNextPlane(Cell3DPosition);
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
    bool createdFromPrevious;

    SyncPlane_node *syncPlaneNodeParent;
    New_catom_parent_response_message() { id = NEW_CATOM_PARENT_RESPONSE_MSG_ID;}
};
typedef shared_ptr<New_catom_parent_response_message> New_catom_parent_response_ptr;

class New_catom_response_message : public Message {
public:
    queue<MessagePtr> requestQueue;

    SyncPlane_node *syncPlaneNodeParent;
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

class Plane_finished_message : public HandleableMessage {
public:
    Plane_finished_message() { id = PLANE_FINISHED_MSG_ID; };

    void handle(BlockCode *blockCode);
    virtual Message* clone() { return new Plane_finished_message(*this); }
    virtual string getName() { return "Plane Finished"; }
};

class Plane_finished_ack_message : public HandleableMessage {
public:
    Plane_finished_ack_message() { id = PLANE_FINISHED_ACK_MSG_ID; };

    void handle(BlockCode *blockCode);

    virtual Message* clone() { return new Plane_finished_ack_message(*this); }
    virtual string getName() { return "Plane Finished Ack"; }
};

class Can_start_next_plane_message : public Message {
public:
    Cell3DPosition origin;
    Can_start_next_plane_message() { id = CAN_START_NEXT_PLANE_MSG_ID; };
};

class Confirmation_can_start_next_plane_message : public Message {
public:
    Cell3DPosition destination;
    Confirmation_can_start_next_plane_message() { id = CONFIRMATION_CAN_START_NEXT_PLANE_MSG_ID; };
};

#endif /* NEIGHBORMESSAGES_H_ */
