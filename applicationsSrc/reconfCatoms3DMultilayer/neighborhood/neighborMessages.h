/*
 *
 *  neighborMessages.h
 *
 *  Created on: 05 Juin 2017
 *  Author: Thadeu
 */

#ifndef NEIGHBORMESSAGES_H_
#define NEIGHBORMESSAGES_H_

#include "../reconf.h"
#include "neighborhood.h"

#define NEW_CATOM_MSG_ID	9001
#define NEW_CATOM_LINE_PARENT_MSG_ID	9002
#define NEW_CATOM_PLANE_PARENT_MSG_ID	9003

#define NEW_CATOM_RESPONSE_MSG_ID	9004
#define NEW_CATOM_LINE_PARENT_RESPONSE_MSG_ID	9005
#define NEW_CATOM_PLANE_PARENT_RESPONSE_MSG_ID	9006

#define PLANE_FINISHED_MSG_ID 9007
#define PLANE_FINISHED_ACK_MSG_ID 9008

#define PARENT_PLANE_FINISHED_MSG_ID 9009

class NeighborMessages
{
private:
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;
    Neighborhood *neighborhood;

public:
    static int nMessagesGetInfo;
    static int nMessagesBorderMessage;

    NeighborMessages(Catoms3D::Catoms3DBlock *catom, Reconf *reconf, Neighborhood *n);

    void init();
    void requestQueueHandler();
    void sendMessageToGetParentInfo();
    void sendMessageToGetLineParentInfo();
    void sendMessageToGetPlaneParentInfo();

    void checkLineParent();

    void handleNewCatomMsg(MessagePtr msg);
    void handleNewCatomLineParentMsg(MessagePtr msg);
    void handleNewCatomPlaneParentMsg(MessagePtr msg);
    void handleNewCatomResponseMsg(MessagePtr msg);
    void handleNewCatomLineParentResponseMsg(MessagePtr msg);
    void handleNewCatomPlaneParentResponseMsg(MessagePtr msg);
    //void handleParentSeedMsg(MessagePtr msg);

    void sendMessagesOnQueue(Cell3DPosition pos);

    void sendMessagePlaneFinished();
    void sendMessagePlaneFinishedAck();
    void sendMessageParentPlaneFinished(Cell3DPosition direction);
    void broadcastMessageParentPlaneFinished();
};

class New_catom_message : public Message {
public:
    SIDE_DIRECTION lineParentDirection;
    New_catom_message() { id = NEW_CATOM_MSG_ID; };
};

class New_catom_line_parent_message : public Message {
public:
    New_catom_line_parent_message() { id = NEW_CATOM_LINE_PARENT_MSG_ID; };
};

class New_catom_plane_parent_message : public Message {
public:
    New_catom_plane_parent_message() { id = NEW_CATOM_PLANE_PARENT_MSG_ID; };
};

class New_catom_response_message : public Message {
public:
    //queue<MessagePtr> requestQueue;
    int floor;

    New_catom_response_message() { id = NEW_CATOM_RESPONSE_MSG_ID; };
};

class New_catom_line_parent_response_message : public Message {
public:
    //queue<MessagePtr> requestQueue;
    bool createdFromPrevious;
    int floor;

    New_catom_line_parent_response_message() { id = NEW_CATOM_LINE_PARENT_RESPONSE_MSG_ID;}
};

class New_catom_plane_parent_response_message : public Message {
public:
    int floor;

    New_catom_plane_parent_response_message() { id = NEW_CATOM_PLANE_PARENT_RESPONSE_MSG_ID;}
};

class Plane_finished_message : public Message {
public:
    Plane_finished_message() { id = PLANE_FINISHED_MSG_ID;};
};

class Plane_finished_ack_message : public Message {
public:
    Plane_finished_ack_message() { id = PLANE_FINISHED_ACK_MSG_ID;};
};

class Parent_plane_finished_message: public Message {
public:
    Parent_plane_finished_message() { id = PARENT_PLANE_FINISHED_MSG_ID;}
};

#endif /* NEIGHBORMESSAGES_H_ */
