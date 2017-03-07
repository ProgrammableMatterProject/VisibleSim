/*
 *  neighbor.h
 *
 *  Created on: 28 February 2017
 *  Author: Thadeu
 */

#ifndef NEIGHBOR_H_
#define NEIGHBOR_H_

#define NEW_CATOM_MSG_ID	9001
#define NEW_CATOM_RESPONSE_MSG_ID	9002
#define RIGHT_SIDE_COMPLETED_MSG_ID	9003
#define LEFT_SIDE_COMPLETED_MSG_ID	9004

#include "cell3DPosition.h"
#include "directions.h"
#include "../reconf.h"

class Neighbor {
    Catoms3D::Catoms3DBlock *catom;
    BlockCodeBuilder blockCodeBuilder;
    bool leftCompleted;
    bool rightCompleted;

public:
    Neighbor(Catoms3D::Catoms3DBlock *catom, BlockCodeBuilder blockCodeBuilder);

    void addNeighbor(Cell3DPosition pos);
    void addNeighborToLeft(Reconf *reconf);
    void addNeighborToRight(Reconf *reconf);
    void addAllNeighbors();

    void sendMessageRightSideCompleted(int numberSeedsRight, bool isSeed);
    void sendMessageLeftSideCompleted(int numberSeedsLeft, bool isSeed);
    void sendMessageToGetNeighborInformation();

    void tryAddNextLineNeighbor(Reconf *reconf);
    void checkLineCompleted(Reconf *reconf);

    void setLeftCompleted() { leftCompleted = true; }
    void setRightCompleted() { rightCompleted = true; }
    bool isLeftCompleted() { return leftCompleted; }
    bool isRightCompleted() { return rightCompleted; }
    bool isOnLeftBorder();
    bool isOnRightBorder();
    bool isFirstCatomOfLine();

    void handleNewCatomMsg(MessagePtr msg, Reconf *reconf);
    void handleNewCatomResponseMsg(MessagePtr msg, Reconf *reconf);
    void handleLeftSideCompletedMsg(MessagePtr msg, Reconf *reconf);
    void handleRightSideCompletedMsg(MessagePtr msg, Reconf *reconf);

};

class New_catom_message : public Message {
public:
    SIDE_DIRECTION lineParentDirection;
    New_catom_message() { id = NEW_CATOM_MSG_ID; };
};

class New_catom_response_message : public Message {
public:
    int currentLine;
    bID lineParent;
    bool leftCompleted, rightCompleted;
    int numberSeedsLeft, numberSeedsRight;
    SIDE_DIRECTION lineParentDirection;
    New_catom_response_message() { id = NEW_CATOM_RESPONSE_MSG_ID; };
};

class Right_side_completed_message : public Message {
public:
    int numberSeedsRight;
    Right_side_completed_message(int nSeedsRight) : numberSeedsRight(nSeedsRight) { id = RIGHT_SIDE_COMPLETED_MSG_ID; };
};

class Left_side_completed_message : public Message {
public:
    int numberSeedsLeft;
    Left_side_completed_message(int nSeedsLeft) : numberSeedsLeft(nSeedsLeft) { id = LEFT_SIDE_COMPLETED_MSG_ID; };
};
#endif /* NEIGHBOR_H_ */
