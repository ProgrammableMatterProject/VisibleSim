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
public:
    void setCatom(Catoms3D::Catoms3DBlock *c) {catom = c;}

    void addNeighbor(BlockCodeBuilder blockCodeBuilder, Cell3DPosition pos);
    void addNeighbors(BlockCodeBuilder blockCodeBuilder);
    void addNeighborToRight(BlockCodeBuilder blockCodeBuilder, Reconf reconf);
    void addNeighborToLeft(BlockCodeBuilder blockCodeBuilder, Reconf reconf);
    void sendMessageLineCompleted(Reconf reconf, SIDE_COMPLETED side);
    void sendMessageToGetNeighborInformation();
    void tryAddNextLineNeighbor(BlockCodeBuilder, Reconf &reconf);
    void checkLineCompleted(Reconf &reconf);
};

class New_catom_message : public Message {
public:
    New_catom_message() { id = NEW_CATOM_MSG_ID; };
};

class New_catom_response_message : public Message {
public:
    int currentLine;
    bID lineParent;
    bool leftCompleted, rightCompleted;
    int numberSeedsLeft, numberSeedsRight;
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
