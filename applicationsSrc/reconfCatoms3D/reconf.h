/*
 *  reconf.h
 *
 *  Created on: 28 February 2017
 *  Author: Thadeu
 */

#ifndef RECONF_H_
#define RECONF_H_
#include <queue>
#include "catoms3DBlockCode.h"
#include "catoms3DWorld.h"
#include "directions.h"
#include "sync/syncPlaneNode.h"

class MessageQueue;

class Reconf {
    Catoms3D::Catoms3DBlock *catom;

    bool lineParent;

    bool seedNext;
    bool seedPrevious;

    bool isInternalSeedNext();
    bool isInternalSeedPrevious();
    bool isBorderSeedNext();
    bool isBorderSeedPrevious();

    bool isHighestOfBorder(int idx);
    int getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos);
    bool isHighest();

public:
    bool isPlaneParent;
    bool isPlaneCompleted;

    bool init;
    //bool isLeaf;
    int childConfirm;
    int nChildren;
    P2PNetworkInterface* interfaceParent;

    SyncPlane_node *syncPlaneNodeParent;
    SyncPlane_node *syncPlaneNode;
    Cell3DPosition lastMessage;

    vector<MessageQueue> messageQueue;

    Reconf(Catoms3D::Catoms3DBlock *c);

    bool isSeedNext();
    bool isSeedPrevious();

    bool isLineParent() { return lineParent; }
    void setLineParent() { lineParent = true; }

    void setSeedNext() { seedNext = true; };
    void setSeedPrevious() { seedPrevious = true; };

    bool checkPlaneCompleted();
    bool areNeighborsPlaced();
    bool isOnBorder();

    void addMessageOnQueue(MessageQueue mQueue);
};

class MessageQueue {
public:
    Cell3DPosition destination;
    Message* message;
    MessageQueue(Cell3DPosition dest, Message *msg) : destination(dest), message(msg) {}
};

#endif /* RECONF_H_ */
