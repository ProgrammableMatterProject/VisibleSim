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
#include "border.h"

class MessageQueue;

class Reconf {
    Catoms3D::Catoms3DBlock *catom;

    bool seedNext;
    bool seedPrevious;

    bool isInternalSeedNext();
    bool isInternalSeedPrevious();
    bool isBorderSeedNext();
    bool isBorderSeedPrevious();

    bool isHighestOfBorder(int idx);


public:
    bool init;
    vector<MessageQueue> messageQueue;
    bool isLineParent;
    bool isPlaneParent;
    bool planeSeed;
    bool confirmNorthLeft;
    bool confirmNorthRight;
    bool confirmWestLeft;
    bool confirmWestRight;
    bool confirmSouthLeft;
    bool confirmSouthRight;
    bool confirmEastLeft;
    bool confirmEastRight;
    int floor;
    int childConfirm;
    int nChildren;
    bool isPlaneCompleted;
    P2PNetworkInterface* interfaceParent;
    bool parentPlaneFinished;

    Reconf(Catoms3D::Catoms3DBlock *c);

    bool isSeedNext();
    bool isSeedPrevious();

    void setSeedNext() { seedNext = true; };
    void setSeedPrevious() { seedPrevious = true; };

    bool checkPlaneCompleted();
    bool areNeighborsPlaced();
    bool isOnBorder();

    void addMessageOnQueue(MessageQueue mQueue);
    bool isPlaneSeed();

    bool arePreviousPlaneNeighborsComplete();
    bool canAddNextPlaneSeed();
};

class MessageQueue {
public:
    Cell3DPosition destination;
    Message* message;
    MessageQueue(Cell3DPosition dest, Message *msg) : destination(dest), message(msg) {}
};

#endif /* RECONF_H_ */
