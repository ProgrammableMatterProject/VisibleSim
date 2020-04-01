/*
 *  reconf.h
 *
 *  Created on: 28 February 2017
 *  Author: Thadeu
 */

#ifndef RECONF_H_
#define RECONF_H_
#include <queue>
#include "robots/catoms3D/catoms3DBlockCode.h"
#include "robots/catoms3D/catoms3DWorld.h"
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

    /* previous floor verification */
    bool confirmNorthLeft;
    bool confirmNorthRight;
    bool confirmWestLeft;
    bool confirmWestRight;
    bool confirmSouthLeft;
    bool confirmSouthRight;
    bool confirmEastLeft;
    bool confirmEastRight;

    /* current floor verification */
    bool canFillLeft;
    bool canFillRight;
    /* Check module x+1 y+1 z or x-1 y-1 z */
    bool canFillNextFloor;

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

    bool canFillNorth() { return confirmNorthLeft && confirmNorthRight; };
    bool canFillWest() { return confirmWestLeft && confirmWestRight; };
    bool canFillSouth() { return confirmSouthLeft && confirmSouthRight; };
    bool canFillEast() { return confirmEastLeft && confirmEastRight; };
};

class MessageQueue {
public:
    Cell3DPosition destination;
    Message* message;
    MessageQueue(Cell3DPosition dest, Message *msg) : destination(dest), message(msg) {}
};

#endif /* RECONF_H_ */
