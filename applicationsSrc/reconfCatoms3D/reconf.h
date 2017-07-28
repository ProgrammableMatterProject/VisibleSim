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

class Reconf {
    Catoms3D::Catoms3DBlock *catom;

    int numberSeedsLeft;
    int numberSeedsRight;

    bool lineParent;
    bool lineCompleted;

    bool seedNext;
    bool seedPrevious;

    bool leftCompleted;
    bool rightCompleted;


    bool isInternalSeedNext();
    bool isInternalSeedPrevious();
    bool isBorderSeedNext();
    bool isBorderSeedPrevious();

    bool isHighestOfBorder(int idx);
    int getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos);
    bool isOnBorder();
    bool isHighest();
public:
    bool planeParent;
    bool planeFinished;
    bool planeFinishedAck;
    bool createdFromPrevious;
    queue<MessagePtr> requestQueue;
    SIDE_DIRECTION lineParentDirection;

    SyncPlane_node *syncPlaneNodeParent;
    SyncPlane_node *syncPlaneNode;

    Reconf(Catoms3D::Catoms3DBlock *c);

    bool isSeedNext();
    bool isSeedPrevious();
    bool needSync();

    bool needSyncToLeftNext();
    bool needSyncToLeftPrevious();

    bool needSyncToRightNext();
    bool needSyncToRightPrevious();

    bool isLineParent() { return lineParent; }
    void setLineParent() { lineParent = true; }

    int getNumberSeedsLeft() { return numberSeedsLeft; }
    void setNumberSeedsLeft(int nSeeds) { numberSeedsLeft = nSeeds; }

    int getNumberSeedsRight() { return numberSeedsRight; }
    void setNumberSeedsRight(int nSeeds) { numberSeedsRight = nSeeds; }

    void setLineCompleted() { lineCompleted = true; }
    bool isLineCompleted() { return lineCompleted; }

    void setSeedNext() { seedNext = true; };
    void setSeedPrevious() { seedPrevious = true; };

    void setLeftCompleted();
    bool isLeftCompleted() { return leftCompleted; }

    void setRightCompleted();
    bool isRightCompleted() { return rightCompleted; }

    bool checkPlaneCompleted();

};

#endif /* RECONF_H_ */
