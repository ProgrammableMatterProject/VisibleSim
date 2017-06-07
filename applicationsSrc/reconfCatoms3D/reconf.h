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
#include "directions.h"

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

    bool isInternalSeed(LINE_DIRECTION);
    bool isBorderSeed(LINE_DIRECTION);
public:
    queue<MessagePtr> requestQueue;
    SIDE_DIRECTION lineParentDirection;

    Reconf(Catoms3D::Catoms3DBlock *c);

    bool isSeedNext();
    bool isSeedPrevious();
    bool needSync();
    bool needSyncToLeft();
    bool needSyncToRight();

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

};

#endif /* RECONF_H_ */
