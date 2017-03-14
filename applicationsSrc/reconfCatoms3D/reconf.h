/*
 *  reconf.h
 *
 *  Created on: 28 February 2017
 *  Author: Thadeu
 */

#ifndef RECONF_H_
#define RECONF_H_
#include "catoms3DBlockCode.h"
#include "directions.h"

class Reconf {
    Catoms3D::Catoms3DBlock *catom;
    int numberSeedsLeft;
    int numberSeedsRight;
    bool seed;
    bool lineParent;
    bool lineCompleted;

    bool isInternalSeed();
    bool isBorderSeed();
public:
    SIDE_DIRECTION lineParentDirection;

    Reconf(Catoms3D::Catoms3DBlock *c);
    bool isSeed();
    bool needSync();
    bool needSyncToLeft();
    bool needSyncToRight();

    bool isSeedCheck();
    bool isLineParent() { return lineParent; }
    void setLineParent() { lineParent = true; }
    int getNumberSeedsLeft() { return numberSeedsLeft; }
    int getNumberSeedsRight() { return numberSeedsRight; }
    void setNumberSeedsLeft(int nSeeds) { numberSeedsLeft = nSeeds; }
    void setNumberSeedsRight(int nSeeds) { numberSeedsRight = nSeeds; }
    void setLineCompleted() { lineCompleted = true; }
    bool isLineCompleted() { return lineCompleted; }
    void setSeed(bool v) { seed = v; };

};

#endif /* RECONF_H_ */
