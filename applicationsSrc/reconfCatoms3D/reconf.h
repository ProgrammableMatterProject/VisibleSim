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
    bool leftCompleted;
    bool rightCompleted;
    bool imSeed;

    bool needSyncToLeft();
    bool needSyncToRight();
    bool isInternalSeed();
    bool isBorderSeed();
public:
    int numberSeedsLeft;
    int numberSeedsRight;
    int lineParentDirection;
    bID lineParent;
    int currentLine;

    Reconf();
    void setCatom(Catoms3D::Catoms3DBlock *c) {catom = c;}
    bool isSeed();
    bool needSync();

    void setLeftCompleted() { leftCompleted = true; }
    void setRightCompleted() { rightCompleted = true; }
    bool isLeftCompleted() { return leftCompleted; }
    bool isRightCompleted() { return rightCompleted; }
    bool isOnLeftBorder();
    bool isOnRightBorder();
    bool iAmSeed() const {
        return imSeed;
    }
};

#endif /* RECONF_H_ */
