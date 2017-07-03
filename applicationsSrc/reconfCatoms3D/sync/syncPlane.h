/*
 *  syncPlane.h
 *
 *  Created on: 15 Juin 2017
 *  Author: Thadeu
 */

#ifndef SYNCPLANE_H_
#define SYNCPLANE_H_

#include "catoms3DBlockCode.h"
#include "../reconf.h"

class SyncPlane {
private:
    int getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos);
protected:
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;

    bool isLowestOfBorder(int idx);

public:
    SyncPlane(Catoms3D::Catoms3DBlock *c, Reconf *r);
    ~SyncPlane();

    bool isOnBorder();
    bool isSeed();
};

#endif /* SYNCPLANE_H_ */
