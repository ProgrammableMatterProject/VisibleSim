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
#include <vector>

#define SYNCPLANE_MESSAGE_ID    8301

class SyncPlane {
private:
    int getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos);
    bool isLowestOfBorder(Cell3DPosition pos);
    bool isOnBorder(Cell3DPosition pos);
    int getIdxForBorder(Cell3DPosition pos);

protected:
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;

public:
    SyncPlane(Catoms3D::Catoms3DBlock *c, Reconf *r);
    ~SyncPlane();

    bool isSeed();
    void sendMessageCanConstructNextPlane();
};

class SyncPlane_message : public Message {
public:
    int floorNumber;
    SyncPlane_message(int floorNumber) {
        this->id = SYNCPLANE_MESSAGE_ID;
        this->floorNumber = floorNumber;
    }
};

class SyncPlane_node_manager {
public:
    static SyncPlane_node *root;
};

#endif /* SYNCPLANE_H_ */

