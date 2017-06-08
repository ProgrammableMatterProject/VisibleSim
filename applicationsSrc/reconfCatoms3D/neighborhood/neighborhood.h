/*
 *  neighborhood.h
 *
 *  Created on: 28 February 2017
 *  Author: Thadeu
 */

#ifndef NEIGHBOR_H_
#define NEIGHBOR_H_


#include "cell3DPosition.h"
#include "directions.h"
#include "../reconf.h"
#include "../sync/sync.h"
#include "../syncCCW/syncCCW.h"

class Neighborhood {
private:
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;
    Sync *sync;
    SyncCCW *syncCCW;
    BlockCodeBuilder blockCodeBuilder;

    void addNeighbor(Cell3DPosition pos);
    void addNeighborToLeft();
    void addNeighborToRight();

public:
    Neighborhood(Catoms3D::Catoms3DBlock *catom, Reconf *reconf, Sync *s, SyncCCW *sccw, BlockCodeBuilder blockCodeBuilder);

    void addNeighborsWithoutSync();
    void addAllNeighbors();

    void tryAddNextLineNeighbor();
    void tryAddPreviousLineNeighbor();
    void tryAddNeighbors();

    bool isOnLeftBorder();
    bool isOnRightBorder();
    bool isFirstCatomOfLine();
};

#endif /* NEIGHBORHOOD_H_ */
