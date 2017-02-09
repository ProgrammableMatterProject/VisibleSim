/*
 *  syncRequest.h
 *
 *  Created on: 09 February 2017
 *  Author: Thadeu
 */
#ifndef SYNCRESPONSE_H_
#define SYNCRESPONSE_H_


#include <set>
#include "catoms3DBlock.h"
#include "syncRequest.h"

class SyncResponse {
	Catoms3D::Catoms3DBlock *catom;

public:
    //SyncResponse(Catoms3D::Catoms3DBlock *c) : catom(c) {}
    void setCatom(Catoms3D::Catoms3DBlock *c) {catom = c;}
void responseSyncLineDown(bID requestCatomID, int requestLine, set<bID> visitedSeeds, set<bID> lineSeeds, bID lineParent);

void responseSyncLineUp(bID requestCatomID, int requestLine, set<bID> visitedSeeds, set<bID> lineSeeds, bID lineParent);

void sendMessageResponseSyncLineDownFindLineParent(bID requestCatomID, int requestLine, set<bID> visitedSeeds, SIDE_DIRECTION side_direction);

void sendMessageResponseSyncLineUpFindLineSeeds(bID requestCatomID, int requestLine, set<bID> visitedSeeds, SIDE_DIRECTION side_direction);

    void sendMessageResponseSyncLineDown(bID requestCatomID, int requestLine);

    void sendMessageResponseSyncLineUp(bID requestCatomID, int requestLine);

};

#endif /* SYNCRESPONSE_H_ */
