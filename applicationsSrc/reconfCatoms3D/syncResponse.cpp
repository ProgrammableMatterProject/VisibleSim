#include "syncResponse.h"
/*

void SyncResponse::responseSyncLineDown(bID requestCatomID, int requestLine, set<bID> visitedSeeds) {
    if (!isRequestHandled)
        return;
    isRequestHandled = false;
    if (lineSeeds.count(catom->blockId) && !visitedSeeds.count(catom->blockId)) {
        sendMessageResponseSyncLineUp(requestCatomID, requestLine);
    }
    if (catom->blockId == lineParent) {
        catom->setColor(BLACK);
        sendMessageResponseSyncLineDown(requestCatomID, requestLine);
    }
    sendMessageResponseSyncLineDownFindLineParent(requestCatomID, requestLine, visitedSeeds, TO_LEFT);
    sendMessageResponseSyncLineDownFindLineParent(requestCatomID, requestLine, visitedSeeds, TO_RIGHT);
}

void SyncResponse::responseSyncLineUp(bID requestCatomID, int requestLine, set<bID> visitedSeeds) {
    if (!isRequestHandled)
        return;
    isRequestHandled = false;
    if (lineSeeds.count(catom->blockId)){ //&& !visitedSeeds.count(catom->blockId)) {
        visitedSeeds.insert(catom->blockId);
        sendMessageResponseSyncLineUp(requestCatomID, requestLine);
    }
    sendMessageResponseSyncLineUpFindLineSeeds(requestCatomID, requestLine, visitedSeeds, TO_LEFT);
    sendMessageResponseSyncLineUpFindLineSeeds(requestCatomID, requestLine, visitedSeeds, TO_RIGHT);
    
}
*/
