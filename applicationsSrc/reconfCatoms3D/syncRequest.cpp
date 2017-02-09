#include "syncRequest.h"

void SyncRequest::requestSyncLineDown(bID requestCatomID, int requestLine, set<bID> visitedSeeds, set<bID> lineSeeds, bID lineParent) {
    if (isRequestHandled)
        return;
    isRequestHandled = true;
    if (lineSeeds.count(catom->blockId) && !visitedSeeds.count(catom->blockId)) {
        sendMessageSyncLineUp(requestCatomID, requestLine);
    }
    if (catom->blockId == lineParent) {
        catom->setColor(BLACK);
        sendMessageSyncLineDown(requestCatomID, requestLine);
    }
    sendMessageSyncLineDownFindLineParent(requestCatomID, requestLine, visitedSeeds, TO_LEFT);
    sendMessageSyncLineDownFindLineParent(requestCatomID, requestLine, visitedSeeds, TO_RIGHT);
}

void SyncRequest::requestSyncLineUp(bID requestCatomID, int requestLine, set<bID> visitedSeeds, set<bID> lineSeeds, bID lineParent) {
    if (isRequestHandled)
        return;
    isRequestHandled = true;
    if (lineSeeds.count(catom->blockId)){ //&& !visitedSeeds.count(catom->blockId)) {
        visitedSeeds.insert(catom->blockId);
        sendMessageSyncLineUp(requestCatomID, requestLine);
    }
    sendMessageSyncLineUpFindLineSeeds(requestCatomID, requestLine, visitedSeeds, TO_LEFT);
    sendMessageSyncLineUpFindLineSeeds(requestCatomID, requestLine, visitedSeeds, TO_RIGHT);
    
}

void SyncRequest::sendMessageSyncLineDownFindLineParent(bID requestCatomID, int requestLine, set<bID> visitedSeeds, SIDE_DIRECTION side_direction) {
    Cell3DPosition neighborPosition;
    if (side_direction == TO_LEFT) 
        neighborPosition = catom->position.addX(-1);
    else
        neighborPosition = catom->position.addX(1);

    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
        Find_line_parent_sync_message *msg = new Find_line_parent_sync_message(requestCatomID, requestLine, visitedSeeds, TO_DOWN);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}

void SyncRequest::sendMessageSyncLineUpFindLineSeeds(bID requestCatomID, int requestLine, set<bID> visitedSeeds, SIDE_DIRECTION side_direction) {
    Cell3DPosition neighborPosition;
    if (side_direction == TO_LEFT) 
        neighborPosition = catom->position.addX(-1);
    else
        neighborPosition = catom->position.addX(1);

    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
        Find_line_seed_sync_message *msg = new Find_line_seed_sync_message(requestCatomID, requestLine, visitedSeeds, TO_DOWN);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}

/*
 * Send sync message to previous line
 */
void SyncRequest::sendMessageSyncLineDown(bID requestCatomID, int requestLine) {
    Cell3DPosition neighborPosition(catom->position.addY(-1));

    catom->setColor(GREEN);
    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
       Line_down_sync_message *msg = new Line_down_sync_message(requestCatomID, requestLine);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}

/*
 * Send sync message to next line
 */
void SyncRequest::sendMessageSyncLineUp(bID requestCatomID, int requestLine) {
    Cell3DPosition neighborPosition(catom->position.addY(1));

    catom->setColor(BLUE);
    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
        Line_up_sync_message *msg = new Line_up_sync_message(requestCatomID, requestLine);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}
