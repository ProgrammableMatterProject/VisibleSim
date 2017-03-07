#include "syncRequest.h"

/*
 *  Sync for the first catom of the line.
 *  Ignores the response to the catom it directly received the msg.
 *  Look for other seeds/parent for both sides of the line.
 */
void SyncRequest::syncLineSeedToLeft(bID requestCatomID, int requestLine, Reconf *reconf, LINE_DIRECTION syncToLineDirection) {
    cout << "syncLineSeed BlockID =  " << catom->blockId << endl;
    if (syncToLineDirection == TO_NEXT &&
            reconf->isSeed()){
        sendSeedMessage(requestCatomID, requestLine, TO_NEXT);
        catom->setColor(BLUE);
    }
    else if (syncToLineDirection == TO_PREVIOUS &&
            reconf->isLineParent()) {
        sendSeedMessage(requestCatomID, requestLine, TO_PREVIOUS);
        catom->setColor(BLUE);
    }
    else if (reconf->lineParentDirection == TO_LEFT || reconf->getNumberSeedsLeft())
        sendNeighborMessage(requestCatomID, requestLine, TO_LEFT);
    else
        sendNeighborMessage(requestCatomID, requestLine, TO_RIGHT);

}

void SyncRequest::syncLineNeighborToLeft(bID requestCatomID, int requestLine, Reconf *reconf, SIDE_DIRECTION sideDirection) {
    cout << "syncLineNeighbor BlockID =  " << catom->blockId << endl;
    if (reconf->isSeed()) {
        sendSeedMessage(requestCatomID, requestLine, TO_NEXT);
    }
    if (reconf->isLineParent()) {
        sendSeedMessage(requestCatomID, requestLine, TO_PREVIOUS);
    }
    sendNeighborMessage(requestCatomID, requestLine, sideDirection);
}

void SyncRequest::sendNeighborMessage(bID requestCatomID, int requestLine, SIDE_DIRECTION side_direction) {
    Cell3DPosition neighborPosition;
    if (side_direction == TO_LEFT) 
        neighborPosition = catom->position.addX(-1);
    else
        neighborPosition = catom->position.addX(1);

    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
        Lookup_neighbor_sync_message *msg = new Lookup_neighbor_sync_message(requestCatomID, requestLine, side_direction);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}

void SyncRequest::sendSeedMessage(bID requestCatomID, int requestLine, LINE_DIRECTION lineDirection) {
    Cell3DPosition neighborPosition;
    if (lineDirection == TO_PREVIOUS) 
        neighborPosition = catom->position.addY(-1);
    else
        neighborPosition = catom->position.addY(1);

    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
        Lookup_line_sync_message *msg = new Lookup_line_sync_message(requestCatomID, requestLine, lineDirection);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}

