#include "syncRequest.h"

/*
 *  Sync the seed of the next line.
 *  Ignores the response to the catom it directly received the msg.
 *  Look for other seeds/parent for both sides of the line.
 */
void SyncRequest::syncLineSeedToLeft(bID requestCatomID, Cell3DPosition requestPosition, Reconf *reconf, LINE_DIRECTION syncToLineDirection) {
    if (syncToLineDirection == TO_NEXT) {
        if (reconf->isSeed()) {
            sendSeedMessage(requestCatomID, requestPosition, TO_NEXT);
            catom->setColor(BLUE);
        }
        else {
            if (reconf->getNumberSeedsLeft())
                sendNeighborMessage(requestCatomID, requestPosition, TO_LEFT, syncToLineDirection);
            else if (reconf->getNumberSeedsRight())
                sendNeighborMessage(requestCatomID, requestPosition, TO_RIGHT, syncToLineDirection);
            else
                cout << "ERROR - expecting seed on this line" << endl;
        }
    }
    else if (syncToLineDirection == TO_PREVIOUS) {
        if (reconf->getNumberSeedsLeft())
            sendNeighborMessage(requestCatomID, requestPosition, TO_LEFT, syncToLineDirection);
        else if (reconf->isLineParent())  {
            sendSeedMessage(requestCatomID, requestPosition, TO_PREVIOUS);
            catom->setColor(BLUE);
        }
        else {
            if (reconf->lineParentDirection == TO_LEFT)
                sendNeighborMessage(requestCatomID, requestPosition, TO_LEFT, syncToLineDirection);
            else if (reconf->lineParentDirection == TO_RIGHT)
                sendNeighborMessage(requestCatomID, requestPosition, TO_RIGHT, syncToLineDirection);
        }
    }
}

void SyncRequest::syncLineNeighborToLeft(bID requestCatomID, Cell3DPosition requestPosition, Reconf *reconf, SIDE_DIRECTION sideDirection, LINE_DIRECTION lineDirection) {
    if (reconf->isSeed() && reconf->isLineCompleted()) {
        sendSeedMessage(requestCatomID, requestPosition, TO_NEXT);
    }
    else if (reconf->getNumberSeedsLeft() && reconf->isLineCompleted() && sideDirection == TO_LEFT) {
        sendNeighborMessage(requestCatomID, requestPosition, TO_LEFT, lineDirection);
    }
    else if (reconf->getNumberSeedsRight() && reconf->isLineCompleted() && sideDirection == TO_RIGHT) {
        sendNeighborMessage(requestCatomID, requestPosition, TO_RIGHT, lineDirection);
    }
    else if (reconf->isLineParent()) {
        sendSeedMessage(requestCatomID, requestPosition, TO_PREVIOUS);
    }
    else {
        sendNeighborMessage(requestCatomID, requestPosition, reconf->lineParentDirection, lineDirection);
    }
}

void SyncRequest::sendNeighborMessage(bID requestCatomID, Cell3DPosition requestPosition, SIDE_DIRECTION side_direction, LINE_DIRECTION line_direction) {
    Cell3DPosition neighborPosition;
    if (side_direction == TO_LEFT) 
        neighborPosition = catom->position.addX(-1);
    else
        neighborPosition = catom->position.addX(1);

    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
        Lookup_forward_sync_message *msg = new Lookup_forward_sync_message(requestCatomID, requestPosition, side_direction, line_direction);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}

void SyncRequest::sendSeedMessage(bID requestCatomID, Cell3DPosition requestPosition, LINE_DIRECTION lineDirection) {
    Cell3DPosition neighborPosition;
    if (lineDirection == TO_PREVIOUS) 
        neighborPosition = catom->position.addY(-1);
    else
        neighborPosition = catom->position.addY(1);

    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
        Lookup_line_sync_message *msg = new Lookup_line_sync_message(requestCatomID, requestPosition, lineDirection);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}

