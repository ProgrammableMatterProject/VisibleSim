#include "syncLeft.h"

void SyncLeft::syncSeed(bID requestCatomID, Cell3DPosition requestPosition, Reconf *reconf, LINE_DIRECTION syncToLineDirection) {
    if (syncToLineDirection == TO_NEXT) {
        syncSeedNext(requestCatomID, requestPosition, reconf);
    }
    else if (syncToLineDirection == TO_PREVIOUS) {
        syncSeedPrevious(requestCatomID, requestPosition, reconf);
    }
}

void SyncLeft::syncSeedNext(bID requestCatomID, Cell3DPosition requestPosition, Reconf *reconf) {
    if (reconf->isSeed()) {
        sendSeedMessage(requestCatomID, requestPosition, TO_NEXT);
        catom->setColor(BLUE);
    }
    else {
        if (reconf->getNumberSeedsLeft())
            sendNeighborMessage(requestCatomID, requestPosition, TO_LEFT, TO_NEXT);
        else if (reconf->getNumberSeedsRight())
            sendNeighborMessage(requestCatomID, requestPosition, TO_RIGHT, TO_NEXT);
        else
            cout << "ERROR - expecting seed on this line" << endl;
    }
}

void SyncLeft::syncSeedPrevious(bID requestCatomID, Cell3DPosition requestPosition, Reconf *reconf) {
    if (reconf->getNumberSeedsLeft())
        sendNeighborMessage(requestCatomID, requestPosition, TO_LEFT, TO_PREVIOUS);
    else if (reconf->isLineParent())  {
        sendSeedMessage(requestCatomID, requestPosition, TO_PREVIOUS);
        catom->setColor(BLUE);
    }
    else {
        if (reconf->lineParentDirection == TO_LEFT)
            sendNeighborMessage(requestCatomID, requestPosition, TO_LEFT, TO_PREVIOUS);
        else if (reconf->lineParentDirection == TO_RIGHT)
            sendNeighborMessage(requestCatomID, requestPosition, TO_RIGHT, TO_PREVIOUS);
    }
}

void SyncLeft::syncNeighbor(bID requestCatomID, Cell3DPosition requestPosition, Reconf *reconf, SIDE_DIRECTION sideDirection, LINE_DIRECTION lineDirection) {
    if (reconf->isSeed() && reconf->isLineCompleted() && !(sideDirection == TO_RIGHT && lineDirection == TO_PREVIOUS)) {
        sendSeedMessage(requestCatomID, requestPosition, TO_NEXT);
    }
    else if (reconf->getNumberSeedsLeft() && reconf->isLineCompleted() && sideDirection == TO_LEFT) {
        sendNeighborMessage(requestCatomID, requestPosition, TO_LEFT, lineDirection);
    }
    else if (reconf->isLineParent() && lineDirection == TO_PREVIOUS) {
        sendSeedMessage(requestCatomID, requestPosition, TO_PREVIOUS);
    }
    else if (reconf->getNumberSeedsRight() && reconf->isLineCompleted() && sideDirection == TO_RIGHT) {
        sendNeighborMessage(requestCatomID, requestPosition, TO_RIGHT, lineDirection);
    }
    else {
        sendNeighborMessage(requestCatomID, requestPosition, reconf->lineParentDirection, lineDirection);
    }
}

void SyncLeft::sendNeighborMessage(bID requestCatomID, Cell3DPosition requestPosition, SIDE_DIRECTION side_direction, LINE_DIRECTION line_direction) {
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

void SyncLeft::sendSeedMessage(bID requestCatomID, Cell3DPosition requestPosition, LINE_DIRECTION lineDirection) {
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

