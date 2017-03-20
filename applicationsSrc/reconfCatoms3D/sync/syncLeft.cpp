#include "syncLeft.h"

/*void SyncLeft::goToLeft(bID requestCatomID, Cell3DPosition requestPosition, LINE_DIRECTION lineDirection) {
    sendNeighborMessage(requestCatomID, requestPosition, TO_LEFT, lineDirection);
}

void SyncLeft::goToRight(bID requestCatomID, Cell3DPosition requestPosition, LINE_DIRECTION lineDirection) {
    sendNeighborMessage(requestCatomID, requestPosition, TO_RIGHT, lineDirection);
}*/

void SyncLeft::syncSeed(SyncModel syncModel, LINE_DIRECTION syncToLineDirection) {
    if (reconf->isLineCompleted() && catom->position == syncModel.requestPosition) {
        syncResponse->response(syncModel, syncResponseModel->routes[syncModel.requestCatomID].direction, true);
    }
    else if (reconf->isLineCompleted() && !reconf->isSeed() && reconf->getNumberSeedsLeft() == 0 &&!catom->getInterface(catom->position.addX(1))->isConnected()) {
        syncResponse->response(syncModel, syncResponseModel->routes[syncModel.requestCatomID].direction, false);
    }
    else {
        if (syncToLineDirection == TO_NEXT) {
            syncSeedNext(syncModel);
        }
        else if (syncToLineDirection == TO_PREVIOUS) {
            syncSeedPrevious(syncModel);
        }
    }
}

void SyncLeft::syncSeedNext(SyncModel syncModel) {
    if (reconf->isSeed()) {
        sendSeedMessage(syncModel, TO_NEXT);
        catom->setColor(BLUE);
    }
    else {
        if (reconf->getNumberSeedsLeft())
            sendNeighborMessage(syncModel, TO_LEFT, TO_NEXT);
        else if (reconf->getNumberSeedsRight())
            sendNeighborMessage(syncModel, TO_RIGHT, TO_NEXT);
        else {
            if (catom->getInterface(catom->position.addX(1))->isConnected())
                sendNeighborMessage(syncModel, TO_RIGHT, TO_NEXT);
            else {
                // TODO
                syncResponse->response(syncModel, DIRECTION_RIGHT, false);
            }
            //cout << "ERROR - expecting seed on this line" << endl;
        }
    }
}

void SyncLeft::syncSeedPrevious(SyncModel syncModel) {
    if (reconf->getNumberSeedsLeft())
        sendNeighborMessage(syncModel, TO_LEFT, TO_PREVIOUS);
    else if (reconf->isLineParent())  {
        sendSeedMessage(syncModel, TO_PREVIOUS);
        catom->setColor(BLUE);
    }
    else {
        if (reconf->lineParentDirection == TO_LEFT)
            sendNeighborMessage(syncModel, TO_LEFT, TO_PREVIOUS);
        else if (reconf->lineParentDirection == TO_RIGHT)
            sendNeighborMessage(syncModel, TO_RIGHT, TO_PREVIOUS);
    }
}

void SyncLeft::syncNeighbor(SyncModel syncModel, SIDE_DIRECTION sideDirection, LINE_DIRECTION lineDirection) {
    if (catom->position == syncModel.requestPosition) {
        syncResponse->response(syncModel, syncResponseModel->routes[syncModel.requestCatomID].direction, true);
    }
    else if (sideDirection == TO_RIGHT && !catom->getInterface(catom->position.addX(1))->isConnected() &&
            ((!reconf->isSeed() && lineDirection == TO_NEXT) ||
            (!reconf->isLineParent() && lineDirection == TO_PREVIOUS)) ) {
        syncResponse->response(syncModel, syncResponseModel->routes[syncModel.requestCatomID].direction, false);
    }

    else if (reconf->isSeed() && reconf->isLineCompleted() && !(sideDirection == TO_RIGHT && lineDirection == TO_PREVIOUS)) {
        sendSeedMessage(syncModel, TO_NEXT);
    }
    else if (sideDirection == TO_LEFT && reconf->getNumberSeedsLeft()) {
        sendNeighborMessage(syncModel, TO_LEFT, lineDirection);
    }
    //else if (reconf->getNumberSeedsLeft() && reconf->isLineCompleted() && sideDirection == TO_LEFT) {
    else if (reconf->isLineParent() && lineDirection == TO_PREVIOUS) {
        sendSeedMessage(syncModel, TO_PREVIOUS);
    }
    //else if (reconf->getNumberSeedsRight() && reconf->isLineCompleted() && sideDirection == TO_RIGHT) {
    else if (sideDirection == TO_RIGHT) {
        sendNeighborMessage(syncModel, TO_RIGHT, lineDirection);
    }
    else {
        cout << "ERROR" << endl;
    }
}

void SyncLeft::sendNeighborMessage(SyncModel syncModel, SIDE_DIRECTION side_direction, LINE_DIRECTION line_direction) {
    Cell3DPosition neighborPosition;
    if (side_direction == TO_LEFT)
        neighborPosition = catom->position.addX(-1);
    else
        neighborPosition = catom->position.addX(1);

    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
        Lookup_forward_sync_message *msg = new Lookup_forward_sync_message(syncModel, side_direction, line_direction);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}

void SyncLeft::sendSeedMessage(SyncModel syncModel, LINE_DIRECTION lineDirection) {
    Cell3DPosition neighborPosition;
    if (lineDirection == TO_PREVIOUS)
        neighborPosition = catom->position.addY(-1);
    else
        neighborPosition = catom->position.addY(1);

    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
        Lookup_line_sync_message *msg = new Lookup_line_sync_message(syncModel, lineDirection);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}

