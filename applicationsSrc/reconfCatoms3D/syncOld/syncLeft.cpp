#include "syncLeft.h"
//SYNC TO LEFT

void SyncLeft::syncSeed(SyncModel syncModel, LINE_DIRECTION syncToLineDirection) {
    if (reconf->isLineCompleted() && catom->position == syncModel.requestPosition) {
        syncResponse->response(syncModel, syncResponseModel->routes[syncModel.requestCatomID].direction, true, true, true);
    }
    else if (reconf->isLineCompleted() && !reconf->isSeedNext() && reconf->getNumberSeedsLeft() == 0 &&!catom->getInterface(catom->position.offsetX(1))->isConnected()) {
        syncResponse->response(syncModel, syncResponseModel->routes[syncModel.requestCatomID].direction, false, true, true);
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
    if (reconf->isSeedNext())
        sendSeedMessage(syncModel, TO_NEXT);
    else if (reconf->getNumberSeedsLeft())
        sendNeighborMessage(syncModel, TO_LEFT, TO_NEXT);
    else if (reconf->getNumberSeedsRight())
        sendNeighborMessage(syncModel, TO_RIGHT, TO_NEXT);
    // line without seeds.
    else if (catom->getInterface(catom->position.offsetX(1))->isConnected())
        sendNeighborMessage(syncModel, TO_RIGHT, TO_NEXT);
}

void SyncLeft::syncSeedPrevious(SyncModel syncModel) {
    SyncRoute *route = &syncResponseModel->routes[syncModel.requestCatomID];
    if (reconf->isLineParent() && (!reconf->getNumberSeedsLeft() || route->leftSeedVisited == true))
        sendSeedMessage(syncModel, TO_PREVIOUS);
    else if(route->leftSeedVisited == false && (reconf->getNumberSeedsLeft() || reconf->lineParentDirection == TO_LEFT))
        sendNeighborMessage(syncModel, TO_LEFT, TO_PREVIOUS);
    else if (reconf->lineParentDirection == TO_RIGHT)
        sendNeighborMessage(syncModel, TO_RIGHT, TO_PREVIOUS);
}

void SyncLeft::syncNeighbor(SyncModel syncModel, SIDE_DIRECTION sideDirection, LINE_DIRECTION lineDirection) {
    if (catom->position == syncModel.requestPosition) {
        syncResponse->response(syncModel, syncResponseModel->routes[syncModel.requestCatomID].direction, true, true, true);
    }
    else if (sideDirection == TO_RIGHT && !catom->getInterface(catom->position.offsetX(1))->isConnected() &&
            ((!reconf->isSeedNext() && lineDirection == TO_NEXT) ||
            (!reconf->isLineParent() && lineDirection == TO_PREVIOUS)) ) {
        syncResponse->response(syncModel, syncResponseModel->routes[syncModel.requestCatomID].direction, false, true, true);
    }
    else if (lineDirection == TO_NEXT)
        syncNeighborNext(syncModel, sideDirection);
    else
        syncNeighborPrevious(syncModel, sideDirection);
}

void SyncLeft::syncNeighborNext(SyncModel syncModel, SIDE_DIRECTION sideDirection)
{
    if (reconf->isSeedNext()) {
        sendSeedMessage(syncModel, TO_NEXT);
    }
    else if (sideDirection == TO_LEFT) { //&& reconf->getNumberSeedsLeft()) {
        sendNeighborMessage(syncModel, TO_LEFT, TO_NEXT);
    }
    else if (sideDirection == TO_RIGHT) {
        sendNeighborMessage(syncModel, TO_RIGHT, TO_NEXT);
    }
}
void SyncLeft::syncNeighborPrevious(SyncModel syncModel, SIDE_DIRECTION sideDirection)
{
    if (reconf->isSeedNext() && sideDirection == TO_LEFT) {
        sendSeedMessage(syncModel, TO_NEXT);
    }
    else if (sideDirection == TO_LEFT && reconf->getNumberSeedsLeft()) {
        sendNeighborMessage(syncModel, TO_LEFT, TO_PREVIOUS);
    }
    else if (reconf->isLineParent()) {
        sendSeedMessage(syncModel, TO_PREVIOUS);
    }
    else if (sideDirection == TO_RIGHT) {
        sendNeighborMessage(syncModel, TO_RIGHT, TO_PREVIOUS);
    }
}

void SyncLeft::sendNeighborMessage(SyncModel syncModel, SIDE_DIRECTION side_direction, LINE_DIRECTION line_direction) {
    Cell3DPosition neighborPosition;
    SyncRoute *route = &syncResponseModel->routes[syncModel.requestCatomID];
    if (side_direction == TO_LEFT) {
        neighborPosition = catom->position.offsetX(-1);
        route->leftSeedVisited = true;
    }
    else {
        neighborPosition = catom->position.offsetX(1);
        route->rightSeedVisited = true;
    }

    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
        Lookup_forward_left_sync_message *msg = new Lookup_forward_left_sync_message(syncModel, side_direction, line_direction);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}

void SyncLeft::sendSeedMessage(SyncModel syncModel, LINE_DIRECTION lineDirection) {
    Cell3DPosition neighborPosition;
    SyncRoute *route = &syncResponseModel->routes[syncModel.requestCatomID];
    if (lineDirection == TO_PREVIOUS) {
        neighborPosition = catom->position.offsetY(-1);
        route->parentVisited = true;
    }
    else {
        neighborPosition = catom->position.offsetY(1);
        route->nextSeedVisited = true;
    }

    if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
        Lookup_line_left_sync_message *msg = new Lookup_line_left_sync_message(syncModel, lineDirection);
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(neighborPosition)));
    }
}
