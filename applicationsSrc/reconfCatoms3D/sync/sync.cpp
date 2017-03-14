#include "sync.h"

Sync::Sync(Catoms3D::Catoms3DBlock *c, Reconf *r) : catom(c), reconf(r)
{
    syncRequest = new SyncRequest(catom);
    syncResponse = new SyncResponse(catom);
    syncOK = false;
}

void Sync::sync() {
    if (reconf->needSyncToLeft()) {
        syncRequest->syncLineSeedToLeft(catom->blockId, catom->position.addX(-1).addY(-1), reconf, TO_PREVIOUS);
    }
}

void Sync::handleResponse(MessagePtr message) {
    shared_ptr<Sync_response_message> recv_message = static_pointer_cast<Sync_response_message>(message);
    SyncRoute *route = &syncRoutes[recv_message->requestCatomID];
    if (recv_message->canSyncLine) {
        syncResponse->forwardResponse(recv_message, syncRoutes[recv_message->requestCatomID]);
    }
    else {
        if (reconf->getNumberSeedsLeft() && !route->leftSeedVisited) {
            route->leftSeedVisited = true;
            syncRequest->syncLineSeedToLeft(recv_message->requestCatomID, recv_message->requestPosition, reconf, TO_PREVIOUS);
        }
        else if (reconf->getNumberSeedsRight() && !route->rightSeedVisited) {
            route->rightSeedVisited = true;
            syncRequest->syncLineSeedToLeft(recv_message->requestCatomID, recv_message->requestPosition, reconf, TO_PREVIOUS);
        }
        else if (reconf->isSeed() && !route->nextSeedVisited) {
            route->nextSeedVisited = true;
            syncResponse->forwardResponse(recv_message, syncRoutes[recv_message->requestCatomID]);
        }
        else if (reconf->isLineParent() && !route->parentVisited) {
            route->parentVisited = true;
            syncResponse->forwardResponse(recv_message, syncRoutes[recv_message->requestCatomID]);
        }
        else {
            syncResponse->forwardResponse(recv_message, syncRoutes[recv_message->requestCatomID]);
        }
    }
}


void Sync::handleLookupForwardMessage(MessagePtr message, Reconf *reconf)
{
    shared_ptr<Lookup_forward_sync_message> recv_message = static_pointer_cast<Lookup_forward_sync_message>(message);

    if (recv_message->side_direction == TO_LEFT) {
        syncRoutes[recv_message->requestCatomID].direction = DIRECTION_RIGHT;
        syncRoutes[recv_message->requestCatomID].leftSeedVisited = true;
    }
    if (recv_message->side_direction == TO_RIGHT) {
        syncRoutes[recv_message->requestCatomID].direction = DIRECTION_LEFT;
        syncRoutes[recv_message->requestCatomID].rightSeedVisited = true;
    }
    if (catom->position == recv_message->requestPosition) {
        syncResponse->response(recv_message->requestCatomID, recv_message->requestPosition, syncRoutes[recv_message->requestCatomID].direction, true);
    }
    syncRequest->syncLineNeighborToLeft(recv_message->requestCatomID, recv_message->requestPosition, reconf, recv_message->side_direction, recv_message->line_direction);
    catom->setColor(RED);
}

void Sync::handleLookupLineMessage(MessagePtr message, Reconf *reconf)
{
    shared_ptr<Lookup_line_sync_message> recv_message = static_pointer_cast<Lookup_line_sync_message>(message);

    if (recv_message->lineDirection == TO_NEXT) {
        syncRoutes[recv_message->requestCatomID].direction = DIRECTION_DOWN;
        syncRoutes[recv_message->requestCatomID].nextSeedVisited = true;
    }
    if (recv_message->lineDirection == TO_PREVIOUS) {
        syncRoutes[recv_message->requestCatomID].direction = DIRECTION_UP;
        syncRoutes[recv_message->requestCatomID].parentVisited = true;
    }

    if (reconf->isLineCompleted() && catom->position == recv_message->requestPosition) {
        catom->setColor(RED);
        syncResponse->response(recv_message->requestCatomID, recv_message->requestPosition, syncRoutes[recv_message->requestCatomID].direction, true);
    }
    else if (reconf->isLineCompleted() && !reconf->isSeed() && reconf->getNumberSeedsLeft() == 0 &&!catom->getInterface(catom->position.addX(1))->isConnected()) {
        syncResponse->response(recv_message->requestCatomID, recv_message->requestPosition, syncRoutes[recv_message->requestCatomID].direction, false);
    }
    else {
        syncRequest->syncLineSeedToLeft(recv_message->requestCatomID, recv_message->requestPosition, reconf, recv_message->lineDirection);
    }
    catom->setColor(WHITE);
}
