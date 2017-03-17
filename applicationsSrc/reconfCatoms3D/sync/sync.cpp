#include "sync.h"

Sync::Sync(Catoms3D::Catoms3DBlock *c, Reconf *r) : catom(c), reconf(r)
{
    syncData = new SyncData;
    syncResponse = new SyncResponse(catom, syncData);
    syncLeft = new SyncLeft(catom, syncData, syncResponse);
}

Sync::~Sync()
{
    delete syncLeft;
    delete syncResponse;
    delete syncData;
}

void Sync::sync() {
    if (reconf->needSyncToLeft()) {
        syncLeft->syncSeed(catom->blockId, catom->position.addX(-1).addY(-1), reconf, TO_PREVIOUS);
    }
}

void Sync::handleResponse(MessagePtr message) {
    shared_ptr<Sync_response_message> recv_message = static_pointer_cast<Sync_response_message>(message);
    SyncRoute *route = &syncData->routes[recv_message->requestCatomID];
    if (recv_message->canSyncLine) {
        syncResponse->forwardResponse(recv_message);
    }
    else {
        /*if (reconf->getNumberSeedsRight() && !route->rightSeedVisited) {
            route->rightSeedVisited = true;
            syncLeft->syncSeed(recv_message->requestCatomID, recv_message->requestPosition, reconf, TO_PREVIOUS);
        }
        else if (reconf->isSeed() && !route->nextSeedVisited) {
            route->nextSeedVisited = true;
            syncResponse->forwardResponse(recv_message);
        }
        else*/
        if (reconf->isSeed() && reconf->getNumberSeedsLeft() && !route->leftSeedVisited) {
            route->leftSeedVisited = true;
            syncLeft->syncSeed(recv_message->requestCatomID, recv_message->requestPosition, reconf, TO_PREVIOUS);
        }
        else {
            syncResponse->forwardResponse(recv_message);
        }
    }
}


void Sync::handleLookupForwardMessage(MessagePtr message, Reconf *reconf)
{
    shared_ptr<Lookup_forward_sync_message> recv_message = static_pointer_cast<Lookup_forward_sync_message>(message);

    if (recv_message->side_direction == TO_LEFT) {
        syncData->routes[recv_message->requestCatomID].direction = DIRECTION_RIGHT;
        syncData->routes[recv_message->requestCatomID].rightSeedVisited = true;
    }
    if (recv_message->side_direction == TO_RIGHT) {
        syncData->routes[recv_message->requestCatomID].direction = DIRECTION_LEFT;
        syncData->routes[recv_message->requestCatomID].leftSeedVisited = true;
    }

    syncLeft->syncNeighbor(recv_message->requestCatomID, recv_message->requestPosition, reconf, recv_message->side_direction, recv_message->line_direction);
}

void Sync::handleLookupLineMessage(MessagePtr message, Reconf *reconf)
{
    shared_ptr<Lookup_line_sync_message> recv_message = static_pointer_cast<Lookup_line_sync_message>(message);

    if (recv_message->lineDirection == TO_NEXT) {
        syncData->routes[recv_message->requestCatomID].direction = DIRECTION_DOWN;
        syncData->routes[recv_message->requestCatomID].parentVisited = true;
    }
    if (recv_message->lineDirection == TO_PREVIOUS) {
        syncData->routes[recv_message->requestCatomID].direction = DIRECTION_UP;
        syncData->routes[recv_message->requestCatomID].nextSeedVisited = true;
    }

    syncLeft->syncSeed(recv_message->requestCatomID, recv_message->requestPosition, reconf, recv_message->lineDirection);
}
