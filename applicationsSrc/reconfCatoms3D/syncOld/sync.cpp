#include "sync.h"

Sync::Sync(Catoms3D::Catoms3DBlock *c, Reconf *r) : catom(c), reconf(r)
{
    syncResponseModel = new SyncResponseModel;
    syncResponse = new SyncResponse(catom, syncResponseModel);
    syncLeft = new SyncLeft(catom, reconf, syncResponseModel, syncResponse);
}

Sync::~Sync()
{
    delete syncLeft;
    delete syncResponse;
    delete syncResponseModel;
}

void Sync::sync() {
    if (reconf->needSyncToLeftNext()) {
        SyncModel syncModel(catom->blockId, catom->position.addX(-1).addY(-1));
        syncLeft->syncSeed(syncModel, TO_PREVIOUS);
    }
    if (reconf->needSyncToRightNext()) {
        SyncModel syncModel(catom->blockId, catom->position.addX(1).addY(1));
//        syncRight->syncSeed(syncModel, TO_PREVIOUS);
    }
}

void Sync::handleResponse(MessagePtr message) {
    shared_ptr<Sync_response_message> recv_message = static_pointer_cast<Sync_response_message>(message);
    if (recv_message->canSyncLine)
        syncResponse->forwardResponse(recv_message);
    handleResponseLeft(recv_message);
}

void Sync::handleResponseLeft(shared_ptr<Sync_response_message> msg)
{
    SyncRoute *route = &syncResponseModel->routes[msg->syncModel.requestCatomID];
    if (reconf->isSeedNext() && reconf->getNumberSeedsLeft() && !route->leftSeedVisited && route->direction != DIRECTION_LEFT) {
        route->leftSeedVisited = true;
        syncLeft->syncSeed(msg->syncModel, TO_PREVIOUS);
    }
    else if (reconf->isLineParent() && route->leftSeedVisited && route->direction != DIRECTION_DOWN) {
        route->parentVisited = true;
        syncLeft->syncSeed(msg->syncModel, TO_PREVIOUS);
    }
    else {
        syncResponse->forwardResponse(msg);
    }
}

void Sync::handleLookupNeighborLeftMessage(MessagePtr message)
{
    shared_ptr<Lookup_forward_left_sync_message> recv_message = static_pointer_cast<Lookup_forward_left_sync_message>(message);

    if (recv_message->side_direction == TO_LEFT) {
        syncResponseModel->routes[recv_message->syncInfo.requestCatomID].direction = DIRECTION_RIGHT;
        //syncResponseModel->routes[recv_message->syncInfo.requestCatomID].rightSeedVisited = true;
    }
    if (recv_message->side_direction == TO_RIGHT) {
        syncResponseModel->routes[recv_message->syncInfo.requestCatomID].direction = DIRECTION_LEFT;
        //syncResponseModel->routes[recv_message->syncInfo.requestCatomID].leftSeedVisited = true;
    }

    syncLeft->syncNeighbor(recv_message->syncInfo, recv_message->side_direction, recv_message->line_direction);
}

void Sync::handleLookupLineLeftMessage(MessagePtr message)
{
    shared_ptr<Lookup_line_left_sync_message> recv_message = static_pointer_cast<Lookup_line_left_sync_message>(message);

    if (recv_message->lineDirection == TO_NEXT) {
        syncResponseModel->routes[recv_message->syncInfo.requestCatomID].direction = DIRECTION_DOWN;
        //syncResponseModel->routes[recv_message->syncInfo.requestCatomID].parentVisited = true;
    }
    if (recv_message->lineDirection == TO_PREVIOUS) {
        syncResponseModel->routes[recv_message->syncInfo.requestCatomID].direction = DIRECTION_UP;
        //syncResponseModel->routes[recv_message->syncInfo.requestCatomID].nextSeedVisited = true;
    }

    syncLeft->syncSeed(recv_message->syncInfo, recv_message->lineDirection);
}
