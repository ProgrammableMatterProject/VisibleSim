#include "sync.h"

Sync::Sync(Catoms3D::Catoms3DBlock *c) : catom(c)
{
    syncRequest = new SyncRequest(catom);
    syncResponse = new SyncResponse(catom);
}

void Sync::handleLookupNeighborMessage(MessagePtr message, Reconf *reconf)
{
    shared_ptr<Lookup_neighbor_sync_message> recv_message = static_pointer_cast<Lookup_neighbor_sync_message>(message);

    if (recv_message->side_direction == TO_LEFT)
        syncRoutes[recv_message->requestCatomID].direction = DIRECTION_RIGHT;
    if (recv_message->side_direction == TO_RIGHT)
        syncRoutes[recv_message->requestCatomID].direction = DIRECTION_LEFT;
    syncRequest->syncLineNeighborToLeft(recv_message->requestCatomID, recv_message->requestLine, reconf, recv_message->side_direction);
}

void Sync::handleLookupLineMessage(MessagePtr message, Reconf *reconf)
{
    shared_ptr<Lookup_line_sync_message> recv_message = static_pointer_cast<Lookup_line_sync_message>(message);

    if (recv_message->lineDirection == TO_NEXT)
        syncRoutes[recv_message->requestCatomID].direction = DIRECTION_DOWN;
    if (recv_message->lineDirection == TO_PREVIOUS) 
        syncRoutes[recv_message->requestCatomID].direction = DIRECTION_UP;

    /*if (catom->blockId == lineParent && 
      currentLine == recv_message->requestLine) {*/
    if (catom->blockId == 139) {
        std::this_thread::sleep_for(std::chrono::milliseconds(1000));
        syncResponse->response(recv_message->requestCatomID, syncRoutes[recv_message->requestCatomID].direction, true);
    }
    else {
        syncRequest->syncLineSeedToLeft(recv_message->requestCatomID, recv_message->requestLine, reconf, recv_message->lineDirection);
    }
}
