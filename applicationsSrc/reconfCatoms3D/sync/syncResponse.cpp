#include "syncResponse.h"

void SyncResponse::response(SyncModel syncModel, DIRECTION direction, bool canSyncLine) {
    Sync_response_message *msg = new Sync_response_message(syncModel, canSyncLine);
    Cell3DPosition pos = catom->position;
    if (direction == DIRECTION_UP)
        pos = pos.addY(1);
    if (direction == DIRECTION_DOWN)
        pos = pos.addY(-1);
    if (direction == DIRECTION_RIGHT)
        pos = pos.addX(1);
    if (direction == DIRECTION_LEFT)
        pos = pos.addX(-1);
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 1000, msg, catom->getInterface(pos)));
}

/*
 * Check if all neighbors confirmed before forward the response
 */
void SyncResponse::forwardResponse(shared_ptr<Sync_response_message> msg) {
    response(msg->syncModel, syncResponseModel->routes[msg->syncModel.requestCatomID].direction, msg->canSyncLine);
    /*
    if (msg->messageFromParent == false) {
        if (msg->canSyncLine == 1) {
            response(msg->requestCatomID, syncRoute.direction, msg->canSyncLine);
        }
        if (msg->canSyncLine == 0) {
            int lineSeedsConfirmations = ++syncRoute.lineSeedsConfirmations;
            bool parentConfirmation = syncRoute.parentConfirmation;
            if (lineSeedsConfirmations == numberLineSeeds && parentConfirmation)
                response(msg->requestCatomID, syncRoute.direction, msg->canSyncLine);

        }
    }
    else {
        int lineSeedsConfirmations = syncRoute.lineSeedsConfirmations;
        syncRoute.parentConfirmation = true;
        if (lineSeedsConfirmations == numberLineSeeds-1)
            response(msg->requestCatomID, syncRoute.direction, msg->canSyncLine);


    }
    */
}
