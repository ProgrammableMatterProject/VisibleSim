#include <iostream>
#include "reconfCatoms3DBlockCode.h"

#define CONSTRUCT_WAIT_TIME 5
#define SYNC_WAIT_TIME 20
#define SYNC_RESPONSE_TIME SYNC_WAIT_TIME + 20

using namespace std;
using namespace Catoms3D;

ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;

    reconf = new Reconf(catom);
    sync = new Sync(catom, reconf);
    syncCCW = new SyncCCW(catom, reconf);
    neighborhood = new Neighborhood(catom, reconf, sync, syncCCW, buildNewBlockCode);
    neighborMessages = new NeighborMessages(catom, reconf, neighborhood, sync, syncCCW);
}

ReconfCatoms3DBlockCode::~ReconfCatoms3DBlockCode() {
    delete reconf;
    delete neighborhood;
    delete sync;
}

void ReconfCatoms3DBlockCode::startup() {
	if (catom->blockId == 1) {
        if (!BlockCode::target->isInTarget(catom->position)) {
            catom->setColor(RED);
        }
        neighborMessages->init();
	}
    if (neighborhood->isFirstCatomOfLine())
        neighborMessages->sendMessageToGetParentInfo();
    else
        neighborMessages->sendMessageToGetLineInfo();
    std::this_thread::sleep_for(std::chrono::milliseconds(CONSTRUCT_WAIT_TIME));
}

void ReconfCatoms3DBlockCode::processLocalEvent(EventPtr pev) {
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
    case EVENT_NI_RECEIVE: {
      message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
        switch(message->id) {
            case NEW_CATOM_MSG_ID:
            {
                neighborMessages->handleNewCatomMsg(message);
                break;
            }
            case NEW_CATOM_PARENT_MSG_ID:
            {
                neighborMessages->handleNewCatomParentMsg(message);
                break;
            }
            case NEW_CATOM_RESPONSE_MSG_ID:
            {
                neighborMessages->handleNewCatomResponseMsg(message);
                neighborMessages->init();
                break;
            }
            case NEW_CATOM_PARENT_RESPONSE_MSG_ID:
            {
                neighborMessages->handleNewCatomParentResponseMsg(message);
                neighborMessages->init();
                break;
            }
            case LEFT_SIDE_COMPLETED_MSG_ID:
            {
                neighborMessages->handleLeftSideCompletedMsg(message);
                break;
            }
            case RIGHT_SIDE_COMPLETED_MSG_ID:
            {
                neighborMessages->handleRightSideCompletedMsg(message);
                break;
            }
            case SYNCCCW_MESSAGE_ID:
            {
                shared_ptr<SyncCCW_message> recv_message = static_pointer_cast<SyncCCW_message>(message);
                if (recv_message->goal == catom->position) {
                    syncCCW->response();
                }
                else if (reconf->isSeedNext() && !reconf->isLineCompleted())
                {
                    reconf->requestQueue.push(recv_message);
                }
                else {
                    syncCCW->handleMessage(recv_message);
                    catom->setColor(BLUE);
                    std::this_thread::sleep_for(std::chrono::milliseconds(SYNC_WAIT_TIME));
                }
                break;
            }
            case SYNCCCW_RESPONSE_MESSAGE_ID:
            {
                shared_ptr<SyncCCW_response_message> recv_message = static_pointer_cast<SyncCCW_response_message>(message);
                if (recv_message->goal == catom->position) {
                    neighborhood->addNeighborsWithoutSync();
                }
                else {
                    syncCCW->handleResponseMessage(recv_message);
                    catom->setColor(GREEN);
                    std::this_thread::sleep_for(std::chrono::milliseconds(SYNC_WAIT_TIME));
                }
                break;
            }
            case LOOKUP_NEIGHBOR_LEFT_SYNC_MESSAGE_ID:
            {
                sync->handleLookupNeighborLeftMessage(message);
                catom->setColor(LIGHTGREEN);
                std::this_thread::sleep_for(std::chrono::milliseconds(SYNC_WAIT_TIME));
                break;
            }
            case LOOKUP_LINE_LEFT_SYNC_MESSAGE_ID:
            {
                sync->handleLookupLineLeftMessage(message);
                catom->setColor(GREEN);
                std::this_thread::sleep_for(std::chrono::milliseconds(SYNC_WAIT_TIME));
                break;
            }
            case SYNC_RESPONSE_MESSAGE_ID:
            {
                catom->setColor(BLUE);
                shared_ptr<Sync_response_message> recv_message = static_pointer_cast<Sync_response_message>(message);
                //if (recv_message->canSyncLine && recv_message->syncModel.requestCatomID == catom->blockId) {
                if (recv_message->syncModel.requestCatomID == catom->blockId) {
                    sync->setSyncOK();
                    neighborhood->tryAddNeighbors();
                }
                else if(recv_message->syncModel.requestCatomID != catom->blockId) {
                    sync->handleResponse(recv_message);
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(SYNC_RESPONSE_TIME));
            }
          }
      }
      break;
	}
}

BlockCode* ReconfCatoms3DBlockCode::buildNewBlockCode(BuildingBlock *host) {
    return (new ReconfCatoms3DBlockCode((Catoms3DBlock*)host));
}
