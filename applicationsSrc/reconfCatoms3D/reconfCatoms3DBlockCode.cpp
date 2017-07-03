#include <iostream>
#include "reconfCatoms3DBlockCode.h"

#define CONSTRUCT_WAIT_TIME 5
#define SYNC_WAIT_TIME 10
#define SYNC_RESPONSE_TIME SYNC_WAIT_TIME

using namespace std;
using namespace Catoms3D;

ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;

    reconf = new Reconf(catom);
    syncNext = new SyncNext(catom, reconf);
    syncPrevious = new SyncPrevious(catom, reconf);
    syncPlane = new SyncPlane(catom, reconf);
    neighborhood = new Neighborhood(catom, reconf, syncNext, syncPrevious, buildNewBlockCode);
    neighborMessages = new NeighborMessages(catom, reconf, neighborhood, syncNext, syncPrevious, syncPlane);
}

ReconfCatoms3DBlockCode::~ReconfCatoms3DBlockCode() {
    delete reconf;
    delete neighborhood;
    delete syncNext;
    delete syncPrevious;
}

void ReconfCatoms3DBlockCode::startup() {
	if (neighborhood->isFirstCatomOfPlane()) {
        if (!BlockCode::target->isInTarget(catom->position)) {
            catom->setColor(RED);
        }
        reconf->planeParent = true;
        neighborMessages->init();
	}
    if (neighborhood->isFirstCatomOfLine()) {
        neighborMessages->sendMessageToGetParentInfo();
    }
    else
        neighborMessages->sendMessageToGetLineInfo();

    //neighborhood->addAllNeighbors();
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
            case SYNCNEXT_MESSAGE_ID:
            {
                shared_ptr<Sync_message> recv_message = static_pointer_cast<Sync_message>(message);
                if (catom->position[0] <= recv_message->goal[0] && catom->position[1] == recv_message->goal[1]) {
                    syncNext->response(recv_message->origin);
                }
                else if (!reconf->isLineCompleted() && (reconf->isSeedNext() || reconf->isSeedPrevious()))
                {
                    reconf->requestQueue.push(recv_message);
                }
                else if (reconf->needSyncToRightNext()) {
                    reconf->setSeedNext();
                    neighborhood->tryAddNeighbors();
                    neighborhood->tryAddNextLineNeighbor();
                    reconf->requestQueue.push(recv_message);
                }
                else if (reconf->needSyncToRightPrevious()) {
                    syncNext->response(recv_message->origin);
                }
                else {
                    syncNext->handleMessage(recv_message);
                    std::this_thread::sleep_for(std::chrono::milliseconds(SYNC_WAIT_TIME));
                }
                break;
            }
            case SYNCPREVIOUS_MESSAGE_ID:
            {
                shared_ptr<Sync_message> recv_message = static_pointer_cast<Sync_message>(message);
                if (catom->position[0] >= recv_message->goal[0] && catom->position[1] == recv_message->goal[1]) {
                    syncPrevious->response(recv_message->origin);
                }
                else if (!reconf->isLineCompleted() && (reconf->isSeedNext() || reconf->isSeedPrevious()))
                {
                    reconf->requestQueue.push(recv_message);
                }
                else if (reconf->needSyncToRightNext()) {
                    syncPrevious->response(recv_message->origin);
                }
                else if (reconf->needSyncToRightPrevious()) {
                    reconf->setSeedPrevious();
                    neighborhood->tryAddNeighbors();
                    neighborhood->tryAddPreviousLineNeighbor();
                    reconf->requestQueue.push(recv_message);
                }
                else {
                    syncPrevious->handleMessage(recv_message);
                    std::this_thread::sleep_for(std::chrono::milliseconds(SYNC_WAIT_TIME));
                }
                break;
            }
            case SYNCNEXT_RESPONSE_MESSAGE_ID:
            case SYNCPREVIOUS_RESPONSE_MESSAGE_ID:
            {
                shared_ptr<Sync_response_message> recv_message = static_pointer_cast<Sync_response_message>(message);
                if (recv_message->origin == catom->position) {
                    neighborhood->addNeighborsWithoutSync();
                }
                else {
                    syncNext->handleMessageResponse(recv_message);
                    std::this_thread::sleep_for(std::chrono::milliseconds(SYNC_WAIT_TIME));
                }
                break;
            }
            case PLANE_FINISHED_MSG_ID:
            {
                if (!reconf->planeFinished) {
                    if (syncPlane->isSeed()) {
                        catom->setColor(BLACK);
                        neighborhood->addNeighborToNextPlane();
                    }
                    else if (reconf->planeParent) {
                        catom->setColor(GREY);
                        neighborhood->addNeighborToNextPlane();
                    }
                    neighborMessages->sendMessagePlaneFinished();
                }
                break;
            }
          }
      }
      break;
	}
}

BlockCode* ReconfCatoms3DBlockCode::buildNewBlockCode(BuildingBlock *host) {
    return (new ReconfCatoms3DBlockCode((Catoms3DBlock*)host));
}
