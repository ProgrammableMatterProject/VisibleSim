#include <iostream>
#include "reconfCatoms3DBlockCode.h"

#define WAIT_TIME 0

using namespace std;
using namespace Catoms3D;

string CSG_FILE = "data/testForm.bc";

ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
	cout << "ReconfCatoms3DBlockCode constructor" << endl;
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;

    reconf = new Reconf(catom);
    neighbor = new Neighbor(catom, reconf, buildNewBlockCode);
    sync = new Sync(catom);
}

ReconfCatoms3DBlockCode::~ReconfCatoms3DBlockCode() {
	cout << "ReconfCatoms3DBlockCode destructor" << endl;
}

void ReconfCatoms3DBlockCode::startup() {
	if (catom->blockId==1) {
        CsgUtils::init(CSG_FILE);
        if (!CsgUtils::isInside(catom->position)) {
            catom->setColor(RED);
        }
	}
    reconf->isSeedCheck();
    if (neighbor->isFirstCatomOfLine()) {
        neighbor->init();
        if (reconf->needSync()) {
            sync->syncRequest->syncLineSeedToLeft(catom->blockId, catom->position[1]+1, reconf, TO_PREVIOUS);
            catom->setColor(BLACK);
        }
        else
            neighbor->addNeighbors();
    }

    else {
        neighbor->sendMessageToGetLineInfo();
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
}

void ReconfCatoms3DBlockCode::catomReady(MessagePtr message)
{
    New_catom_response_ptr recv_message = static_pointer_cast<New_catom_response_message>(message);
    if (reconf->needSync()) {
        sync->syncRequest->syncLineSeedToLeft(catom->blockId, catom->position[1]+1, reconf, TO_PREVIOUS);
        catom->setColor(BLACK);
    }
    else {
        if (recv_message->lineParentDirection == TO_LEFT)
            neighbor->addNeighborToRight();
        else
            neighbor->addNeighborToLeft();
    }
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
                neighbor->handleNewCatomMsg(message);
                break;
            }
            case NEW_CATOM_RESPONSE_MSG_ID:
            {
                neighbor->handleNewCatomResponseMsg(message);
                catomReady(message);
            }
            case LEFT_SIDE_COMPLETED_MSG_ID:
            {
                neighbor->handleLeftSideCompletedMsg(message);
                break;
            }
            case RIGHT_SIDE_COMPLETED_MSG_ID:
            {
                neighbor->handleRightSideCompletedMsg(message);
                break;
            }
            case LOOKUP_NEIGHBOR_SYNC_MESSAGE_ID:
            {
                sync->handleLookupNeighborMessage(message, reconf);
                catom->setColor(LIGHTGREEN);
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
                break;
            }
            case LOOKUP_LINE_SYNC_MESSAGE_ID:
            {
                sync->handleLookupLineMessage(message, reconf);
                catom->setColor(GREEN);
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
                break;
            }
            case SYNC_RESPONSE_MESSAGE_ID:
            {
                catom->setColor(BLUE);
                shared_ptr<Sync_response_message> recv_message = static_pointer_cast<Sync_response_message>(message);
                if (recv_message->requestCatomID != catom->blockId) {
//                    syncResponse->forwardResponse(recv_message, syncRoutes[recv_message->requestCatomID]);
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
            }
          }
      }
      break;
	}
}

BlockCode* ReconfCatoms3DBlockCode::buildNewBlockCode(BuildingBlock *host) {
    return (new ReconfCatoms3DBlockCode((Catoms3DBlock*)host));
}
