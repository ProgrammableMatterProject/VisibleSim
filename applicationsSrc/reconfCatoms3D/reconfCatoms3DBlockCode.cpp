#include <iostream>
#include "reconfCatoms3DBlockCode.h"

#define WAIT_TIME 0

using namespace std;
using namespace Catoms3D;

string CSG_FILE = "data/mug.bc";

ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
	cout << "ReconfCatoms3DBlockCode constructor" << endl;
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;

    neighbor = new Neighbor(catom, buildNewBlockCode);
    reconf = new Reconf(catom);
    sync = new Sync(catom);
}

ReconfCatoms3DBlockCode::~ReconfCatoms3DBlockCode() {
	cout << "ReconfCatoms3DBlockCode destructor" << endl;
}

void ReconfCatoms3DBlockCode::debug() {
    if (catom->blockId == 167) {
        neighbor->checkLineCompleted(reconf);
        if (reconf->needSync())
            catom->setColor(BLACK);
        std::this_thread::sleep_for(std::chrono::milliseconds(1000));
        sync->syncRequest->syncLineSeedToLeft(167, 10, reconf, TO_PREVIOUS);
    }
    else 
    {
        if (neighbor->isFirstCatomOfLine()) {
            reconf->setLineParent();
            reconf->isSeedCheck();
            neighbor->checkLineCompleted(reconf);
            neighbor->addNeighborToLeft(reconf);
            neighbor->addNeighborToRight(reconf);
        }
        else {
            neighbor->sendMessageToGetNeighborInformation();
        }
    }
}

void ReconfCatoms3DBlockCode::startup() {
	if (catom->blockId==1) {
        CsgUtils::init(CSG_FILE);
        if (!CsgUtils::isInside(catom->position)) {
            catom->setColor(RED);
        }
	}
    bool DEBUG = true;
    if (DEBUG) {
        debug();
    } 
    else {
        if (neighbor->isFirstCatomOfLine()) {
            reconf->setLineParent();
            neighbor->checkLineCompleted(reconf);
            neighbor->addNeighborToLeft(reconf);
            neighbor->addNeighborToRight(reconf);
        }
        else {
            neighbor->sendMessageToGetNeighborInformation();
        }
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
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
                neighbor->handleNewCatomMsg(message, reconf);
                break;
            }
            case NEW_CATOM_RESPONSE_MSG_ID:
            {
                neighbor->handleNewCatomResponseMsg(message, reconf);
                break;
            }
            case LEFT_SIDE_COMPLETED_MSG_ID:
            {
                neighbor->handleLeftSideCompletedMsg(message, reconf);
                break;
            }
            case RIGHT_SIDE_COMPLETED_MSG_ID:
            {
                neighbor->handleRightSideCompletedMsg(message, reconf);
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
                else {
                    catom->setColor(BLACK);
                    neighbor->sendMessageToGetNeighborInformation();
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
