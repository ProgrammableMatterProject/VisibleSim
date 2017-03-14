#include <iostream>
#include "reconfCatoms3DBlockCode.h"

#define WAIT_TIME 0

using namespace std;
using namespace Catoms3D;

//string CSG_FILE = "data/manyHoles.bc";
string CSG_FILE = "data/testForm.bc";


ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;

    reconf = new Reconf(catom);
    sync = new Sync(catom, reconf);
    neighbor = new Neighbor(catom, reconf, sync, buildNewBlockCode);
}

ReconfCatoms3DBlockCode::~ReconfCatoms3DBlockCode() {
    delete reconf;
    delete neighbor;
    delete sync;
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
        catomReady();
    }

    else {
        neighbor->sendMessageToGetLineInfo();
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
}

void ReconfCatoms3DBlockCode::catomReady()
{
    /*if (reconf->needSync()) {
        cout << catom->blockId << " " << reconf->isSeed() << endl;
        sync->sync();
        catom->setColor(BLACK);
    }
    else {
        neighbor->addNeighbors();
    }*/
    neighbor->addNeighbors();
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
                catomReady();
                break;
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
            case LOOKUP_FORWARD_SYNC_MESSAGE_ID:
            {
                sync->handleLookupForwardMessage(message, reconf);
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
                    sync->handleResponse(recv_message);
                }
                else {
                    sync->setSyncOK();
                    neighbor->addNeighbors();
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
