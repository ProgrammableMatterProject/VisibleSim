#include <iostream>
#include "reconfCatoms3DBlockCode.h"

#define WAIT_TIME 5

using namespace std;
using namespace Catoms3D;

//string CSG_FILE = "data/manyHoles.bc";
string CSG_FILE = "data/manyHolesScale2.bc";
//string CSG_FILE = "data/letterC.bc";
//string CSG_FILE = "data/testForm.bc";


ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;

    reconf = new Reconf(catom);
    sync = new Sync(catom, reconf);
    neighborhood = new Neighborhood(catom, reconf, sync, buildNewBlockCode);
}

ReconfCatoms3DBlockCode::~ReconfCatoms3DBlockCode() {
    delete reconf;
    delete neighborhood;
    delete sync;
}

void ReconfCatoms3DBlockCode::startup() {
	if (catom->blockId == 1) {
        CsgUtils::init(CSG_FILE);
        if (!CsgUtils::isInside(catom->position)) {
            catom->setColor(RED);
        }
	}
    reconf->isSeedCheck();
    if (neighborhood->isFirstCatomOfLine()) {
        neighborhood->init();
        catomReady();
    }

    else {
        neighborhood->sendMessageToGetLineInfo();
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
        neighborhood->addNeighbors();
    }*/
    neighborhood->addNeighbors();
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
                neighborhood->handleNewCatomMsg(message);
                break;
            }
            case NEW_CATOM_RESPONSE_MSG_ID:
            {
                neighborhood->handleNewCatomResponseMsg(message);
                catomReady();
                std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
                break;
            }
            case LEFT_SIDE_COMPLETED_MSG_ID:
            {
                neighborhood->handleLeftSideCompletedMsg(message);
                break;
            }
            case RIGHT_SIDE_COMPLETED_MSG_ID:
            {
                neighborhood->handleRightSideCompletedMsg(message);
                break;
            }
            case LOOKUP_FORWARD_SYNC_MESSAGE_ID:
            {
                sync->handleLookupForwardMessage(message, reconf);
                catom->setColor(LIGHTGREEN);
                break;
            }
            case LOOKUP_LINE_SYNC_MESSAGE_ID:
            {
                sync->handleLookupLineMessage(message, reconf);
                catom->setColor(GREEN);
                std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
                break;
            }
            case SYNC_RESPONSE_MESSAGE_ID:
            {
                catom->setColor(BLUE);
                shared_ptr<Sync_response_message> recv_message = static_pointer_cast<Sync_response_message>(message);
                if (recv_message->requestCatomID == catom->blockId) {
                    sync->setSyncOK();
                    neighborhood->addNeighbors();
                }
                else {
                    sync->handleResponse(recv_message);
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
            }
          }
      }
      break;
	}
}

BlockCode* ReconfCatoms3DBlockCode::buildNewBlockCode(BuildingBlock *host) {
    return (new ReconfCatoms3DBlockCode((Catoms3DBlock*)host));
}
