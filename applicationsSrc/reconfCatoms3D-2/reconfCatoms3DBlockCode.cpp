#include <iostream>
#include "reconfCatoms3DBlockCode.h"
#include "catoms3DWorld.h"

#define CONSTRUCT_WAIT_TIME 25
#define SYNC_WAIT_TIME 5
#define SYNC_RESPONSE_TIME SYNC_WAIT_TIME
#define PLANE_WAIT_TIME 0

using namespace std;
using namespace Catoms3D;

ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;

    reconf = new Reconf(catom);
    neighborhood = new Neighborhood(catom, reconf, buildNewBlockCode);
    neighborMessages = new NeighborMessages(catom, reconf, neighborhood);
}

ReconfCatoms3DBlockCode::~ReconfCatoms3DBlockCode() {
    delete reconf;
    delete neighborhood;
}

void ReconfCatoms3DBlockCode::startup() {
    if (catom->blockId == 1) {
        //srand(time(NULL));
        reconf->floor = 0;
    }

    planningRun();
    //stochasticRun();
    //neighborhood->addAllNeighbors();

    std::this_thread::sleep_for(std::chrono::milliseconds(CONSTRUCT_WAIT_TIME));
}

void ReconfCatoms3DBlockCode::planningRun() {
    if (catom->blockId == 1) {
        neighborMessages->init();
        reconf->planeParent = true;
    }
    else if (neighborhood->isFirstCatomOfPlane()) {
        reconf->planeParent = true;
        neighborMessages->sendMessageToGetPlaneParentInfo();
        catom->setColor(GREEN);
    }
    else if (neighborhood->isFirstCatomOfLine()) {
        neighborMessages->sendMessageToGetLineParentInfo();
    }
    else {
        neighborMessages->sendMessageToGetParentInfo();
    }
}

void ReconfCatoms3DBlockCode::stochasticRun() {
    for (int i = 0; i < 100000; i++) {
        int id = rand()%Catoms3D::getWorld()->getSize() + 1;
        ReconfCatoms3DBlockCode *catom = (ReconfCatoms3DBlockCode*)Catoms3D::getWorld()->getBlockById(id)->blockCode;
        if (catom->neighborhood->addFirstNeighbor())
            break;
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
                neighborMessages->handleNewCatomMsg(message);
                break;
            }
            case NEW_CATOM_LINE_PARENT_MSG_ID:
            {
                neighborMessages->handleNewCatomLineParentMsg(message);
                break;
            }
            case NEW_CATOM_PLANE_PARENT_MSG_ID:
            {
                neighborMessages->handleNewCatomPlaneParentMsg(message);
                break;
            }
            case NEW_CATOM_RESPONSE_MSG_ID:
            {
                neighborMessages->handleNewCatomResponseMsg(message);
                neighborMessages->init();
                break;
            }
            case NEW_CATOM_LINE_PARENT_RESPONSE_MSG_ID:
            {
                neighborMessages->handleNewCatomLineParentResponseMsg(message);
                neighborMessages->init();
                break;
            }
            case NEW_CATOM_PLANE_PARENT_RESPONSE_MSG_ID:
            {
                neighborMessages->handleNewCatomPlaneParentResponseMsg(message);
                neighborMessages->init();
                break;
            }
            case CANFILLLEFT_MESSAGE_ID:
            {
                neighborhood->sendResponseMessageToAddLeft();
                break;
            }
            case CANFILLLEFTRESPONSE_MESSAGE_ID:
            {
                if (reconf->floor == 0 || reconf->arePreviousPlaneNeighborsComplete())
                    neighborhood->addNeighborToLeft();
                break;
            }
            case CANFILLRIGHT_MESSAGE_ID:
            {
                neighborhood->sendResponseMessageToAddRight();
                break;
            }
            case CANFILLRIGHTRESPONSE_MESSAGE_ID:
            {
                if (reconf->floor == 0 || reconf->arePreviousPlaneNeighborsComplete())
                    neighborhood->addNeighborToRight();
                break;
            }
          }
      }
      break;
    case ADDLEFTBLOCK_EVENT_ID: {
        neighborhood->addNeighbor(catom->position.addX(-1));
        //getStats();
        break;
    }
    case ADDRIGHTBLOCK_EVENT_ID: {
        neighborhood->addNeighbor(catom->position.addX(1));
        //getStats();
        break;
    }
    case ADDNEXTLINE_EVENT_ID: {
        neighborhood->addNextLineNeighbor();
        //getStats();
        break;
    }
    case ADDPREVIOUSLINE_EVENT_ID: {
        neighborhood->addPreviousLineNeighbor();
        //getStats();
        break;
    }
    case EVENT_ADD_NEIGHBOR: {
        //if (catom->getInterface(9)->isConnected() && catom->getInterface(10)->isConnected())
            //neighborhood->addRight();
        //if (catom->getInterface(8)->isConnected() && catom->getInterface(11)->isConnected())
            //neighborhood->addLeft();
        neighborhood->addNeighbors();
        break;
    }
	}
}

void ReconfCatoms3DBlockCode::getStats() {
    int count = 0;
    count += Scheduler::getScheduler()->getNbEventsById(ADDLEFTBLOCK_EVENT_ID);
    count += Scheduler::getScheduler()->getNbEventsById(ADDRIGHTBLOCK_EVENT_ID);
    count += Scheduler::getScheduler()->getNbEventsById(ADDNEXTLINE_EVENT_ID);
    count += Scheduler::getScheduler()->getNbEventsById(ADDPREVIOUSLINE_EVENT_ID);
    int nbBlocks = World::getWorld()->getNbBlocks();
    int nMessages = 0;
    nMessages += NeighborMessages::nMessagesGetInfo;
    nMessages += Neighborhood::numberMessagesToAddBlock;
    cout << nbBlocks*100/12607 << ';' << count << ';' << nbBlocks << ';' << nMessages << endl;
}

BlockCode* ReconfCatoms3DBlockCode::buildNewBlockCode(BuildingBlock *host) {
    return (new ReconfCatoms3DBlockCode((Catoms3DBlock*)host));
}
