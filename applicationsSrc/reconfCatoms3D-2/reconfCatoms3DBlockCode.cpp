#include <iostream>
#include "reconfCatoms3DBlockCode.h"
#include "catoms3DWorld.h"

#define CONSTRUCT_WAIT_TIME 1
#define SYNC_WAIT_TIME 0
#define SYNC_RESPONSE_TIME SYNC_WAIT_TIME
#define PLANE_WAIT_TIME 0
#define PLANE_FINISHED_TIME 1

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
        reconf->isPlaneParent = true;
    }
    else if (neighborhood->isFirstCatomOfPlane()) {
        reconf->isPlaneParent = true;
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
                if (reconf->floor == 0 || (reconf->confirmWestLeft && reconf->confirmWestRight))
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
            case NEXTPLANECONFIRMATION_NORTHLEFT_MESSAGE_ID:
            {
                reconf->confirmNorthLeft = true;
                neighborhood->addNeighbors();
                break;
            }
            case NEXTPLANECONFIRMATION_NORTHRIGHT_MESSAGE_ID:
            {
                reconf->confirmNorthRight = true;
                neighborhood->addNeighbors();
                break;
            }
            case NEXTPLANECONFIRMATION_WESTLEFT_MESSAGE_ID:
            {
                reconf->confirmWestLeft = true;
                neighborhood->addNeighbors();
                break;
            }
            case NEXTPLANECONFIRMATION_WESTRIGHT_MESSAGE_ID:
            {
                reconf->confirmWestRight = true;
                neighborhood->addNeighbors();
                break;
            }
            case NEXTPLANECONFIRMATION_SOUTHLEFT_MESSAGE_ID:
            {
                reconf->confirmSouthLeft = true;
                neighborhood->addNeighbors();
                break;
            }
            case NEXTPLANECONFIRMATION_SOUTHRIGHT_MESSAGE_ID:
            {
                reconf->confirmSouthRight = true;
                neighborhood->addNeighbors();
                break;
            }
            case NEXTPLANECONFIRMATION_EASTLEFT_MESSAGE_ID:
            {
                reconf->confirmEastLeft = true;
                neighborhood->addNeighbors();
                break;
            }
            case NEXTPLANECONFIRMATION_EASTRIGHT_MESSAGE_ID:
            {
                reconf->confirmEastRight = true;
                neighborhood->addNeighbors();
                break;
            }
            case PLANE_FINISHED_MSG_ID:
            {
                reconf->childConfirm++;
                if (reconf->childConfirm == reconf->nChildren) {
                    if (!reconf->isPlaneParent) {
                        neighborMessages->sendMessagePlaneFinished();
                    }
                    else {
                        planeFinishedAck();
                    }
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(PLANE_FINISHED_TIME));
                break;
            }
            case PLANE_FINISHED_ACK_MSG_ID:
            {
                planeFinishedAck();
                std::this_thread::sleep_for(std::chrono::milliseconds(PLANE_FINISHED_TIME));
                break;
            }
            case PARENT_PLANE_FINISHED_MSG_ID:
            {
                neighborMessages->broadcastMessageParentPlaneFinished();
                std::this_thread::sleep_for(std::chrono::milliseconds(PLANE_FINISHED_TIME));
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
        uint64_t face = Catoms3DWorld::getWorld()->lattice->getOppositeDirection((std::static_pointer_cast<AddNeighborEvent>(pev))->face);
        if (!reconf->init)
            break;

        if (reconf->areNeighborsPlaced() && reconf->nChildren == 0)
            neighborMessages->sendMessagePlaneFinished();

        if (face == 1 || face == 4)
        {
            if (catom->getInterface(1)->isConnected() &&
                    catom->getInterface(4)->isConnected())
            {
                neighborhood->sendMessageToNextPlaneNorthRight();
            }
        }
        if (face == 1 || face == 5)
        {
            if (catom->getInterface(1)->isConnected() &&
                    catom->getInterface(5)->isConnected())
            {
                neighborhood->sendMessageToNextPlaneNorthLeft();
            }
        }
        if (face == 6 || face == 2)
        {
            if (catom->getInterface(6)->isConnected() &&
                    catom->getInterface(2)->isConnected())
            {
                neighborhood->sendMessageToNextPlaneWestLeft();
            }
        }
        if (face == 6 || face == 5)
        {
            if (catom->getInterface(6)->isConnected() &&
                    catom->getInterface(5)->isConnected())
            {
                neighborhood->sendMessageToNextPlaneWestRight();
            }
        }
        if (face == 7 || face == 5)
        {
            if (catom->getInterface(7)->isConnected() &&
                    catom->getInterface(5)->isConnected())
            {
                neighborhood->sendMessageToNextPlaneSouthLeft();
            }
        }
        if (face == 7 || face == 4)
        {
            if (catom->getInterface(7)->isConnected() &&
                    catom->getInterface(4)->isConnected())
            {
                neighborhood->sendMessageToNextPlaneSouthRight();
            }
        }
        if (face == 0 || face == 2)
        {
            if (catom->getInterface(0)->isConnected() &&
                    catom->getInterface(2)->isConnected())
            {
                neighborhood->sendMessageToNextPlaneEastLeft();
            }
        }
        if (face == 0 || face == 5)
        {
            if (catom->getInterface(0)->isConnected() &&
                    catom->getInterface(5)->isConnected())
            {
                neighborhood->sendMessageToNextPlaneEastRight();
            }
        }

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

void ReconfCatoms3DBlockCode::planeFinishedAck() {
    neighborMessages->sendMessagePlaneFinishedAck();
    if (reconf->isPlaneSeed()) {
        if(catom->getInterface(catom->position.addZ(1))->isConnected()) {
            neighborMessages->sendMessageParentPlaneFinished(catom->position.addZ(1));
        }
    }
}

BlockCode* ReconfCatoms3DBlockCode::buildNewBlockCode(BuildingBlock *host) {
    return (new ReconfCatoms3DBlockCode((Catoms3DBlock*)host));
}
