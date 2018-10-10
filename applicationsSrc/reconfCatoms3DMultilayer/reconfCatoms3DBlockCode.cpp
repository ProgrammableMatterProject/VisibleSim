#include <iostream>
#include "reconfCatoms3DBlockCode.h"
#include "catoms3DWorld.h"

#define CONSTRUCT_WAIT_TIME 0
#define SYNC_WAIT_TIME 0
#define SYNC_RESPONSE_TIME SYNC_WAIT_TIME
#define PLANE_WAIT_TIME 0
#define PLANE_FINISHED_TIME 0

using namespace std;
using namespace Catoms3D;

ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;

    reconf = new Reconf(catom);

    syncNext = new SyncNext(catom, reconf);
    syncPrevious = new SyncPrevious(catom, reconf);

    neighborhood = new Neighborhood(catom, reconf, syncNext, syncPrevious, buildNewBlockCode);
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

    //planningRun();
    //stochasticRun();
    neighborhood->addAllNeighbors();

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
            case SYNCNEXT_MESSAGE_ID:
            {
                shared_ptr<Sync_message> recv_message = static_pointer_cast<Sync_message>(message);
                syncNextMessage(recv_message);
                break;
            }
            case SYNCPREVIOUS_MESSAGE_ID:
            {
                shared_ptr<Sync_message> recv_message = static_pointer_cast<Sync_message>(message);
                syncPreviousMessage(recv_message);
                break;
            }
            case SYNCNEXT_RESPONSE_MESSAGE_ID:
            case SYNCPREVIOUS_RESPONSE_MESSAGE_ID:
            {
                shared_ptr<Sync_response_message> recv_message = static_pointer_cast<Sync_response_message>(message);
                syncResponse(recv_message);
                break;
            }
            case CANFILLLEFTRESPONSE_MESSAGE_ID:
            {
                reconf->canFillLeft = true;
                neighborhood->addNeighbors();
                break;
            }
            case CANFILLRIGHTRESPONSE_MESSAGE_ID:
            {
                reconf->canFillRight = true;
                neighborhood->addNeighbors();
                break;
            }
            case CANFILLNEXTFLOOR_MESSAGE_ID:
            {
                reconf->canFillNextFloor = true;
                neighborhood->addNeighbors();
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
                neighborhood->addNeighbors();
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
        neighborhood->addNeighbor(catom->position.addY(1));
        //getStats();
        break;
    }
    case ADDPREVIOUSLINE_EVENT_ID: {
        neighborhood->addNeighbor(catom->position.addY(-1));
        //getStats();
        break;
    }
    case ADDNEXTPLANE_EVENT_ID: {
        neighborhood->addNeighbor(catom->position.addZ(1));
        //getStats();
        break;
    }
    case ADDPREVIOUSPLANE_EVENT_ID: {
        neighborhood->addNeighbor(catom->position.addZ(-1));
        //getStats();
        break;
    }
    case EVENT_ADD_NEIGHBOR: {
        uint64_t face = Catoms3DWorld::getWorld()->lattice->getOppositeDirection((std::static_pointer_cast<AddNeighborEvent>(pev))->face);
        if (!reconf->init)
            break;

        if (reconf->areNeighborsPlaced() && reconf->nChildren == 0)
            neighborMessages->sendMessagePlaneFinished();

        // Add neighbors on same plane next line
        if (face == 1 || face == 6)
        {
            if (catom->getInterface(1)->isConnected() &&
                    catom->getInterface(6)->isConnected())
            {
                neighborhood->sendResponseMessageToAddLeft();
            }
        }
        if (face == 7 || face == 0)
        {
            if (catom->getInterface(7)->isConnected() &&
                    catom->getInterface(0)->isConnected())
            {
                neighborhood->sendResponseMessageToAddRight();
            }
        }

        // Rules to allow next plane to add neighbor
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

        if (catom->position[2]%2) {
            if (face == 6 || face == 1)
                neighborhood->sendMessageCanFillNextFloor();
        }
        else {
            if (face == 0 || face == 7)
                neighborhood->sendMessageCanFillNextFloor();
        }

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
    //cout << nbBlocks*100/30729 << ';' << count << ';' << nbBlocks << ';' << nMessages << ';' << getScheduler()->now() << endl;
}

void ReconfCatoms3DBlockCode::planeFinishedAck() {
    neighborMessages->sendMessagePlaneFinishedAck();
    if (reconf->isPlaneSeed()) {
        if(catom->getInterface(catom->position.addZ(1))->isConnected()) {
            neighborMessages->sendMessageParentPlaneFinished(catom->position.addZ(1));
        }
    }
}

void ReconfCatoms3DBlockCode::syncNextMessage(shared_ptr<Sync_message> recv_message)
{
    if (syncNext->needSyncToRight() &&
            catom->position[1] == recv_message->goal[1] &&
            catom->position[0] <= recv_message->goal[0]) {
        syncNext->response(recv_message->origin);
    }
    else if (syncPrevious->needSyncToLeft() &&
            catom->position[1] < recv_message->goal[1]) {
        syncNext->response(recv_message->origin);
    }
    else {
        if (syncNext->needSyncToRight() &&
                catom->position[1] < recv_message->goal[1]) {
            neighborhood->addNextLineNeighbor();
        }
        syncNext->handleMessage(recv_message);
        std::this_thread::sleep_for(std::chrono::milliseconds(SYNC_WAIT_TIME));
    }
}

void ReconfCatoms3DBlockCode::syncPreviousMessage(shared_ptr<Sync_message> recv_message)
{
    if (syncPrevious->needSyncToLeft() &&
            catom->position[1] == recv_message->goal[1] &&
            catom->position[0] >= recv_message->goal[0]) {
        syncPrevious->response(recv_message->origin);
    }
    else if (syncNext->needSyncToRight() &&
            catom->position[1] > recv_message->goal[1]) {
        syncPrevious->response(recv_message->origin);
    }
    else {
        if (syncPrevious->needSyncToLeft() &&
                catom->position[1] > recv_message->goal[1]) {
            neighborhood->addPreviousLineNeighbor();
        }
        syncPrevious->handleMessage(recv_message);
        std::this_thread::sleep_for(std::chrono::milliseconds(SYNC_WAIT_TIME));
    }
}

void ReconfCatoms3DBlockCode::syncResponse(shared_ptr<Sync_response_message> recv_message)
{
    if (recv_message->origin == catom->position) {
        neighborhood->addNeighborToLeft();
        neighborhood->addNeighborToRight();
    }
    else {
        syncNext->handleMessageResponse(recv_message);
        std::this_thread::sleep_for(std::chrono::milliseconds(SYNC_RESPONSE_TIME));
    }
}

BlockCode* ReconfCatoms3DBlockCode::buildNewBlockCode(BuildingBlock *host) {
    return (new ReconfCatoms3DBlockCode((Catoms3DBlock*)host));
}
