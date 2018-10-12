#include <iostream>
#include "reconfCatoms3DBlockCode.h"
#include "catoms3DWorld.h"

#define CONSTRUCT_WAIT_TIME 0
#define SYNC_WAIT_TIME 0
#define SYNC_RESPONSE_TIME SYNC_WAIT_TIME
#define PLANE_FINISHED_TIME 0

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
    syncPlaneManager = new SyncPlaneManager(catom, reconf, syncPlane, neighborhood, neighborMessages);
}

ReconfCatoms3DBlockCode::~ReconfCatoms3DBlockCode() {
    delete reconf;
    delete neighborhood;
    delete neighborMessages;
    delete syncNext;
    delete syncPrevious;
    delete syncPlane;
    delete syncPlaneManager;
}

void ReconfCatoms3DBlockCode::startup() {
    //if (catom->blockId == 1)
        //srand(time(NULL));

    /* Run with the planning algorithm */
    planningRun();

    /* Stochastically filling */
    //stochasticRun();

    /* Fill in a FIFO order */
    //neighborhood->addAllNeighbors();

    /* Run with a centralized node that keeps a register of planes in a tree format */
    //startTree();

    std::this_thread::sleep_for(std::chrono::milliseconds(CONSTRUCT_WAIT_TIME));
}

void ReconfCatoms3DBlockCode::planningRun() {
    if (neighborhood->isFirstCatomOfPlane()) {
        reconf->isPlaneParent = true;
        if (catom->position[2]%2)
            reconf->interfaceParent = catom->BaseSimulator::BuildingBlock::getInterface(8)->connectedInterface;
        else
            reconf->interfaceParent = catom->BaseSimulator::BuildingBlock::getInterface(10)->connectedInterface;

        neighborMessages->init();
    }
    else if (neighborhood->isFirstCatomOfLine()) {
        neighborMessages->sendMessageToGetParentInfo();
    }
    else {
        neighborMessages->sendMessageToGetLineInfo();
    }
}

void ReconfCatoms3DBlockCode::startTree() {
    if (catom->blockId == 1) {
        SyncPlane_node_manager::root->planeNumber = catom->position[2];
        reconf->syncPlaneNode = SyncPlane_node_manager::root;
        reconf->syncPlaneNodeParent = SyncPlane_node_manager::root;
    }
    else {
        if (Catoms3DWorld::getWorld()->getBlockByPosition(catom->position.addZ(-1)) != NULL) {
            ReconfCatoms3DBlockCode *neighborBlockCode = (ReconfCatoms3DBlockCode*)Catoms3DWorld::getWorld()->getBlockByPosition(catom->position.addZ(-1))->blockCode;
            reconf->syncPlaneNodeParent = neighborBlockCode->reconf->syncPlaneNode;
        }
    }
}

void ReconfCatoms3DBlockCode::stochasticRun() {
    for (int i = 0; i < 100000; i++) {
        ReconfCatoms3DBlockCode *catom = (ReconfCatoms3DBlockCode*)Catoms3D::getWorld()->getBlockById(rand()%Catoms3D::getWorld()->getSize() + 1)->blockCode;
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
        if (message->isMessageHandleable()) {
            shared_ptr<HandleableMessage> hm = static_pointer_cast<HandleableMessage>(message);
            int connectedBlockId = message->sourceInterface->getConnectedBlockId();
            BlockCode *block = World::getWorld()->getBlockById(connectedBlockId)->blockCode;
            hm->handle(block);
        }
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
            case PLANE_FINISHED_MSG_ID:
            {
                break;
            }
            case PLANE_FINISHED_ACK_MSG_ID:
            {
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
          }
      }
      break;
    case EVENT_ADD_NEIGHBOR: {
        uint64_t face = Catoms3DWorld::getWorld()->lattice->getOppositeDirection((std::static_pointer_cast<AddNeighborEvent>(pev))->face);
        if (!reconf->init)
            break;

        if (reconf->areNeighborsPlaced() && reconf->nChildren == 0) {
            neighborMessages->sendMessagePlaneFinished();
        }

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
        break;
    }
    case ADDLEFTBLOCK_EVENT_ID: {
        neighborhood->addNeighbor(catom->position.addX(-1));
        getStats();
        break;
    }
    case ADDRIGHTBLOCK_EVENT_ID: {
        neighborhood->addNeighbor(catom->position.addX(1));
        getStats();
        break;
    }
    case ADDNEXTLINE_EVENT_ID: {
        neighborhood->addNeighbor(catom->position.addY(1));
        getStats();
        break;
    }
    case ADDPREVIOUSLINE_EVENT_ID: {
        neighborhood->addNeighbor(catom->position.addY(-1));
        getStats();
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
    nMessages += Sync::nMessagesSyncResponse*2;
    //cout << nbBlocks*100/30729 << ';' << count << ';' << nbBlocks << ';' << nMessages <<  ';' << getScheduler()->now() << endl;
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

void ReconfCatoms3DBlockCode::planeContinue()
{
    int continueBlockId = SyncPlane_node_manager::root->canContinue(catom->position[2]);
    if (continueBlockId != 0) {
        ReconfCatoms3DBlockCode* otherCatom = (ReconfCatoms3DBlockCode*)Catoms3D::getWorld()->getBlockById(continueBlockId)->blockCode;
        otherCatom->neighborhood->addNeighborToNextPlane();
    }
    else {
        int nextId = SyncPlane_node_manager::root->isOk(catom->position[2]+1);
        if (nextId != 0) {
            ReconfCatoms3DBlockCode* otherCatom = (ReconfCatoms3DBlockCode*)Catoms3D::getWorld()->getBlockById(nextId)->blockCode;
            otherCatom->neighborhood->addNeighborToNextPlane();
        }
    }
}

void ReconfCatoms3DBlockCode::removeSeed()
{
    if (!reconf->isPlaneParent) {
        if (catom->getInterface(catom->position.addZ(-1))->isConnected()) {
            ReconfCatoms3DBlockCode* otherCatom = (ReconfCatoms3DBlockCode*)Catoms3D::getWorld()->getBlockByPosition(catom->position.addZ(-1))->blockCode;
            SyncPlane_node_manager::root->remove(otherCatom->reconf->syncPlaneNode, otherCatom->reconf->syncPlaneNodeParent);
        }
    }
}

BlockCode* ReconfCatoms3DBlockCode::buildNewBlockCode(BuildingBlock *host) {
    return (new ReconfCatoms3DBlockCode((Catoms3DBlock*)host));
}
