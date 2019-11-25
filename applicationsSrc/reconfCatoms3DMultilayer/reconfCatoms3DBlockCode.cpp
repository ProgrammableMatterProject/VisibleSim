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
    if (host == NULL) return;

    scheduler = getScheduler();
    catom = (Catoms3DBlock*)hostBlock;

    world = Catoms3DWorld::getWorld();
    lattice = world->lattice;

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
        cout << "HIGHLIGHT_CSG: " << HIGHLIGHT_CSG << endl;
        highlightCSG();

        //srand(time(NULL));
        reconf->floor = 0;

        // const Cell3DPosition& gs = BaseSimulator::getWorld()->lattice->gridSize;
        // Cell3DPosition pos;
        // short iz = 2;
        // for (short iy = - iz / 2; iy < gs[1] - iz / 2; iy++) {
        //     for (short ix = - iz / 2; ix < gs[0] - iz / 2; ix++) {
        //         pos.set(ix, iy, iz);

        //         lattice->highlightCell(pos, BLACK);
        //     }
        // }
    }

    if (not target->isInTarget(catom->position)) return;

    planningRun();
    //stochasticRun();
    // neighborhood->addAllNeighbors();

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
    // int nbBlocks = World::getWorld()->getNbBlocks();
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

bool ReconfCatoms3DBlockCode::parseUserCommandLineArgument(int &argc, char **argv[]) {
    /* Reading the command line */
    if ((argc > 0) && ((*argv)[0][0] == '-')) {
        switch((*argv)[0][1]) {

            // case 'b':   {
            //     BUILDING_MODE = true;
            //     return true;
            // } break;

            case '-': {
                string varg = string((*argv)[0] + 2); // argv[0] without "--"
                if (varg == string("csg")) HIGHLIGHT_CSG = true;
                else return false;

                return true;
            }

            default:
                cerr << "Unrecognized command line argument: " << (*argv)[0] << endl;
        }
    }

    return false;
}

void ReconfCatoms3DBlockCode::highlightCSG() {
    if (not HIGHLIGHT_CSG)
        return;

    // Initialize Target Object Preview
    const Cell3DPosition& gs = world->lattice->gridSize;
    Cell3DPosition pos;
    for (short iz = 0; iz < gs[2]; iz++) {
        for (short iy = 0; iy < gs[1]; iy++) {
            for (short ix = 0; ix < gs[0]; ix++) {

        // for (short iy = - iz / 2; iy < gs[1] - iz / 2; iy++) {
        //     for (short ix = - iz / 2; ix < gs[0] - iz / 2; ix++) {
                pos.set(ix, iy, iz);

                if (HIGHLIGHT_CSG and target->isInTarget(pos))
                    lattice->highlightCell(pos, WHITE);
            }
        }
    }
}

// void ReconfCatoms3DBlockCode::initializeSandbox() {
//     const Cell3DPosition& ulb = lattice->getGridUpperBounds();
//     const Cell3DPosition sbSeedPos = Cell3DPosition(3, 3, 3);

//     for (int x = sbSeedPos[0]; x < ulb[0]; x+=B) {
//         for (int y = sbSeedPos[1]; y < ulb[1]; y+=B) {
//             const Cell3DPosition& trPos = Cell3DPosition(x, y, sbSeedPos[2]);

//             for (int i = 0; i < XBranch; i++) {
//                 Cell3DPosition pos = trPos;
//                 for (int j = 0; j < 3; j++) {
//                     pos += rm->getIncidentTipRelativePos((BranchIndex)i);

//                     if (lattice->isInGrid(pos)) {
//                         world->addBlock(0, buildNewBlockCode, pos, GREY);
//                     }
//                 }
//             }

//             if (trPos != sbSeedPos) { // or i != ZBranch)
//                 Cell3DPosition futureTRPos = trPos
//                     + rm->getEntryPointRelativePos(Z_EPL);

//                 if (lattice->isInGrid(futureTRPos) and rm->isInCSGScaffold(norm(trPos)))
//                     world->addBlock(0, buildNewBlockCode, futureTRPos, YELLOW);
//             }
//         }
//     }
// }
