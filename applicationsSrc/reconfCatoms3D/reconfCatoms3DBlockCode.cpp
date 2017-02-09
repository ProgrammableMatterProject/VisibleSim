#include <iostream>
#include <sstream>
#include "reconfCatoms3DBlockCode.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "csgUtils.h"
#include "events.h"
#include "lattice.h"
#include "neighbors.h"

#define WAIT_TIME 10

using namespace std;
using namespace Catoms3D;

CSGNode* ReconfCatoms3DBlockCode::csgRoot = NULL;
BoundingBox ReconfCatoms3DBlockCode::boundingBox;
Seed *ReconfCatoms3DBlockCode::root = NULL;

ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
	cout << "ReconfCatoms3DBlockCode constructor" << endl;
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;
}

ReconfCatoms3DBlockCode::~ReconfCatoms3DBlockCode() {
	cout << "ReconfCatoms3DBlockCode destructor" << endl;
}

Vector3D gridToWorldPosition(const Cell3DPosition &pos) {
    Vector3D res;

    res.pt[3] = 1.0;
    res.pt[2] = M_SQRT2_2 * (pos[2] + 0.5);
    if (IS_EVEN(pos[2])) {
        res.pt[1] = (pos[1] + 0.5);
        res.pt[0] = (pos[0] + 0.5);
    } else {
        res.pt[1] = (pos[1] + 1.0);
        res.pt[0] = (pos[0] + 1.0);
    }

    return res;
}

Vector3D ReconfCatoms3DBlockCode::getWorldPosition(Cell3DPosition gridPosition) {
    Vector3D worldPosition = gridToWorldPosition(gridPosition);
    cout << "Position = " << worldPosition << endl;
    worldPosition.pt[0] += boundingBox.P0[0]; 
    worldPosition.pt[1] += boundingBox.P0[1]; 
    worldPosition.pt[2] += boundingBox.P0[2]; 
    return worldPosition;
}

void ReconfCatoms3DBlockCode::addNeighbors() {
    Neighbors neighbors;
    for (int i = 0; i < 12; i++) {
        Cell3DPosition neighborGridPos = catom->position;
        int *neighborPosPointer = (catom->position[2]%2) ? Neighbors::neighborDirectionsOdd[i] : Neighbors::neighborDirectionsEven[i];

        neighborGridPos.pt[0] += neighborPosPointer[0];
        neighborGridPos.pt[1] += neighborPosPointer[1];
        neighborGridPos.pt[2] += neighborPosPointer[2];
        if (neighborGridPos[2] != catom->position[2])
            continue;
        addNeighbor(neighborGridPos);
    }
}

void ReconfCatoms3DBlockCode::addNeighbor(Cell3DPosition pos) {
    Catoms3DWorld *world = Catoms3DWorld::getWorld();
    Neighbors neighbors;
    Color color;
    if (world->lattice->isFree(pos) && csgRoot->isInside(getWorldPosition(pos), color)) {
        if (neighbors.isPositionBlockable(pos))
            world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, pos, PINK, 0, false);
        else if (neighbors.isPositionBlocked(pos))
            world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, pos, RED, 0, false);
        else {
            world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, pos, BLACK, 0, false);
        }
        world->linkBlock(pos);
    }
}

bool ReconfCatoms3DBlockCode::needSync() {
    Color c;
    if (!csgRoot->isInside(getWorldPosition(catom->position.addY(-1)), c) &&
        csgRoot->isInside(getWorldPosition(catom->position.addX(-1)), c) &&
        csgRoot->isInside(getWorldPosition(catom->position.addX(-1).addY(-1)), c) )
        return true;
    return false;
}

void ReconfCatoms3DBlockCode::addNeighborsOnXAxis() {
    Color color;
    for (int i = 0; i < 2; i++) {
        Cell3DPosition neighborGridPos = catom->position;
        neighborGridPos.pt[0] += (i == 0 ? 1 : -1);
        addNeighbor(neighborGridPos);
    }
}

void ReconfCatoms3DBlockCode::startup() {
    Color color;
    leftCompleted = rightCompleted = false;
    syncRequest.setCatom(catom);
    syncResponse.setCatom(catom);

	if (catom->blockId==1) {
        csgRoot = csgUtils.readFile("data/mug.bc");
        csgRoot->toString();
        csgRoot->boundingBox(boundingBox);
        // TODO fix bounding box precision
        boundingBox.P0.pt[0] -= 1;
        boundingBox.P0.pt[1] -= 1;
        boundingBox.P0.pt[2] -= 1;
        boundingBox.P1.pt[0] -= 1;
        boundingBox.P1.pt[1] -= 1;
        boundingBox.P1.pt[2] -= 1;
        cout << "Bounding box: " << boundingBox.P0 << ' ' << boundingBox.P1 << endl;
        worldPosition = getWorldPosition(catom->position);
        if (csgRoot->isInside(worldPosition, color)) {
            catom->setColor(color);
            //root = new Seed(catom->blockId, SEED_DIRECTION::UP);
            //root = new Seed(catom->blockId, SEED_DIRECTION::DOWN);
        }
        else {
            catom->setColor(RED);
        }
        currentLine = catom->position[1];
	}
    if (catom->getInterface(0)->connectedInterface == NULL && catom->getInterface(6)->connectedInterface == NULL) {
        lineParent = catom->blockId;
        currentLine = catom->position[1];
    }
    isSeed();
    sendMessageCompletedSide(SIDE_COMPLETED::LEFT);
    sendMessageCompletedSide(SIDE_COMPLETED::RIGHT);
    // First catom of the line
    bool DEBUG = true;
    if (DEBUG) {
        if (catom->blockId == 167) {
            set<bID> visitedSeeds;
            visitedSeeds.insert(167);
            std::this_thread::sleep_for(std::chrono::milliseconds(1000));
            syncRequest.requestSyncLineDown(167, 10, visitedSeeds, lineSeeds, lineParent);
        }
        else 
        {
            if (catom->getInterface(0)->connectedInterface == NULL && catom->getInterface(6)->connectedInterface == NULL) {
                addNeighborsOnXAxis();
            }
            else {
                sendMessageToGetNeighborInformation();
            }
        }
    } // END OF DEBUG 
    else {
        if (catom->getInterface(0)->connectedInterface == NULL && catom->getInterface(6)->connectedInterface == NULL) {
                addNeighborsOnXAxis();
            }
            else {
                sendMessageToGetNeighborInformation();
            }
        }

    std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
}

bool ReconfCatoms3DBlockCode::isSeed() {
    Color color;
    if (catom->blockId == 145)
        return false;
    //Intern seed
    if (!csgRoot->isInside(getWorldPosition(catom->position.addX(1).addY(1)), color) && csgRoot->isInside(getWorldPosition(catom->position.addY(1)),color) ){
        return true;
    }
    //Right border seed
    if (lineSeeds.empty() &&
        !csgRoot->isInside(getWorldPosition(catom->position.addX(1)), color) && 
        csgRoot->isInside(getWorldPosition(catom->position.addY(1)), color)) {
        return true;
    }
    return false;
}

void ReconfCatoms3DBlockCode::sendMessageToGetNeighborInformation()
{
    for (int i = 0; i < 2; i++) {
        Cell3DPosition neighborPosition = (i == 0) ? catom->position.addX(-1) : catom->position.addX(1);
        if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
            New_catom_message *msg = new New_catom_message;
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, msg, catom->getInterface(neighborPosition)));
        }
    }
}

void ReconfCatoms3DBlockCode::sendMessageCompletedSide(SIDE_COMPLETED side)
{
    Color color;
    int offset = (side == LEFT) ? 1 : -1;
    Cell3DPosition neighborPosition = catom->position.addX(offset);

    if (!csgRoot->isInside(getWorldPosition(catom->position.addX(offset*-1)), color)){
        if (side == LEFT) {leftCompleted = true; catom->setColor(YELLOW);}
        else {rightCompleted = true;
        catom->setColor(WHITE);}

        if (catom->getInterface(neighborPosition)->connectedInterface != NULL) {
            Message *msg;
            if (side == LEFT) msg = new Left_side_completed_message(lineSeeds);
            else msg = new Right_side_completed_message(lineSeeds);

            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, msg, catom->getInterface(neighborPosition)));
            std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
        }
    }
}

void ReconfCatoms3DBlockCode::tryAddNextLineNeighbor() {
    if  (isSeed()) {
        lineSeeds.insert(catom->blockId);
        addNeighbor(catom->position.addY(1));
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
                New_catom_response_message *msg = new New_catom_response_message;
                msg->currentLine = currentLine;
                msg->lineParent = lineParent;
                msg->lineSeeds = lineSeeds;
                msg->leftCompleted = leftCompleted;
                msg->rightCompleted = rightCompleted;
                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, msg, message->destinationInterface));
                break;
            }
            case NEW_CATOM_RESPONSE_MSG_ID:
            {
                shared_ptr<New_catom_response_message> recv_message = static_pointer_cast<New_catom_response_message>(message);
                currentLine = recv_message->currentLine;
                lineParent = recv_message->lineParent;
                lineSeeds = recv_message->lineSeeds;
                leftCompleted = recv_message->leftCompleted || leftCompleted;
                rightCompleted = recv_message->rightCompleted || rightCompleted;

                catom->setColor(RED);
                std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));

                if ((leftCompleted || rightCompleted) && !(leftCompleted && rightCompleted)) 
                    catom->setColor(BLUE);
                if (leftCompleted && rightCompleted) {
                    catom->setColor(DARKORANGE);
                    tryAddNextLineNeighbor();
                }

                addNeighborsOnXAxis();
                break;
            }
            case LEFT_SIDE_COMPLETED_MSG_ID:
            {
                leftCompleted = true;
                shared_ptr<Left_side_completed_message> recv_message = static_pointer_cast<Left_side_completed_message>(message);
                lineSeeds.insert(recv_message->seeds.begin(), recv_message->seeds.end());
                if (catom->getInterface(catom->position.addX(1))->connectedInterface != NULL) {
                    Left_side_completed_message *msg = new Left_side_completed_message(lineSeeds);
                    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, msg, catom->getInterface(catom->position.addX(1))));
                }
                catom->setColor(BLUE);
                if (rightCompleted) 
                {
                    catom->setColor(DARKORANGE);
                    tryAddNextLineNeighbor();
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
                break;
            }
            case RIGHT_SIDE_COMPLETED_MSG_ID:
            {
                rightCompleted = true;
                shared_ptr<Right_side_completed_message> recv_message = static_pointer_cast<Right_side_completed_message>(message);
                lineSeeds.insert(recv_message->seeds.begin(), recv_message->seeds.end());
                if (catom->getInterface(catom->position.addX(-1))->connectedInterface != NULL) {
                    Right_side_completed_message *msg = new Right_side_completed_message(lineSeeds);
                    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 10, msg, catom->getInterface(catom->position.addX(-1))));
                }
                catom->setColor(BLUE);
                if (leftCompleted) {
                    catom->setColor(DARKORANGE);
                    tryAddNextLineNeighbor();
                }
                
                break;
            }
            case FIND_LINE_PARENT_SYNC_MESSAGE:
            {
                shared_ptr<Find_line_parent_sync_message> recv_message = static_pointer_cast<Find_line_parent_sync_message>(message);
                syncRequest.requestSyncLineDown(recv_message->requestCatomID, recv_message->requestLine, recv_message->visitedSeeds, lineSeeds, lineParent);
                break;
            }
            case FIND_LINE_SEED_SYNC_MESSAGE:
            {
                shared_ptr<Find_line_seed_sync_message> recv_message = static_pointer_cast<Find_line_seed_sync_message>(message);
                syncRequest.requestSyncLineUp(recv_message->requestCatomID, recv_message->requestLine, recv_message->visitedSeeds, lineSeeds, lineParent);
                break;
            }
            case LINE_DOWN_SYNC_MESSAGE_ID:
            {
                shared_ptr<Line_down_sync_message> recv_message = static_pointer_cast<Line_down_sync_message>(message);
                set<bID> newLineVisitedSeeds;
                newLineVisitedSeeds.insert(catom->blockId);
                syncRequest.requestSyncLineDown(recv_message->requestCatomID, recv_message->requestLine, newLineVisitedSeeds, lineSeeds, lineParent);
                catom->setColor(MAGENTA);
                std::this_thread::sleep_for(std::chrono::milliseconds(1000));
                break;
            }
            case LINE_UP_SYNC_MESSAGE_ID:
            {
                //TODO test if conversion works
                shared_ptr<Line_up_sync_message> recv_message = static_pointer_cast<Line_up_sync_message>(message);
                set<bID> newLineVisitedSeeds;
                newLineVisitedSeeds.insert(catom->blockId);
                syncRequest.requestSyncLineUp(recv_message->requestCatomID, recv_message->requestLine, newLineVisitedSeeds, lineSeeds, lineParent);
                catom->setColor(RED);
                std::this_thread::sleep_for(std::chrono::milliseconds(1000));
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


