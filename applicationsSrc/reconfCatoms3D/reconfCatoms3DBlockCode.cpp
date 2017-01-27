/*
 * reconfCatoms3DBlockCode.cpp
 *
 *  Created on: 17 October 2016
 *  Author: Thadeu
 */

#include <iostream>
#include <sstream>
#include "reconfCatoms3DBlockCode.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "csgUtils.h"
#include "events.h"
#include "lattice.h"
#include "neighbors.h"

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
	if (catom->blockId==1) {
        csgRoot = csgUtils.readFile("data/mug.bc");
        csgRoot->toString();
            csgRoot->boundingBox(boundingBox);
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
	}
    isSeed();
    sendMessageCompletedSide(SIDE_COMPLETED::LEFT);
    sendMessageCompletedSide(SIDE_COMPLETED::RIGHT);
    if (catom->getInterface(0)->connectedInterface == NULL && catom->getInterface(6)->connectedInterface == NULL)
        addNeighborsOnXAxis();
    sendMessageToGetNeighborInformation();
    

    std::this_thread::sleep_for(std::chrono::milliseconds(100));

//    addNeighbors();
}

bool ReconfCatoms3DBlockCode::isSeed() {
    Color color;
    //Intern seed
    if (!csgRoot->isInside(getWorldPosition(catom->position.addX(1).addY(1)), color) && csgRoot->isInside(getWorldPosition(catom->position.addY(1)),color) ){
        lineSeeds.insert(catom->blockId);
        return true;
    }
    //Right border seed
    if (lineSeeds.empty() &&
        !csgRoot->isInside(getWorldPosition(catom->position.addX(1)), color) && 
        csgRoot->isInside(getWorldPosition(catom->position.addY(1)), color))
        return true;
    return false;
}

void ReconfCatoms3DBlockCode::sendMessageToGetNeighborInformation()
{
    for (int i = 0; i < 2; i++) {
        int side = (i == 0) ? 6 : 0; // X neighbors interface TODO: dont depend on catom side
        if (catom->getInterface(side)->connectedInterface != NULL) {
            cout << catom->blockId << endl;
            New_catom_message *msg = new New_catom_message;
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, msg, catom->getInterface(side)));
        }
    }
}
void ReconfCatoms3DBlockCode::sendMessageCompletedSide(SIDE_COMPLETED side)
{
    Color color;
    int offset = (side == LEFT) ? -1 : 1;

    if (!csgRoot->isInside(getWorldPosition(catom->position.addX(offset)), color)){
        if (side == LEFT) {leftCompleted = true; catom->setColor(YELLOW);}
        else {rightCompleted = true;
        catom->setColor(WHITE);}

        if (catom->getInterface(side)->connectedInterface != NULL) {
            Message *msg;
            if (side == LEFT) msg = new Left_side_completed_message(lineSeeds);
            else msg = new Right_side_completed_message(lineSeeds);

            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, msg, catom->getInterface(side)));
            cout << "MASTER" << endl;
            std::this_thread::sleep_for(std::chrono::milliseconds(1000));
        }
    }
}

void ReconfCatoms3DBlockCode::addNextLineNeighbor() {
    Color color;
    if (isSeed()){
        Catoms3DWorld::getWorld()->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, catom->position.addY(1), BLACK, 0, false);
        Catoms3DWorld::getWorld()->linkBlock(catom->position.addY(1));
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
                msg->lineSeeds = lineSeeds;
                msg->leftCompleted = leftCompleted;
                msg->rightCompleted = rightCompleted;
                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 10, msg, message->destinationInterface));
                break;
            }
            case NEW_CATOM_RESPONSE_MSG_ID:
            {
                shared_ptr<New_catom_response_message> recv_message = static_pointer_cast<New_catom_response_message>(message);
                lineSeeds = recv_message->lineSeeds;
                leftCompleted = recv_message->leftCompleted || leftCompleted;
                rightCompleted = recv_message->rightCompleted || rightCompleted;
                catom->setColor(RED);
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
                if ((leftCompleted || rightCompleted) && !(leftCompleted && rightCompleted)) 
                    catom->setColor(BLUE);
                if (leftCompleted && rightCompleted) {
                    catom->setColor(DARKORANGE);
                    addNextLineNeighbor();
                }

                addNeighborsOnXAxis();
                break;
            }
            case LEFT_SIDE_COMPLETED_MSG_ID:
            {
                leftCompleted = true;
                shared_ptr<Left_side_completed_message> recv_message = static_pointer_cast<Left_side_completed_message>(message);
                lineSeeds.insert(recv_message->seeds.begin(), recv_message->seeds.end());
                if (catom->getInterface(0)->connectedInterface != NULL) {
                    Left_side_completed_message *msg = new Left_side_completed_message(lineSeeds);
                    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, msg, catom->getInterface(0)));
                }
                catom->setColor(BLUE);
                if (rightCompleted) 
                {
                    cout << "-------------" << endl;
                    catom->setColor(DARKORANGE);
                    addNextLineNeighbor();
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
                break;
            }
            case RIGHT_SIDE_COMPLETED_MSG_ID:
            {
                rightCompleted = true;
                shared_ptr<Right_side_completed_message> recv_message = static_pointer_cast<Right_side_completed_message>(message);
                lineSeeds.insert(recv_message->seeds.begin(), recv_message->seeds.end());
                if (catom->getInterface(6)->connectedInterface != NULL) {
                    Right_side_completed_message *msg = new Right_side_completed_message(lineSeeds);
                    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 10, msg, catom->getInterface(6)));
                }
                catom->setColor(BLUE);
                if (leftCompleted) {
                    catom->setColor(DARKORANGE);
                    addNextLineNeighbor();
                }
                
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


