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
queue<int> ReconfCatoms3DBlockCode::catomQueue;
BoundingBox ReconfCatoms3DBlockCode::boundingBox;
Seed *root = NULL;

ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
	cout << "ReconfCatoms3DBlockCode constructor" << endl;
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;
    world = (Catoms3DWorld*)Catoms3DWorld::getWorld();
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

Vector3D ReconfCatoms3DBlockCode::getWorldPosition(BoundingBox boundingBox, Cell3DPosition gridPosition) {
    Vector3D worldPosition = gridToWorldPosition(gridPosition);
    cout << "Position = " << worldPosition << endl;
    worldPosition.pt[0] += boundingBox.P0[0]; 
    worldPosition.pt[1] += boundingBox.P0[1]; 
    worldPosition.pt[2] += boundingBox.P0[2]; 
    return worldPosition;
}

Cell3DPosition rightPosition(Cell3DPosition actualPosition) {
    Cell3DPosition nextPosition = actualPosition;
    nextPosition.pt[0] += 1;
    return nextPosition;
}
Cell3DPosition upperPosition(Cell3DPosition actualPosition) {
    Cell3DPosition nextPosition = actualPosition;
    nextPosition.pt[1] += 1;
    return nextPosition;
}
Cell3DPosition rightUpperPosition(Cell3DPosition actualPosition) {
    Cell3DPosition nextPosition = actualPosition;
    nextPosition.pt[0] += 1;
    nextPosition.pt[1] += 1;
    return nextPosition;
}
Cell3DPosition leftUpperPosition(Cell3DPosition actualPosition) {
    Cell3DPosition nextPosition = actualPosition;
    nextPosition.pt[0] -= 1;
    nextPosition.pt[1] += 1;
    return nextPosition;
}

void ReconfCatoms3DBlockCode::addLine() {
    Color color;
    Neighbors neighbors(world);
    for (int i = 0; i < 2; i++) {
        Cell3DPosition neighborGridPos = catom->position;
        if (i == 0) { neighborGridPos.pt[0] += 1;} else neighborGridPos.pt[0] -=1;

        if (world->lattice->isFree(neighborGridPos) && csgRoot->isInside(getWorldPosition(boundingBox, neighborGridPos), color)) {
            if (neighbors.isPositionBlockable(neighborGridPos)) {
                std::this_thread::sleep_for(std::chrono::milliseconds(1000));
                cout << "BLOCKABLE BLOCK AT POSITION " << neighborGridPos << endl;
                world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, neighborGridPos, PINK, 0, false);
                world->linkBlock(neighborGridPos);
                std::this_thread::sleep_for(std::chrono::milliseconds(1000));
            }
            else if (neighbors.isPositionBlocked(neighborGridPos)) {
                std::this_thread::sleep_for(std::chrono::milliseconds(1000));
                cout << "IMPOSSIBLE BLOCK AT POSITION " << neighborGridPos << endl;
                world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, neighborGridPos, RED, 0, false);
                world->linkBlock(neighborGridPos);
                std::this_thread::sleep_for(std::chrono::milliseconds(1000));
            }
            else {
                if (!csgRoot->isInside(rightPosition(neighborGridPos), color)) 
                    color = WHITE;
                else 
                    color = BLACK;
                world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, neighborGridPos, color, 0, false);
                world->linkBlock(neighborGridPos);
                int neighborId = world->lattice->getBlock(neighborGridPos)->blockId;
                catomQueue.push(neighborId);
                if (!csgRoot->isInside(rightUpperPosition(neighborGridPos), color))  {
                    world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, rightUpperPosition(neighborGridPos), color, 0, false);
                    world->linkBlock(rightUpperPosition(neighborGridPos));
                }
                else if (!csgRoot->isInside(upperPosition(neighborGridPos), color))  {
                    world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, upperPosition(neighborGridPos), color, 0, false);
                    world->linkBlock(upperPosition(neighborGridPos));

                }
                else if (!csgRoot->isInside(leftUpperPosition(neighborGridPos), color))  {
                    world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, leftUpperPosition(neighborGridPos), color, 0, false);
                    world->linkBlock(leftUpperPosition(neighborGridPos));

                }
            }
        }
    }
}

/*
void ReconfCatoms3DBlockCode::addNeighbors() {
    Neighbors neighbors(world);
    for (int i = 0; i < 12; i++) {
        Cell3DPosition neighborGridPos = catom->position;
        int *neighborPosPointer = (catom->position[2]%2) ? Neighbors::neighborDirectionsOdd[i] : Neighbors::neighborDirectionsEven[i];

        // to get only the same plane
        if (neighborPosPointer[2] != 0) 
            continue;         

        neighborGridPos.pt[0] += neighborPosPointer[0];
        neighborGridPos.pt[1] += neighborPosPointer[1];
        neighborGridPos.pt[2] += neighborPosPointer[2];
        if (world->lattice->isFree(neighborGridPos)) {
            Color color;
            Vector3D newWorldPos = getWorldPosition(boundingBox, neighborGridPos);
            if (csgRoot->isInBorder(newWorldPos, color, 1.5)) {
                if (neighbors.isPositionBlockable(neighborGridPos)) {
                    std::this_thread::sleep_for(std::chrono::milliseconds(1000));
                    cout << "BLOCKABLE BLOCK AT POSITION " << neighborGridPos << endl;
                    world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, neighborGridPos, PINK, 0, false);
                    world->linkBlock(neighborGridPos);
                    std::this_thread::sleep_for(std::chrono::milliseconds(1000));
                }
                else if (neighbors.isPositionBlocked(neighborGridPos)) {
                    std::this_thread::sleep_for(std::chrono::milliseconds(1000));
                    cout << "IMPOSSIBLE BLOCK AT POSITION " << neighborGridPos << endl;
                    world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, neighborGridPos, RED, 0, false);
                    world->linkBlock(neighborGridPos);
                    std::this_thread::sleep_for(std::chrono::milliseconds(1000));
                }
                else {
                    world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, neighborGridPos, color, 0, false);
                    world->linkBlock(neighborGridPos);
                    int neighborId = world->lattice->getBlock(neighborGridPos)->blockId;
                    catomQueue.push(neighborId);
                }

            }
            //std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }
}
*/
void ReconfCatoms3DBlockCode::startup() {
	if (catom->blockId==1) {
        csgRoot = csgUtils.readFile("data/mug.bc");
        csgRoot->toString();
            csgRoot->boundingBox(boundingBox);
        cout << "Bounding box: " << boundingBox.P0 << ' ' << boundingBox.P1 << endl;
        worldPosition = getWorldPosition(boundingBox, catom->position);
        Color color;
        if (csgRoot->isInside(worldPosition, color)) {
            catom->setColor(color);
        }
        else {
            catom->setVisible(false);
        }
	}
    //root = new Seed(1);
    addLine();
}

void ReconfCatoms3DBlockCode::processLocalEvent(EventPtr pev) {
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
    case EVENT_NI_RECEIVE: {
      message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
        switch(message->id) {
            case CATOM_MSG_ID:
            {
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

