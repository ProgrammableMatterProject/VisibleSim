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

using namespace std;
using namespace Catoms3D;

int neighborDirectionsEven[12][3] = {{0,0,-1},{-1,-1,-1},{-1,0,-1},{0,-1,-1},{0,0,1},{-1,-1,1},{-1,0,1},{0,-1,1},{1,0,0},{0,1,0},{-1,0,0},{0,-1,0}};
int neighborDirectionsOdd[12][3] = {{0,0,-1},{1,1,-1},{1,0,-1},{0,1,-1},{0,0,1},{1,1,1},{1,0,1},{0,1,1},{1,0,0},{0,1,0},{-1,0,0},{0,-1,0}};

int sideOneOddXY[4][3] = {{1,0,-1},{1,1,-1},{0,1,-1},{0,0,-1}};
int sideTwoOddXY[4][3] = {{1,0,1},{1,1,1},{0,1,1},{0,0,1}};
int sideOneEvenXY[4][3] = {{0,0,-1},{-1,-1,-1},{-1,0,-1},{0,-1,-1}};
int sideTwoEvenXY[4][3] = {{0,0,1},{-1,-1,1},{-1,0,1},{0,-1,1}};

int sideOneOddXZ[5][3] = {{1,0,-1},{1,1,-1},{1,1,1},{1,0,1},{1,0,0}};
int sideTwoOddXZ[5][3] = {{0,1,-1},{0,0,-1},{0,1,1},{0,0,1},{-1,0,0}};
int sideOneEvenXZ[5][3] = {{0,0,-1},{0,-1,-1},{0,0,1},{0,-1,1},{1,0,0}};
int sideTwoEvenXZ[5][3] = {{-1,0,-1},{-1,-1,-1},{-1,0,1},{-1,-1,1},{-1,0,0}};

int sideOneOddYZ[5][3] = {{0,0,-1},{1,0,-1},{0,0,1},{1,0,1},{0,-1,0}};
int sideTwoOddYZ[5][3] = {{1,1,-1},{0,1,-1},{1,1,1},{0,1,1},{0,1,0}};
int sideOneEvenYZ[5][3] = {{0,-1,-1},{-1,-1,-1},{0,-1,1},{-1,-1,1},{0,-1,0}};
int sideTwoEvenYZ[5][3] = {{0,0,-1},{-1,0,-1},{0,0,1},{-1,0,1},{0,1,0}};
CSGNode* ReconfCatoms3DBlockCode::csgRoot = NULL;
queue<int> ReconfCatoms3DBlockCode::catomQueue;
BoundingBox ReconfCatoms3DBlockCode::boundingBox;

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

Vector3D ReconfCatoms3DBlockCode::getWorldPosition(BoundingBox boundingBox) {
    Vector3D worldPosition = gridToWorldPosition(catom->position);
    cout << "Position = " << worldPosition << endl;
    worldPosition.pt[0] += boundingBox.P0[0]; 
    worldPosition.pt[1] += boundingBox.P0[1]; 
    worldPosition.pt[2] += boundingBox.P0[2]; 
    return worldPosition;
}

bool ReconfCatoms3DBlockCode::isPositionUnblockedSide(const Cell3DPosition &pos) {
    int xyPos[4][3] = {{-1,0,0}, {1,0,0}, {0,-1,0}, {0,1,0}};
    Cell3DPosition occupiedPosition(pos[0]+xyPos[0][0],pos[1]+xyPos[0][1], pos[2]+xyPos[0][2]);
    Cell3DPosition forbiddenPosition(pos[0]+xyPos[1][0], pos[1]+xyPos[1][1], pos[2]+xyPos[1][2]);
    if (cellHasBlock(occupiedPosition) && cellHasBlock(forbiddenPosition))
        return false;
    occupiedPosition.set(pos[0]+xyPos[2][0],pos[1]+xyPos[2][1], pos[2]+xyPos[2][2]);
    forbiddenPosition.set(pos[0]+xyPos[3][0], pos[1]+xyPos[3][1], pos[2]+xyPos[3][2]);
    if (cellHasBlock(occupiedPosition) && cellHasBlock(forbiddenPosition))
        return false;
    return true;
}

bool ReconfCatoms3DBlockCode::isPositionUnblockedXY(const Cell3DPosition &pos) {
    int *sideOne, *sideTwo;
    Cell3DPosition position1, position2;
    bool isInSide1 = false, isInSide2 = false;
    for (int i = 0; i < 4; i++) {
        if (pos[2]%2) {
            sideOne = sideOneOddXY[i];
            sideTwo = sideTwoOddXY[i];
        }
        else {
            sideOne = sideOneEvenXY[i];
            sideTwo = sideTwoEvenXY[i];
        }
        position1.set(pos[0]+sideOne[0], pos[1] + sideOne[1], pos[2] + sideOne[2]); 
        position2.set(pos[0]+sideTwo[0], pos[1] + sideTwo[1], pos[2] + sideTwo[2]); 
        if (cellHasBlock(position1))
            isInSide1 = true;
        if (cellHasBlock(position2))
            isInSide2 = true;
    }
    if (isInSide1 && isInSide2)
        return false;
    return true;
}

bool ReconfCatoms3DBlockCode::isPositionUnblockedYZ(const Cell3DPosition &pos) {
    int *sideOne, *sideTwo;
    Cell3DPosition position1, position2;
    bool isInSide1 = false, isInSide2 = false;
    for (int i = 0; i < 5; i++) {
        if (pos[2]%2) {
            sideOne = sideOneOddYZ[i];
            sideTwo = sideTwoOddYZ[i];
        }
        else {
            sideOne = sideOneEvenYZ[i];
            sideTwo = sideTwoEvenYZ[i];
        }
        position1.set(pos[0]+sideOne[0], pos[1] + sideOne[1], pos[2] + sideOne[2]); 
        position2.set(pos[0]+sideTwo[0], pos[1] + sideTwo[1], pos[2] + sideTwo[2]); 
        if (cellHasBlock(position1))
            isInSide1 = true;
        if (cellHasBlock(position2))
            isInSide2 = true;
    }
    if (isInSide1 && isInSide2)
        return false;
    return true;
}

bool ReconfCatoms3DBlockCode::isPositionUnblockedXZ(const Cell3DPosition &pos) {
    int *sideOne, *sideTwo;
    Cell3DPosition position1, position2;
    bool isInSide1 = false, isInSide2 = false;
    for (int i = 0; i < 5; i++) {
        if (pos[2]%2) {
            sideOne = sideOneOddXZ[i];
            sideTwo = sideTwoOddXZ[i];
        }
        else {
            sideOne = sideOneEvenXZ[i];
            sideTwo = sideTwoEvenXZ[i];
        }
        position1.set(pos[0]+sideOne[0], pos[1] + sideOne[1], pos[2] + sideOne[2]); 
        position2.set(pos[0]+sideTwo[0], pos[1] + sideTwo[1], pos[2] + sideTwo[2]); 
        if (cellHasBlock(position1))
            isInSide1 = true;
        if (cellHasBlock(position2))
            isInSide2 = true;
    }
    if (isInSide1 && isInSide2)
        return false;
    return true;
}

bool ReconfCatoms3DBlockCode::isPositionUnblocked(const Cell3DPosition &pos) {
    if (isPositionUnblockedSide(pos)) {
        if (isPositionUnblockedXZ(pos) || isPositionUnblockedYZ(pos) || isPositionUnblockedXY(pos)) {
            return true;
        }
    }
    return false;
}

bool ReconfCatoms3DBlockCode::isPositionUnblockable(const Cell3DPosition &pos) {
    Cell3DPosition neighborPos;
    int *direction;
    for (int i = 0; i < 12; i++) {
        direction = pos[2]%2 ? neighborDirectionsOdd[i] : neighborDirectionsEven[i];
        neighborPos.set(pos[0]+direction[0], pos[1] + direction[1], pos[2] + direction[2]); 
        if (world->lattice->isFree(neighborPos) && isPositionUnblocked(neighborPos)) {
           simulatedBlockPosition = pos; 
           if (!isPositionUnblocked(neighborPos))
               return false;
           simulatedBlockPosition.set(0,0,0);
        }
    }
    return true;
}

bool ReconfCatoms3DBlockCode::cellHasBlock(const Cell3DPosition &pos) {
    if (simulatedBlockPosition == pos)
        return true;
    return world->lattice->cellHasBlock(pos);
}

void ReconfCatoms3DBlockCode::addNeighbors() {
    for (int i = 0; i < 12; i++) {
        Cell3DPosition neighborGridPos = catom->position;
        int *neighborPosPointer = (catom->position[2]%2) ? neighborDirectionsOdd[i] : neighborDirectionsEven[i];
        neighborGridPos.pt[0] += neighborPosPointer[0];
        neighborGridPos.pt[1] += neighborPosPointer[1];
        neighborGridPos.pt[2] += neighborPosPointer[2];
        if (world->lattice->isFree(neighborGridPos)) {
            Color color;
            Vector3D newWorldPos = getWorldPosition(boundingBox);
            if (csgRoot->isInBorder(newWorldPos, color, 1.5)) {
                if (isPositionUnblockable(neighborGridPos)) {
                    world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, neighborGridPos, color, 0, false);
                    world->linkBlock(neighborGridPos);
                    int neighborId = world->lattice->getBlock(neighborGridPos)->blockId;
                    catomQueue.push(neighborId);
                }
                else {
                    cout << "ERROR TO ADD BLOCK AT POSITION " << neighborGridPos << endl;
                    world->addBlock(0, ReconfCatoms3DBlockCode::buildNewBlockCode, neighborGridPos, RED, 0, false);
                    world->linkBlock(neighborGridPos);
                    //std::this_thread::sleep_for(std::chrono::milliseconds(100));
                }

            }
            //std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }
}

void ReconfCatoms3DBlockCode::startup() {
	if (catom->blockId==1) {
        csgRoot = csgUtils.readFile("data/mug.bc");
        csgRoot->toString();
            csgRoot->boundingBox(boundingBox);
        cout << "Bounding box: " << boundingBox.P0 << ' ' << boundingBox.P1 << endl;
        worldPosition = getWorldPosition(boundingBox);
        Color color;
        if (csgRoot->isInside(worldPosition, color)) {
            catom->setColor(color);
        }
        else {
            catom->setVisible(false);
        }
	}
    addNeighbors();
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

