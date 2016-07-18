/*
 * reconfiguration.cpp
 *
 *  Created on: 27 nov 2015
 *      Author: andre
 */

#include <iostream>
#include <memory>

#include "catoms2DWorld.h"
#include "scheduler.h"
#include "events.h"

#include "reconfiguration.h"
#include "reconfigurationMsg.h"



#include <set>

using namespace std;
using namespace Catoms2D;

#define ROTATION_DIRECTION RelativeDirection::CW
#define OPPOSITE_ROTATION_DIRECTION RelativeDirection::getOpposite(ROTATION_DIRECTION)

Reconfiguration::Reconfiguration(Catoms2DBlock *c, Map *m): map(m) {
    catom = c;
    state = UNKNOWN;
    if (c->blockId == 1) {
        std::set<Coordinate> myset;
        std::set<Coordinate>::iterator it;
        myset.insert(Coordinate(1,0));
        myset.insert(Coordinate(1,0));
        myset.insert(Coordinate(1,0));
        std::cout << "myset contains:";
        for (it=myset.begin(); it!=myset.end(); ++it)
            std::cout << ' ' << *it;
        std::cout << '\n';
    }
}

Reconfiguration::~Reconfiguration() {}

void Reconfiguration::handle(MessagePtr m) {
    ReconfigurationMsg_ptr rm = std::static_pointer_cast<ReconfigurationMsg>(m);
    P2PNetworkInterface *recv = m->destinationInterface;
    switch(rm->subtype) {
    case ReconfigurationMsg::STATE_QUERY: {
        ReconfigurationStateQueryMsg_ptr rsqm = std::static_pointer_cast<ReconfigurationStateQueryMsg>(m);
        Coordinate nextCoordinate;
        P2PNetworkInterface *nextP2P;
        // Query goes through the perimeter of the concerned cell following the rotation direction
        // identify next neighbor modules on the perimeter of the concerned cell
        nextP2P = catom->getNextInterface(ROTATION_DIRECTION, recv, true);
        nextCoordinate = map->getPosition(nextP2P);
        if (Map::areNeighbors(nextCoordinate,rsqm->cell)) {
            // forward to this next catom on the border of the queried cell 
        } else {
            // send back the reply. All nodes on the reply path will include their state, 
            // and the state of the node moving around them
      
        }
    }
        break;
    case ReconfigurationMsg::STATE_ANSWER: {
        ReconfigurationStateAnswerMsg_ptr rsam = std::static_pointer_cast<ReconfigurationStateAnswerMsg>(m);
        P2PNetworkInterface *nextP2P;
        if (catom->blockId == rsam->destinationId) {
            // answer is for that catom
        } else {
            // add catom's state, and states of catom moving around this one, then forward the msg
            nextP2P = catom->getNextInterface(OPPOSITE_ROTATION_DIRECTION, recv, true);
        }
    }
        break;
    default:
        cerr << "unknown reconfiguration message type" << endl;
    }
}

void Reconfiguration::start() {
  
    if (isInTarget()) {
        return;
    }
  
    //tryToMove();
}

void Reconfiguration::tryToMove() {
    if (isFree()) {
        catom->setColor(GREEN);
    } else {
        catom->setColor(RED);
    }
}

bool Reconfiguration::isInTarget() {
    return Map::isInTarget(map->getPosition());
}

bool Reconfiguration::isOnBorder() {
    return (catom->nbNeighbors(true) <= 5);
}

bool Reconfiguration::isFree() {
    // at least 3 empty faces!
    return (catom->nbConsecutiveEmptyFaces(true) >= 3);
}

Rotation2DMove* Reconfiguration::nextMove() {
    if (isFree()) {
        Catoms2DBlock* pivot = map->getOnBorderNeighbor(ROTATION_DIRECTION);
        return new Rotation2DMove(pivot,ROTATION_DIRECTION);
    }
    return NULL;
}

bool Reconfiguration::isDone() {
    Catoms2DWorld *world = Catoms2DWorld::getWorld();
    Cell3DPosition gridSize = world->lattice->gridSize;
    for (int iy = 0; iy < gridSize[2]; iy++) {
        for (int ix = 0; ix < gridSize[0]; ix++) {
            if (world->getTargetGrid(ix,0,iy) == fullCell ) {
                if (!world->lattice->getBlock(Cell3DPosition(ix,0,iy))) {
                    return false;
                }
            }
        }
    }
    return true;
}
