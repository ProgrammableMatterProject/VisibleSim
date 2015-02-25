/*
 * catom2D1BlockCode.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include <sstream>
#include "catom2D1BlockCode.h"
#include "scheduler.h"
#include "events.h"
//MODIF NICO
#include <boost/shared_ptr.hpp>


using namespace std;
using namespace Catoms2D;

Catoms2D1BlockCode::Catoms2D1BlockCode(Catoms2DBlock *host):Catoms2DBlockCode(host) {
	scheduler = Catoms2D::getScheduler();
	catom2D = (Catoms2DBlock*)hostBlock;
}

Catoms2D1BlockCode::~Catoms2D1BlockCode() {
}

void Catoms2D1BlockCode::startup() {
	stringstream info;
	Catoms2DWorld *world = Catoms2DWorld::getWorld();
	int *gridSize = world->getGridSize();
	info << "Starting ";
	scheduler->trace(info.str(),hostBlock->blockId);
	
	/*
	if (catom2D->blockId == 1) {
		cout << "Catom2D 1 receiving the target map..." << endl;
		for (int iy = 0; iy < gridSize[2]; iy++) {
			for (int ix = 0; ix < gridSize[0]; ix++) {
				if (world->getTargetGrid(ix,0,iy) == fullCell ) {
						cout << "(" << ix << " " << iy << ")" << endl;
				}
			}
		}
	}*/
	
	/*if (catom2D->blockId == 1) {
		string s = string("@") + to_string(catom2D->blockId) + ("neighbor: ");
		for (int i=0; i<6; i++) {
			if (catom2D->getInterface((NeighborDirection::Direction)i) != NULL) {
				s = s + to_string(catom2D->getInterface((NeighborDirection::Direction)i)->connectedInterface->hostBlock->blockId) + string(" ");
			}
		}
		cout << s << endl;
	}*/
	
	/*
	if (world->getTargetGrid(catom2D->position[0],catom2D->position[1],catom2D->position[2]) == fullCell) {
		cout << "@" << catom2D->blockId << " correctly placed (" << catom2D->position[0] << "," << catom2D->position[2] << ")" << endl;
	} else {
		for (int iy = 0; iy < gridSize[2]; iy++) {
			for (int ix = 0; ix < gridSize[0]; ix++) {
				if (world->getTargetGrid(ix,0,iy) == fullCell &&  world->getGridPtr(ix,0,iy)==NULL) {
					cout << "@" << catom2D->blockId << " moved to " << "(" << ix << "," << iy << ")" << endl;
					world->setGridPtr(catom2D->position[0],catom2D->position[1],catom2D->position[2],NULL);
					catom2D->setPosition(Vecteur(ix,0,iy));
					world->setGridPtr(ix,0,iy,catom2D);
					return;
				}
			}
		}
		cout << "@" << catom2D->blockId << " no where to move" << endl; 
	}*/
}

void Catoms2D1BlockCode::processLocalEvent(EventPtr pev) {
	int i;
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
	case EVENT_NI_RECEIVE: {
	  message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
	  P2PNetworkInterface * recv_interface = message->destinationInterface;
	}
	  break;
	}
}

Catoms2D::Catoms2DBlockCode* Catoms2D1BlockCode::buildNewBlockCode(Catoms2DBlock *host) {
	return(new Catoms2D1BlockCode(host));
}
