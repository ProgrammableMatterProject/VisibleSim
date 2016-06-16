/*
 * buildingBlock.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "world.h"
#include "buildingBlock.h"
#include "scheduler.h"
#include "trace.h"

using namespace std;

namespace BaseSimulator {

int BuildingBlock::nextId = 0;

//===========================================================================================================
//
//          BuildingBlock  (class)
//
//===========================================================================================================

BuildingBlock::BuildingBlock(int bId) {
    OUTPUT << "BuildingBlock constructor (id:" << nextId << ")" << endl;
    if (bId < 0) {
	blockId = nextId;
	nextId++;
    } else {
	blockId = bId;
    }
    P2PNetworkInterfaceNextLocalId = 0;
    state = ALIVE;
    generator = boost::rand48(50*blockId);
    clock = NULL;
}

BuildingBlock::~BuildingBlock() {
    delete blockCode;
    OUTPUT << "BuildingBlock destructor" << endl;
    if (clock != NULL) {
	delete clock;
    }
}

unsigned int BuildingBlock::getNextP2PInterfaceLocalId() {
    int id = P2PNetworkInterfaceNextLocalId;
    P2PNetworkInterfaceNextLocalId++;
    return(id);
}

bool BuildingBlock::addP2PNetworkInterfaceAndConnectTo(BuildingBlock *destBlock) {
    P2PNetworkInterface *ni1, *ni2;
    ni1 = NULL;
    ni2 = NULL;
    if (!getP2PNetworkInterfaceByBlockRef(destBlock)) {
	// creation of the new network interface
	OUTPUT << "adding a new interface to block " << destBlock->blockId << endl;
	ni1 = new P2PNetworkInterface(this);
	P2PNetworkInterfaceList.push_back(ni1);
    }

    if (!destBlock->getP2PNetworkInterfaceByBlockRef(this)) {
	// creation of the new network interface
	OUTPUT << "adding a new interface to block " << this->blockId << endl;
	ni2 = new P2PNetworkInterface(destBlock);
	destBlock->P2PNetworkInterfaceList.push_back(ni2);
    }

    if (ni1!=NULL && ni2!=NULL) {
	ni1->connect(ni2);
	return (true);
    } else {
	OUTPUT << "*** ERROR *** could not connect the new interfaces" << endl;
    }
    return false;
}

bool BuildingBlock::addP2PNetworkInterfaceAndConnectTo(int destBlockId) {
    // if the link is not in the list
    BuildingBlock *destBlock = BaseSimulator::getWorld()->getBlockById(destBlockId);
    if (!getP2PNetworkInterfaceByBlockRef(destBlock)) {
	// creation of the new network interface
	P2PNetworkInterface* ni1 = new P2PNetworkInterface(this);
	P2PNetworkInterfaceList.push_back(ni1);
	// if the corresponding interface exists in the connected block, we link the two interfaces
	if (destBlock->addP2PNetworkInterfaceAndConnectTo(this)) {
	    P2PNetworkInterface* ni2 = destBlock->getP2PNetworkInterfaceByBlockRef(this);
	    ni1->connect(ni2);
	}
    }
    return false;
}

P2PNetworkInterface *BuildingBlock::getP2PNetworkInterfaceByBlockRef(BuildingBlock *destBlock) {
    for(list <P2PNetworkInterface*>::const_iterator it=P2PNetworkInterfaceList.begin(); it != P2PNetworkInterfaceList.end(); it++) {
	if ((*it)->connectedInterface) {
	    if ((*it)->connectedInterface->hostBlock == destBlock) {
		return (*it);
	    }
	}
    }
    return NULL;
}

P2PNetworkInterface*BuildingBlock::getP2PNetworkInterfaceByDestBlockId(int destBlockId) {
    for(list <P2PNetworkInterface*>::const_iterator it=P2PNetworkInterfaceList.begin(); it != P2PNetworkInterfaceList.end(); it++) {
	if ((*it)->connectedInterface) {
	    if ((*it)->connectedInterface->hostBlock->blockId == destBlockId) {
		return (*it);
	    }
	}
    }
    return NULL;
}


void BuildingBlock::scheduleLocalEvent(EventPtr pev) {
    localEventsList.push_back(pev);

    if (localEventsList.size() == 1) {
	uint64_t date;
	date = this->blockCode->availabilityDate;
	if (date < getScheduler()->now()) date=getScheduler()->now();
	getScheduler()->schedule(new ProcessLocalEvent(date,this));
    }
    return;
}

void BuildingBlock::processLocalEvent() {
    EventPtr pev;

    if (localEventsList.size() == 0) {
	cerr << "*** ERROR *** The local event list should not be empty !!" << endl;
	getScheduler()->trace("*** ERROR *** The local event list should not be empty !!");
	exit(EXIT_FAILURE);
    }
    pev = localEventsList.front();
    localEventsList.pop_front();
    blockCode->processLocalEvent(pev);
    if (blockCode->availabilityDate < getScheduler()->now()) blockCode->availabilityDate = getScheduler()->now();
    if (localEventsList.size() > 0) {
	getScheduler()->schedule(new ProcessLocalEvent(blockCode->availabilityDate,this));
    }
}        

void BuildingBlock::setColor(int idColor) {
    const GLfloat *col = tabColors[idColor%12];
    color.set(col[0],col[1],col[2],col[3]);
    getWorld()->updateGlData(this);
}    

void BuildingBlock::setColor(const Color &c) {
    lock();
    if (state >= ALIVE) {
	color = c;
    }
    unlock();
    getWorld()->updateGlData(this);
}

void BuildingBlock::setPosition(const Cell3DPosition &p) {
    position = p;
    getWorld()->updateGlData(this);
}

void BuildingBlock::setPosition(const Vector3D &p) {
    position = Cell3DPosition(short(p[0]), short(p[1]), short(p[2]));
    getWorld()->updateGlData(this);
}

    
void BuildingBlock::tap(uint64_t date) {
    OUTPUT << "tap scheduled" << endl;
    getScheduler()->scheduleLock(new TapEvent(date, this));
}
    
int BuildingBlock::getNextRandomNumber() {
    int x = 0;
    do {
	x = generator();
    } while (x <= 0);
    return x;
}

uint64_t BuildingBlock::getTime() {
    if (clock == NULL) {
	cerr << "device has no internal clock" << endl;
	return 0;
    }
    return clock->getTime();
}

uint64_t BuildingBlock::getSchedulerTimeForLocalTime(uint64_t localTime) {
    if (clock == NULL) {
	cerr << "device has no internal clock" << endl;
	return localTime;
    }
    return clock->getSchedulerTimeForLocalTime(localTime);
}

} // BaseSimulator namespace
