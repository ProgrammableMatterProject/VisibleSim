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

BuildingBlock::BuildingBlock(int bId, BlockCodeBuilder bcb) {
    OUTPUT << "BuildingBlock constructor (id:" << nextId << ")" << endl;
    if (bId < 0) {
		blockId = nextId;
		nextId++;
    } else {
		blockId = bId;
    }
    P2PNetworkInterfaceNextLocalId = 0;
    state.store(ALIVE);
    clock = NULL;
    std::random_device rd;
    generator = std::ranlux48(rd());
    dis = uniform_int_distribution<>(0, 50 * blockId);
    buildNewBlockCode = bcb;
	blockCode = (BaseSimulator::BlockCode*)bcb(this);
}

BuildingBlock::~BuildingBlock() {
    delete blockCode;
	OUTPUT << "BuildingBlock destructor" << endl;    

	if (clock != NULL) {
		delete clock;
    }

	for (P2PNetworkInterface *p2p : P2PNetworkInterfaces)
		delete p2p;
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
		P2PNetworkInterfaces.push_back(ni1);
    }

    if (!destBlock->getP2PNetworkInterfaceByBlockRef(this)) {
		// creation of the new network interface
		OUTPUT << "adding a new interface to block " << this->blockId << endl;
		ni2 = new P2PNetworkInterface(destBlock);
		destBlock->P2PNetworkInterfaces.push_back(ni2);
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
		P2PNetworkInterfaces.push_back(ni1);
		// if the corresponding interface exists in the connected block, we link the two interfaces
		if (destBlock->addP2PNetworkInterfaceAndConnectTo(this)) {
			P2PNetworkInterface* ni2 = destBlock->getP2PNetworkInterfaceByBlockRef(this);
			ni1->connect(ni2);
		}
    }
    return false;
}

P2PNetworkInterface *BuildingBlock::getP2PNetworkInterfaceByBlockRef(BuildingBlock *destBlock) {
    for(P2PNetworkInterface *p2p : P2PNetworkInterfaces) {
		if (p2p->connectedInterface) {
			if (p2p->connectedInterface->hostBlock == destBlock) {
				return p2p;
			}
		}
    }
    return NULL;
}

P2PNetworkInterface*BuildingBlock::getP2PNetworkInterfaceByDestBlockId(int destBlockId) {
    for(P2PNetworkInterface *p2p : P2PNetworkInterfaces) {
		if (p2p->connectedInterface) {
			if (p2p->connectedInterface->hostBlock->blockId == destBlockId) {
				return p2p;
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
    if (state.load() >= ALIVE) {
		color = c;
    }
    getWorld()->updateGlData(this);
}

void BuildingBlock::setPosition(const Cell3DPosition &p) {
    position = p;
    getWorld()->updateGlData(this);
}
    
void BuildingBlock::tap(uint64_t date, bool debug) {
    OUTPUT << "tap scheduled" << endl;
    getScheduler()->scheduleLock(new TapEvent(date, this, debug));
}
    
int BuildingBlock::getNextRandomNumber() {
    return dis(generator);
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
