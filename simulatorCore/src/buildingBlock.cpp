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
}

BuildingBlock::~BuildingBlock() {
	OUTPUT << "BuildingBlock destructor" << endl;
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
	list <P2PNetworkInterface*>::const_iterator niIt=P2PNetworkInterfaceList.begin();
	while (niIt!=P2PNetworkInterfaceList.end() && (*niIt)->connectedInterface->hostBlock!=destBlock) niIt++;
	return (niIt==P2PNetworkInterfaceList.end())?NULL:(*niIt);
}

P2PNetworkInterface*BuildingBlock::getP2PNetworkInterfaceByDestBlockId(int destBlockId) {
	list <P2PNetworkInterface*>::const_iterator niIt=P2PNetworkInterfaceList.begin();
	while (niIt!=P2PNetworkInterfaceList.end() && (*niIt)->connectedInterface->hostBlock->blockId != destBlockId) {
        OUTPUT << (*niIt)->connectedInterface->hostBlock->blockId << endl;
        niIt++;
    }
	return (niIt==P2PNetworkInterfaceList.end())?NULL:(*niIt);
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

int BuildingBlock::getNextRandomNumber() {
	int x = 0;
	do {
		x = generator();
	} while (x <= 0);
	return x;
}

bool BuildingBlock::getAttribute(const string &str,ostringstream &sout) {
    return blockCode->getAttribute(str,sout);
}

} // BaseSimulator namespace
