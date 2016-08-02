/*
 * catom2D1BlockCode.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include <sstream>
#include "noneCatoms2DBlockCode.h"
#include "scheduler.h"
#include "events.h"

using namespace std;
using namespace Catoms2D;

NoneCatoms2DBlockCode::NoneCatoms2DBlockCode(Catoms2DBlock *host):Catoms2DBlockCode(host) {
	cout << "NoneCatoms2DBlockCode constructor" << endl;
	scheduler = getScheduler();
	catom2D = (Catoms2DBlock*)hostBlock;
}

NoneCatoms2DBlockCode::~NoneCatoms2DBlockCode() {
	cout << "NoneCatoms2DBlockCode destructor" << endl;
}

void NoneCatoms2DBlockCode::startup() {
	stringstream info;

	info << "Starting ";
	scheduler->trace(info.str(),hostBlock->blockId);	
}

void NoneCatoms2DBlockCode::processLocalEvent(EventPtr pev) {
	// int i;
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
	case EVENT_NI_RECEIVE: {
		message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
		// P2PNetworkInterface * recv_interface = message->destinationInterface;
	}
		break;
	}
}

BlockCode* NoneCatoms2DBlockCode::buildNewBlockCode(BuildingBlock *host) {
	return(new NoneCatoms2DBlockCode((Catoms2DBlock*)host));
}
