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
	cout << "Catoms2D1BlockCode constructor" << endl;
	scheduler = Catoms2D::getScheduler();
	catom2D = (Catoms2DBlock*)hostBlock;
}

Catoms2D1BlockCode::~Catoms2D1BlockCode() {
	cout << "Catoms2D1BlockCode destructor" << endl;
}

void Catoms2D1BlockCode::startup() {
	stringstream info;

	info << "Starting ";
	scheduler->trace(info.str(),hostBlock->blockId);	
}

void Catoms2D1BlockCode::processLocalEvent(EventPtr pev) {
	// int i;
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
	case EVENT_NI_RECEIVE: {
	  message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
	  // P2PNetworkInterface * recv_interface = message->destinationInterface;
	}
	  break;
	}
}

Catoms2D::Catoms2DBlockCode* Catoms2D1BlockCode::buildNewBlockCode(Catoms2DBlock *host) {
	return(new Catoms2D1BlockCode(host));
}
