/*
 * robotBlocksBlockCode.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "robotBlocksBlockCode.h"
#include "trace.h"

using namespace std;

namespace RobotBlocks {
RobotBlocksBlockCode::RobotBlocksBlockCode(RobotBlocksBlock *host):BlockCode(host) {
#ifdef DEBUG_OBJECT_LIFECYCLE
	OUTPUT << "RobotBlocksBlockCode constructor" << endl;
#endif
}

RobotBlocksBlockCode::~RobotBlocksBlockCode() {
#ifdef DEBUG_OBJECT_LIFECYCLE    
	OUTPUT << "RobotBlocksBlockCode destructor" << endl;
#endif
}

void RobotBlocksBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;

//cout << "event #" << pev->id << ":" << pev->eventType << endl;
    switch (pev->eventType) {
        case EVENT_NI_RECEIVE: {
            message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
    // search message id in eventFuncMap
            multimap<int,eventFunc>::iterator im = eventFuncMap.find(message->type);
            if (im!=eventFuncMap.end()) {
                P2PNetworkInterface *recv_interface = message->destinationInterface;
                (*im).second(this,message,recv_interface);
            } else {
                OUTPUT << "ERROR: message Id #" << message->type << " unknown!" << endl;
            }
        } break;
        case EVENT_ADD_NEIGHBOR: {
            OUTPUT << "ADD_NEIGHBOR" << endl;
        } break;
        case EVENT_TAP: {
			int face = (std::static_pointer_cast<TapEvent>(pev))->tappedFace;
            onTap(face);
        } break;
    }
}
}
