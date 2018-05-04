/*
 * blinkyBlocksBlockCode.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "blinkyBlocksBlockCode.h"
#include "trace.h"

using namespace std;

namespace BlinkyBlocks {

BlinkyBlocksBlockCode::BlinkyBlocksBlockCode(BlinkyBlocksBlock *host):BlockCode(host) {
	OUTPUT << "BlinkyBlocksBlockCode constructor" << endl;
}

BlinkyBlocksBlockCode::~BlinkyBlocksBlockCode() {
	OUTPUT << "BlinkyBlocksBlockCode destructor" << endl;
}

void BlinkyBlocksBlockCode::processLocalEvent(EventPtr pev) {
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
                OUTPUT << "ERROR: message Id #"<< message->type << " unknown!" << endl;
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
