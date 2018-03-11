/*
 * blinkyBlocksBlockCode.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "smartBlocksBlockCode.h"
#include "trace.h"

using namespace std;

namespace SmartBlocks {
SmartBlocksBlockCode::SmartBlocksBlockCode(SmartBlocksBlock *host):BlockCode(host) {
	OUTPUT << "SmartBlocksBlockCode constructor" << endl;
}

SmartBlocksBlockCode::~SmartBlocksBlockCode() {
	OUTPUT << "SmartBlocksBlockCode destructor" << endl;
}

void SmartBlocksBlockCode::processLocalEvent(EventPtr pev) {
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
            //startup();
        } break;
        case EVENT_TRANSLATION_END: {
#ifdef verbose
			info.str("");
			info << "rec.: EVENT_MOTION_END";
			scheduler->trace(info.str(),hostBlock->blockId);
#endif
            onMotionEnd();
        }  break;
    }
}

}
