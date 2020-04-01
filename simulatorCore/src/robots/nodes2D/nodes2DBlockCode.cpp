/**
 * @file   okteenBlockCode.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:55:11 2019
 *
 * @brief
 *
 *
 */

#include "robots/nodes2D/nodes2DBlockCode.h"

#include <iostream>

#include "comm/network.h"
#include "utils/trace.h"

using namespace std;
using namespace Nodes2D;

Nodes2DBlockCode::Nodes2DBlockCode(Nodes2DBlock *host):BlockCode(host) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "Nodes2DBlockCode constructor" << endl;
#endif
}

Nodes2DBlockCode::~Nodes2DBlockCode() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "Nodes2DBlockCode destructor" << endl;
#endif
}

void Nodes2DBlockCode::addDebugAttributes(Scheduler *scheduler) {
}

void Nodes2DBlockCode::processLocalEvent(EventPtr pev) {
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

        case EVENT_TAP: {
            int face = (std::static_pointer_cast<TapEvent>(pev))->tappedFace;
            onTap(face);
        } break;

        case EVENT_NODES2DMOTION_END: {
#ifdef verbose
            info.str("");
            info << "rec.: EVENT_MOTION_END";
            scheduler->trace(info.str(),hostBlock->blockId);
#endif
            onMotionEnd();
        }  break;
    }
}
