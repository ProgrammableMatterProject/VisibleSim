/*
 * catoms3DBlockCode.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "catoms3DBlockCode.h"
#include "trace.h"
#include "catoms3DWorld.h"

using namespace std;

namespace Catoms3D {

Catoms3DBlockCode::Catoms3DBlockCode(Catoms3DBlock *host):BlockCode(host) {
#ifdef DEBUG_OBJECT_LIFECYCLE
	OUTPUT << "Catoms3DBlockCode constructor" << endl;
#endif
    // motionRules = Catoms3DWorld::getWorld()->getMotionRules();
}

Catoms3DBlockCode::~Catoms3DBlockCode() {
#ifdef DEBUG_OBJECT_LIFECYCLE
	OUTPUT << "Catoms3DBlockCode destructor" << endl;
#endif
}

void Catoms3DBlockCode::addDebugAttributes(Scheduler *scheduler) {
 /*   Catoms3DBlock *catom = (Catoms3DBlock*)(hostBlock);
    scheduler->addKeyword(new KeywordT<short>("orientationCode",&catom->orientationCode,"orientation of the block"));*/
}

void Catoms3DBlockCode::processLocalEvent(EventPtr pev) {
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
#ifdef DEBUG_NEIGHBORHOOD
            OUTPUT << "ADD_NEIGHBOR" << endl;
#endif
            //startup();
        } break;
        case EVENT_TAP: {
			int face = (std::static_pointer_cast<TapEvent>(pev))->tappedFace;
            onTap(face);
        } break;
        case EVENT_ROTATION3D_END: {
#ifdef verbose
			info.str("");
			info << "rec.: EVENT_MOTION_END";
			scheduler->trace(info.str(),hostBlock->blockId);
#endif
            Catoms3DBlock*c3d = (Catoms3DBlock*)hostBlock;
            c3d->setPositionAndOrientation(c3d->position,c3d->orientationCode);
            onMotionEnd();
        }  break;
				case EVENT_TELEPORTATION_END: {
					onMotionEnd();
				}
    }
}


/*bool Catoms3DBlockCode::getAttribute(const string &att,ostringstream &sout) {

    Catoms3DBlock *catom = (Catoms3DBlock*)(hostBlock);

    if (att=="blockId") {
        sout << catom->blockId << endl;
        return false;
    } else if (att.substr(0,9)=="neighbor[") {
        int n = atoi(att.substr(9,2).c_str());
        if (n>=0 && n<12) {
            P2PNetworkInterface *p2p = catom->getInterface(n);
            if (p2p->connectedInterface!=NULL) {
                sout << "catom#" << p2p->connectedInterface->hostBlock->blockId << endl;
            } else {
                sout << "no catom linked."<< endl;
            }
        } else {
            sout << "Error: neighbor[n],  0<=n<12" << endl;
        }
        return true;
    }
    if (att=="gridpos") {
        sout << catom->position << endl;
        return false;
    }
    if (att=="worldpos") {
        sout << catom->getGlBlock()->position[0] << "," << catom->getGlBlock()->position[1] << "," << catom->getGlBlock()->position[2] << endl;
        return false;
    }
    if (att=="orientationcode") {
        sout << catom->orientationCode << endl;
        return false;
    }
    if (att=="matrix") {
        sout << catom->getGlBlock()->mat << endl;
        return false;
    }

    sout << "attribute "<< att << " unknown!" << endl;
    return false;
}
*/
}
