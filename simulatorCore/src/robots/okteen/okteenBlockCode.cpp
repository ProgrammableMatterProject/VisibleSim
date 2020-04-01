/*
 * okteenBlockCode.cpp
 *
 *  Created on: 12 mai 2017
 *  Author: Benoît Piranda
 */

#include <iostream>
#include "okteenBlockCode.h"
#include "network.h"
#include "trace.h"

using namespace std;

namespace Okteen {

OkteenBlockCode::OkteenBlockCode(OkteenBlock *host):BlockCode(host) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "OkteenBlockCode constructor" << endl;
#endif
}

OkteenBlockCode::~OkteenBlockCode() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "OkteenBlockCode destructor" << endl;
#endif
}

void OkteenBlockCode::addDebugAttributes(Scheduler *scheduler) {
 /*   OkteenBlock *catom = (OkteenBlock*)(hostBlock);
    scheduler->addKeyword(new KeywordT<short>("orientationCode",&catom->orientationCode,"orientation of the block"));*/
}

void OkteenBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;

    BlockCode::processLocalEvent(pev);

    switch (pev->eventType) {
        case EVENT_ROTATION3D_END: {
#ifdef verbose
            info.str("");
            info << "rec.: EVENT_MOTION_END";
            scheduler->trace(info.str(),hostBlock->blockId);
#endif
            OkteenBlock*c3d = (OkteenBlock*)hostBlock;
            c3d->setPosition(c3d->position);
            onMotionEnd();
        }  break;
    }
}


/*bool OkteenBlockCode::getAttribute(const string &att,ostringstream &sout) {

    OkteenBlock *catom = (OkteenBlock*)(hostBlock);

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
