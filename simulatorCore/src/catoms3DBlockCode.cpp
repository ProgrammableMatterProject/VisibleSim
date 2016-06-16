/*
 * catoms3DBlockCode.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "catoms3DBlockCode.h"
#include "network.h"
#include "trace.h"

using namespace std;

namespace Catoms3D {

Catoms3DBlockCode::Catoms3DBlockCode(Catoms3DBlock *host):BlockCode(host) {
	OUTPUT << "Catoms3DBlockCode constructor" << endl;
}

Catoms3DBlockCode::~Catoms3DBlockCode() {
	OUTPUT << "Catoms3DBlockCode destructor" << endl;
}

void Catoms3DBlockCode::addDebugAttributes(Scheduler *scheduler) {
 /*   Catoms3DBlock *catom = (Catoms3DBlock*)(hostBlock);
    scheduler->addKeyword(new KeywordT<short>("orientationCode",&catom->orientationCode,"orientation of the block"));*/
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
