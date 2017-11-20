/*
 * catom2D1BlockCode.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include <sstream>
#include "catoms3DWorld.h"
#include "simpleCatom3DBlockCode.h"
#include "scheduler.h"
#include "events.h"
#include <memory>

#define verbose 1

using namespace std;
using namespace Catoms3D;

SimpleCatom3DBlockCode::SimpleCatom3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
	cout << "SimpleCatom3DBlockCode constructor" << endl;
	scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;
}

SimpleCatom3DBlockCode::~SimpleCatom3DBlockCode() {
	cout << "SimpleCatom3DBlockCode destructor" << endl;
}

void SimpleCatom3DBlockCode::startup() {
    static bool firstStart=true;
	stringstream info;

console << "start\n";
	info << "Starting ";
OUTPUT << "start SimpleCatom3DBlockCode " << catom->blockId << endl;
	/* skeleton test */
/*	Catoms3DWorld*wrl = Catoms3DWorld::getWorld();
	Vector3D pos(catom->ptrGlBlock->position[0],catom->ptrGlBlock->position[1],catom->ptrGlBlock->position[2]);
	potentiel = wrl->getSkeletonPotentiel(pos);
	catom->setColor(potentiel>1.0?YELLOW:DARKORANGE);

	info << potentiel;
	scheduler->trace(info.str(),hostBlock->blockId);*/
	if (catom->blockId==1 && firstStart) {
        step=0;
        currentOr = catom->orientationCode;
        nextRotation();
        firstStart=false;
	}
}

void SimpleCatom3DBlockCode::processLocalEvent(EventPtr pev) {
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
        case EVENT_ROTATION3D_END: {
#ifdef verbose
			info.str("");
			info << "rec.: EVENT_MOTION_END";
			scheduler->trace(info.str(),hostBlock->blockId);
#endif
//            Catoms3DWorld*wrl = Catoms3DWorld::getWorld();
//            Vector3D position=wrl->lattice->gridToWorldPosition(catom->position);
            catom->setPositionAndOrientation(catom->position,catom->orientationCode);
            nextRotation();
            // Rotations3D rotations(catom,voisin,m_1*Vector3D(-1,1,M_SQRT2),ALPHA,m_1*Vector3D(-1,-1, -M_SQRT2),ALPHA);

        }
        break;
        /*case EVENT_NI_RECEIVE: {
            message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
            P2PNetworkInterface * recv_interface = message->destinationInterface;
        }
        break;*/
        default:
            Catoms3DBlockCode::processLocalEvent(pev);
	}
}

void SimpleCatom3DBlockCode::nextRotation() {
    Catoms3DWorld*wrl = Catoms3DWorld::getWorld();
    static const double r1 = 0.453081839321973;
    static const double r2 = 0.4530052159;
    static const double ALPHA=atan(sqrt(2.0)/2.0)*180.0/M_PI;

OUTPUT << "nextRotation step=" << step << ", catom " << catom->blockId << endl;
    int id=1000000;
    int i=0;
    Catoms3DBlock *voisin=NULL;
    P2PNetworkInterface *p2p;
    while (i<12) {
        p2p = catom->getInterface(i);
        if(p2p->connectedInterface && p2p->connectedInterface->hostBlock->blockId<id) {
            voisin = (Catoms3DBlock*)p2p->connectedInterface->hostBlock;
            id = voisin->blockId;
        }
        i++;
    }
    /*Matrix m_1;
    voisin->getGlBlock()->mat.inverse(m_1);*/
    Time t;
    switch (step++) {
        case 0 : {
            Rotations3D rotations(catom,voisin,r1,Vector3D(0,0,1),45.0,Vector3D(0,0,1),45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
         case 1 : {
            OUTPUT << "Or:\t" << currentOr << "\t" << catom->orientationCode << endl;
            Rotations3D rotations(catom,voisin,r1,Vector3D(0,0,1),-45.0,Vector3D(0,0,1),-45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 2 : {
            Rotations3D rotations(catom,voisin,r1,Vector3D(0,0,1),-45.0,Vector3D(0,0,1),-45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 3 : {
            OUTPUT << "Or:\t" << currentOr << "\t" << catom->orientationCode << endl;
            Rotations3D rotations(catom,voisin,r1,Vector3D(0,0,1),45.0,Vector3D(0,0,1),45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 4 : { // 0->5
            Rotations3D rotations(catom,voisin,r1,Vector3D(0,0,1),45.0,Vector3D(-1,1,0),45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 5 : {
            OUTPUT << "Or:\t" << currentOr << "\t" << catom->orientationCode << endl;
            Rotations3D rotations(catom,voisin,r1,Vector3D(-1,1,0),-45.0,Vector3D(0,0,1),-45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 6 : { // 0->2
            Rotations3D rotations(catom,voisin,r1,Vector3D(0,0,1),-45.0,Vector3D(1,1,0),45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
       case 7 : {
            OUTPUT << "Or:\t" << currentOr << "\t" << catom->orientationCode << endl;
            Rotations3D rotations(catom,voisin,r1,Vector3D(1,1,0),-45.0,Vector3D(0,0,1),45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 8 : { // 0->10
            Rotations3D rotations(catom,voisin,r1,Vector3D(0,0,1),45.0,Vector3D(-1,1,0),-45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 9 : {
            OUTPUT << "Or:\t" << currentOr << "\t" << catom->orientationCode << endl;
            Rotations3D rotations(catom,voisin,r1,Vector3D(-1,1,0),45.0,Vector3D(0,0,1),-45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 10 : { // 0->9
            Rotations3D rotations(catom,voisin,r1,Vector3D(0,0,1),-45.0,Vector3D(1,1,0),-45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 11 : {
            OUTPUT << "Or:\t" << currentOr << "\t" << catom->orientationCode << endl;
            Rotations3D rotations(catom,voisin,r1,Vector3D(1,1,0),45.0,Vector3D(0,0,1),45.0);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 12 : { // 0->2
            Rotations3D rotations(catom,voisin,r2,Vector3D(0,1,0),ALPHA,Vector3D(1.0,1.0,M_SQRT2),ALPHA);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 13 : {
            OUTPUT << "Or:\t" << currentOr << "\t" << catom->orientationCode << endl;
            Rotations3D rotations(catom,voisin,r2,Vector3D(1.0,1.0,M_SQRT2),-ALPHA,Vector3D(0,1,0),-ALPHA);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 14 : { // 0->5
            Rotations3D rotations(catom,voisin,r2,Vector3D(0,1,0),ALPHA,Vector3D(1.0,-1.0,M_SQRT2),-ALPHA);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 15 : {
            OUTPUT << "Or:\t" << currentOr << "\t" << catom->orientationCode << endl;
            Rotations3D rotations(catom,voisin,r2,Vector3D(1.0,-1.0,M_SQRT2),ALPHA,Vector3D(0,1,0),-ALPHA);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 16 : { // 0->10
            Rotations3D rotations(catom,voisin,r2,Vector3D(0,1,0),-ALPHA,Vector3D(-1.0,-1.0,M_SQRT2),ALPHA);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 17 : {
            OUTPUT << "Or:\t" << currentOr << "\t" << catom->orientationCode << endl;
            Rotations3D rotations(catom,voisin,r2,Vector3D(-1.0,-1.0,M_SQRT2),-ALPHA,Vector3D(0,1,0),ALPHA);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 18 : { // 0->9
            Rotations3D rotations(catom,voisin,r2,Vector3D(0,1,0),-ALPHA,Vector3D(-1.0,1.0,M_SQRT2),-ALPHA);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        case 19 : {
            OUTPUT << "Or:\t" << currentOr << "\t" << catom->orientationCode << endl;
            Rotations3D rotations(catom,voisin,r2,Vector3D(-1.0,1.0,M_SQRT2),ALPHA,Vector3D(0,1,0),ALPHA);
            t = scheduler->now()+2000;
            scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
        }
        break;
        /*case 20 : {
            // changement de bloc initial
            if (currentOr<23) {
                currentOr++;
                catom->setPositionAndOrientation(catom->position,currentOr);
                Rotations3D rotations(catom,voisin,m_1*Vector3D(0,0,1),45.0,m_1*Vector3D(0,0,1),45.0);
                t = scheduler->now()+2000;
                scheduler->schedule(new Rotation3DStartEvent(t,catom,rotations));
                step=0;
            }
        }*/
    }

    #ifdef verbose
            stringstream info;
            info.str("");
            info << "Rotation3DStartEvent(" << t << ") around #" << voisin->blockId;
            scheduler->trace(info.str(),catom->blockId,LIGHTGREY);
    #endif
}
// bool SimpleCatom3DBlockCode::getAttribute(const string &att,ostringstream &sout) {
//     if (att=="potentiel") {
//         sout << potentiel << endl;
//         return true;
//     }
//     return Catoms3DBlockCode::getAttribute(att,sout);
// }
