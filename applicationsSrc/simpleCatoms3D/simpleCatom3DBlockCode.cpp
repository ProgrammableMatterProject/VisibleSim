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
#include <boost/shared_ptr.hpp>

#define verbose 1

using namespace std;
using namespace Catoms3D;

SimpleCatom3DBlockCode::SimpleCatom3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
	cout << "SimpleCatom3DBlockCode constructor" << endl;
	scheduler = Catoms3D::getScheduler();
	catom = (Catoms3DBlock*)hostBlock;
}

SimpleCatom3DBlockCode::~SimpleCatom3DBlockCode() {
	cout << "SimpleCatom3DBlockCode destructor" << endl;
}

void SimpleCatom3DBlockCode::startup() {
	stringstream info;

	info << "Starting ";
	/*int n=0;
	for (int i=0; i<12; i++) {
        if (catom->getInterface(i)->connectedInterface!=NULL) {
            n++;
        }
	}
	switch (n) {
        case 0 : catom->setColor(DARKGREY); break;
        case 1 : catom->setColor(RED); break;
        case 2 : catom->setColor(ORANGE); break;
        case 3 : catom->setColor(PINK); break;
        case 4 : catom->setColor(YELLOW); break;
        case 5 : catom->setColor(GREEN); break;
        case 6 : catom->setColor(LIGHTGREEN); break;
        case 7 : catom->setColor(LIGHTBLUE); break;
        case 8 : catom->setColor(BLUE); break;
        case 9 : catom->setColor(MAGENTA); break;
        case 10 : catom->setColor(GOLD); break;
        case 11 : catom->setColor(DARKORANGE); break;
        case 12 : catom->setColor(WHITE); break;
	}*/

	/* skeleton test */
	Catoms3DWorld*wrl = Catoms3DWorld::getWorld();
	Vecteur pos(catom->ptrGlBlock->position[0],catom->ptrGlBlock->position[1],catom->ptrGlBlock->position[2]);
	potentiel = wrl->getSkeletonPotentiel(pos);
	catom->setColor(potentiel>1.0?YELLOW:DARKORANGE);

	info << potentiel;
	scheduler->trace(info.str(),hostBlock->blockId);

/*
	if (catom->blockId==1) {
        Vecteur position=wrl->gridToWorldPosition(Cell3DPosition(2,1,0));
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
        Matrice m_1;
        voisin->getGlBlock()->mat.inverse(m_1);
        // recherche le voisin d'indice minimum
        //Rotations rotations(catom,voisin,m_1*Vecteur(0,1,0),35.2643896828,m_1*Vecteur(-1,1, -M_SQRT2),35.2643896828);
        Rotations rotations(catom,voisin,m_1*Vecteur(0,0,1),45.0,m_1*Vecteur(-1,1,0),45.0);
        uint64_t t = scheduler->now()+2000;
        scheduler->schedule(new MotionStartEvent(t,catom,rotations));
#ifdef verbose
        stringstream info;
        info.str("");
        info << "MotionStartEvent(" << t << ") around #" << voisin->blockId;
        scheduler->trace(info.str(),catom->blockId,LIGHTGREY);
#endif
	}*/
}

void SimpleCatom3DBlockCode::processLocalEvent(EventPtr pev) {
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
        case EVENT_MOTION_END: {
#ifdef verbose
			info.str("");
			info << "rec.: EVENT_MOTION_END";
			scheduler->trace(info.str(),hostBlock->blockId);
#endif
            Catoms3DBlock *voisin=NULL;
            P2PNetworkInterface *p2p;
            int i=0,id=10000;
            while (i<12) {
                p2p = catom->getInterface(i);

                if(p2p->connectedInterface && p2p->connectedInterface->hostBlock->blockId<id) {
                    voisin = (Catoms3DBlock*)p2p->connectedInterface->hostBlock;
                    id = voisin->blockId;
                }
                i++;
            }
            Matrice m_1;
            voisin->getGlBlock()->mat.inverse(m_1);
            // Rotations rotations(catom,voisin,m_1*Vecteur(-1,1,M_SQRT2),35.26,m_1*Vecteur(-1,-1, -M_SQRT2),35.26);
            Rotations rotations(catom,voisin,m_1*Vecteur(-1,-1,0),45.0,m_1*Vecteur(0,0,1),45.0);
            uint64_t t = scheduler->now()+1002000;
            // scheduler->schedule(new MotionStartEvent(t,catom,rotations));
        }
        break;
        case EVENT_NI_RECEIVE: {
            message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
            P2PNetworkInterface * recv_interface = message->destinationInterface;


        }
        break;
	}
}

Catoms3DBlockCode* SimpleCatom3DBlockCode::buildNewBlockCode(Catoms3DBlock *host) {
	return(new SimpleCatom3DBlockCode(host));
}

bool SimpleCatom3DBlockCode::getAttribute(const string &att,ostringstream &sout) {
    if (att=="potentiel") {
        sout << potentiel << endl;
        return true;
    }
    return Catoms3DBlockCode::getAttribute(att,sout);
}
