/*
 * blockCode.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "blockCode.h"
#include "trace.h"
#include "scheduler.h"
#include "buildingBlock.h"
#include "world.h"
#include "lattice.h"

using namespace std;

namespace BaseSimulator {

Target *BlockCode::target = NULL;
// TiXmlNode *BlockCode::xmlTargetListNode = NULL;

BlockCode::BlockCode(BuildingBlock *host) : hostBlock(host) {
	scheduler = getScheduler();
	lattice = getWorld()->lattice;
	console.setInfo(scheduler, hostBlock->blockId);
	addDebugAttributes(scheduler);
}

BlockCode::~BlockCode() {
	if (target) {
		delete target;
		target = NULL;
	}
		
	eventFuncMap.clear();
}

void BlockCode::addMessageEventFunc(int type,eventFunc func) {
    eventFuncMap.insert(pair<int,eventFunc>(type,func));
}

int BlockCode::sendMessage(Message*msg,P2PNetworkInterface *dest,int t0,int dt) {
    int t1 = scheduler->now() + t0 + (int)(((double)dt*rand())/RAND_MAX);

    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(t1, msg, dest));
    return 0;
}

int BlockCode::sendMessage(const char*msgString,Message*msg,P2PNetworkInterface *dest,int t0,int dt) {
    int t1 = scheduler->now() + t0 + (int)(((double)dt*rand())/RAND_MAX);

    console << " sends " << msgString << " to " << dest->getConnectedBlockId() << " at " << t1 << "\n";
    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(t1, msg, dest));
    return 0;
}

int BlockCode::sendMessageToAllNeighbors(Message*msg,int t0,int dt,int nexcept,...) {
    va_list ap;
    P2PNetworkInterface *tabExceptions[hostBlock->getNbInterfaces()];
//    int n = va_arg(vl,int);
    va_start(ap,nexcept);
    for (int i=0; i<nexcept; i++) {
        tabExceptions[i] = va_arg(ap,P2PNetworkInterface*);
    }
    va_end(ap);

    P2PNetworkInterface *p2p;
    int j,n=0,t1;
    for (int i=0; i<hostBlock->getNbInterfaces(); i++) {
        p2p = hostBlock->getInterface(i);
        if(p2p && p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
            j=0;
            while (j<nexcept && p2p!=tabExceptions[j]) j++;
            if (j==nexcept) {
                t1 = scheduler->now() + t0 + (int)(((double)dt*rand())/RAND_MAX);
                OUTPUT << hostBlock->blockId << " sends " << msg->type << " to " << p2p->connectedInterface->hostBlock->blockId << " at " << t1 << endl;
                Message* msg_clone = msg->clone();
                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(t1, msg_clone, p2p));
                n++;
            }
        }
    }
    delete msg;
    return n;
}

int BlockCode::sendMessageToAllNeighbors(const char*msgString, Message*msg,int t0,int dt,int nexcept,...) {
    va_list ap;
    P2PNetworkInterface *tabExceptions[hostBlock->getNbInterfaces()];
//    int n = va_arg(vl,int);
    va_start(ap,nexcept);
    for (int i=0; i<nexcept; i++) {
        tabExceptions[i] = va_arg(ap,P2PNetworkInterface*);
    }
    va_end(ap);

    P2PNetworkInterface *p2p;
    int j,n=0,t1;
    for (int i=0; i<hostBlock->getNbInterfaces(); i++) {
        p2p = hostBlock->getInterface(i);
        if(p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
            j=0;
            while (j<nexcept && p2p!=tabExceptions[j]) j++;
            if (j==nexcept) {
                t1 = scheduler->now() + t0 + (int)(((double)dt*rand())/RAND_MAX);
                console << " sends " << msgString << " to " << p2p->getConnectedBlockId() << " at " << t1 << "\n";
                OUTPUT << hostBlock->blockId << " sends " << msg->type << " to " << p2p->connectedInterface->hostBlock->blockId << " at " << t1 << endl;
                Message* msg_clone = msg->clone();
                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(t1, msg_clone, p2p));
                n++;
            }
        }
    }
    delete msg;
    return n;
}

void BlockCode::processLocalEvent(EventPtr pev) {
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
            startup();
        } break;
        case EVENT_TAP: {
			int face = (std::static_pointer_cast<TapEvent>(pev))->tappedFace;
            onTap(face);
        } break;
    }
}

void BlockCode::onTap(int face) {
	stringstream info;
	info.str("");
	info << "Tapped on face " << lattice->getDirectionString(face);
	scheduler->trace(info.str(),hostBlock->blockId);	
}

} // BaseSimulator namespace
