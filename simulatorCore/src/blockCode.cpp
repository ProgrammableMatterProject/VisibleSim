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
	return sendMessage(NULL, msg, dest, t0, dt);
}

int BlockCode::sendMessage(const char*msgString,Message*msg,P2PNetworkInterface *dest,int t0,int dt) {
  int t1 = scheduler->now() + t0 + (int)(((double)dt*hostBlock->getRandomUint())/((double)uintRNG::max()));

	if (msgString)
		console << " sends " << msgString << " to " << dest->getConnectedBlockId() << " at " << t1 << "\n";

	OUTPUT << hostBlock->blockId << " sends " << msg->type << " to "
		   << dest->connectedInterface->hostBlock->blockId << " at " << t1 << endl;

    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(t1, msg, dest));
    return 0;
}

int BlockCode::sendMessageToAllNeighbors(Message*msg,int t0,int dt,int nexcept,...) {
	va_list args;
	va_start(args,nexcept);
	int ret = sendMessageToAllNeighbors(NULL, msg, t0, dt, nexcept, args);
	va_end(args);

	return ret;
}

int BlockCode::sendMessageToAllNeighbors(const char*msgString, Message*msg,int t0,int dt,int nexcept,...) {
	va_list args;
	va_start(args,nexcept);
	int ret = sendMessageToAllNeighbors(msgString, msg, t0, dt, nexcept, args);
	va_end(args);

	return ret;
}

int BlockCode::sendMessageToAllNeighbors(const char*msgString, Message*msg,int t0,int dt, int nexcept, va_list args) {
    P2PNetworkInterface *tabExceptions[hostBlock->getNbInterfaces()];
    for (int i=0; i<nexcept; i++) {
        tabExceptions[i] = va_arg(args,P2PNetworkInterface*);
    }

    P2PNetworkInterface *p2p;
    int j,n=0;
    for (int i=0; i<hostBlock->getNbInterfaces(); i++) {
        p2p = hostBlock->getInterface(i);
        if(p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
            j=0;
            while (j<nexcept && p2p!=tabExceptions[j]) j++;
            if (j==nexcept) {
				sendMessage(msgString, msg->clone(), p2p, t0, dt);
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
            OUTPUT << "ADD_NEIGHBOR" << endl;
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

bool BlockCode::loadNextTarget() {
	target = Target::loadNextTarget();

	return target != NULL;
}

} // BaseSimulator namespace
