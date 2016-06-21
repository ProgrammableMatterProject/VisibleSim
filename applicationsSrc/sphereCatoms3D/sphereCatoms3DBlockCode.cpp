/*
 * sphereCatoms3DBlockCode.cpp
 *
 *  Created on: 03 May 2016
 *  Author: Thadeu
 */

#include <iostream>
#include <sstream>
#include "sphereCatoms3DBlockCode.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"
#include <boost/shared_ptr.hpp>
#include <chrono>
#include <fstream>
//#define MAX 1000
#define INF 2000000000

using namespace std;
using namespace Catoms3D;

int SphereCatoms3DBlockCode::radius[1000000] = {0};

SphereCatoms3DBlockCode::SphereCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
	cout << "SphereCatoms3DBlockCode constructor" << endl;
	scheduler = Catoms3D::getScheduler();
	catom = (Catoms3DBlock*)hostBlock;
    distance = INF;
}

SphereCatoms3DBlockCode::~SphereCatoms3DBlockCode() {
	cout << "SphereCatoms3DBlockCode destructor" << endl;
}

void SphereCatoms3DBlockCode::startup() {
	stringstream info;

	info << "Starting  ";

    info << "POSITION = " << catom->position << endl;
	scheduler->trace(info.str(),hostBlock->blockId);

	if (catom->blockId==3333) {
        distance = 0;
        sendDistanceMessage();
	}
}


void SphereCatoms3DBlockCode::processLocalEvent(EventPtr pev) {
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
    case EVENT_NI_RECEIVE: {
      message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
        switch(message->id) {
            case DISTANCE_MSG_ID:
            {
                Distance_message_ptr recv_message = boost::static_pointer_cast<Distance_message>(message);
                if (recv_message->getDistance() < distance) {
                    if (distance != INF) {
                        cout << "hihihi" << endl;
                        radius[distance]--;
                    }
                    distance = recv_message->getDistance();
                    radius[distance]++;

                    sendDistanceMessage();
                    if (distance == 1) {
                        catom->setColor(YELLOW);
                        catom->setVisible(true);
                    }
                    else if (distance == 2) {
                        catom->setColor(GREEN);
                        catom->setVisible(true);
                    }
        /*            else if (distance == 3) {
                        catom->setColor(RED);
                        catom->setVisible(true);
                    }
                    */
                    else {
                        catom->setVisible(false);
                    }
                }
            }
          }
      }
      break;
	}
}

void SphereCatoms3DBlockCode::sendDistanceMessage() {
    for (int i = 0; i < 12; i++) {
        if (catom->getInterface(i)->connectedInterface != NULL) {
            Distance_message *message = new Distance_message(distance+1);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, message, catom->getInterface(i)));
        }
    }
}

Catoms3DBlockCode* SphereCatoms3DBlockCode::buildNewBlockCode(Catoms3DBlock *host) {
	return(new SphereCatoms3DBlockCode(host));
}

Distance_message::Distance_message(int _distance) {
    id = DISTANCE_MSG_ID;
    distance = _distance;
}

