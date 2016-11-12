/*
 * reconfCatoms3DBlockCode.cpp
 *
 *  Created on: 17 October 2016
 *  Author: Thadeu
 */

#include <iostream>
#include <sstream>
#include "reconfCatoms3DBlockCode.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "csgUtils.h"
#include "events.h"

using namespace std;
using namespace Catoms3D;

CSGNode* ReconfCatoms3DBlockCode::csgRoot = NULL;

ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
	cout << "ReconfCatoms3DBlockCode constructor" << endl;
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;
}

ReconfCatoms3DBlockCode::~ReconfCatoms3DBlockCode() {
	cout << "ReconfCatoms3DBlockCode destructor" << endl;
}

void ReconfCatoms3DBlockCode::startup() {
	stringstream info;
	info << "Starting  ";
	scheduler->trace(info.str(),hostBlock->blockId);

    hasPosition = false;
	if (catom->blockId==1) {
        csgRoot = csgUtils.readFile("data/mug.bc");
        csgRoot->toString();
        BoundingBox bb;
        csgRoot->boundingBox(bb);
        cout << "Bounding box: " << bb.P0 << ' ' << bb.P1 << endl;

        myPosition = bb.P0;

        Color color;
        if (csgRoot->isInside(myPosition, color)) {
            catom->setColor(color);
        }
        else {
            catom->setVisible(false);
        }

        hasPosition = true;
        sendCSGMessage();
	}
}


void ReconfCatoms3DBlockCode::processLocalEvent(EventPtr pev) {
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
    case EVENT_NI_RECEIVE: {
      message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
        switch(message->id) {
            case CSG_MSG_ID:
            {
                if (!hasPosition) {
                    catom->setColor(PINK);
                    CSG_message_ptr recv_message = std::static_pointer_cast<CSG_message>(message);

                    myPosition = recv_message->getPosition();

                    Color color;
                    if (csgRoot->isInside(myPosition, color)) {
                        catom->setColor(color);
                    }
                    else {
                        catom->setVisible(false);
                    }

                    hasPosition = true;
                    sendCSGMessage();
                }
                break;
            }
          }
      }
      break;
	}
}

void ReconfCatoms3DBlockCode::sendCSGMessage() {
    for (int i = 0; i < 12; i++) {
        if (catom->getInterface(i)->connectedInterface != NULL) {
            Vector3D pos(
                myPosition.pt[0] + Catoms3D::tabConnectorPositions[i][0], 
                myPosition.pt[1] + Catoms3D::tabConnectorPositions[i][1],
                myPosition.pt[2] + Catoms3D::tabConnectorPositions[i][2],
                1);
            CSG_message *message = new CSG_message(csgUtils.getCSGBuffer(), csgUtils.getCSGBufferSize(), pos);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, message, catom->getInterface(i)));
        }
    }
}

CSG_message::CSG_message(char *_csgBuffer, int _csgBufferSize, Vector3D pos) {
	id = CSG_MSG_ID;

//    csgBuffer = new char[_csgBufferSize];
//    memcpy(csgBuffer, _csgBuffer, _csgBufferSize);

//    csgBufferSize = _csgBufferSize;

    position = pos;
}

CSG_message::~CSG_message() {
}

BlockCode* ReconfCatoms3DBlockCode::buildNewBlockCode(BuildingBlock *host) {
    return (new ReconfCatoms3DBlockCode((Catoms3DBlock*)host));
}

