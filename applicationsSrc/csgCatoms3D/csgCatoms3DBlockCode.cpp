/*
 * catom2D1BlockCode.cpp
 *
 *  Created on: 05 August 2015
 *  Author: Thadeu
 */

#include <iostream>
#include <sstream>
#include "csgCatoms3DBlockCode.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"
#include <boost/shared_ptr.hpp>

using namespace std;
using namespace Catoms3D;

CsgCatoms3DBlockCode::CsgCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
	cout << "CsgCatoms3DBlockCode constructor" << endl;
	scheduler = Catoms3D::getScheduler();
	catom = (Catoms3DBlock*)hostBlock;
}

CsgCatoms3DBlockCode::~CsgCatoms3DBlockCode() {
	cout << "CsgCatoms3DBlockCode destructor" << endl;
}

void CsgCatoms3DBlockCode::startup() {
	stringstream info;

	info << "Starting  ";

    Vecteur basePosition(4, 4, 4);
    info << "POSITION = " << catom->position << endl;
	scheduler->trace(info.str(),hostBlock->blockId);
    hasPosition = false;

	if (catom->blockId==1) {
        csgUtils.readCSGFile("out.bc");
        if (csgUtils.isInCSG(myPosition))
            catom->setColor(YELLOW);
        else 
            catom->setColor(PINK);
            //catom->setVisible(false);
        myPosition = Vecteur(0, 0, 0);
        hasPosition = true;

        sendCSGMessage();
	}
}

void CsgCatoms3DBlockCode::processLocalEvent(EventPtr pev) {
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
    case EVENT_NI_RECEIVE: {
      message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
        switch(message->id) {
            case CSG_MSG_ID:
            {
                if (!hasPosition) {
                    catom->setColor(PINK);
                    CSG_message_ptr recv_message = boost::static_pointer_cast<CSG_message>(message);

                    char *csgBuffer = recv_message->getCsgBuffer();
                    int csgBufferSize = recv_message->getCsgBufferSize();
                    csgUtils.readCSGBuffer(csgBuffer, csgBufferSize);

                    myPosition = recv_message->getPosition();
                    //catom->setColor(isInCSG() ? YELLOW: PINK);
                    if (csgUtils.isInCSG(myPosition))
                        catom->setColor(YELLOW);
                    else 
                        catom->setColor(PINK);
                        //catom->setVisible(false);
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

void CsgCatoms3DBlockCode::sendCSGMessage() {
    for (int i = 0; i < 12; i++) {
        if (catom->getInterface(i)->connectedInterface != NULL) {
            Vecteur pos(
                myPosition.pt[0] + Catoms3D::tabConnectorPositions[i][0], 
                myPosition.pt[1] + Catoms3D::tabConnectorPositions[i][1],
                myPosition.pt[2] + Catoms3D::tabConnectorPositions[i][2]);
            CSG_message *message = new CSG_message(csgUtils.getCSGBuffer(), csgUtils.getCSGBufferSize(), pos);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, message, catom->getInterface(i)));
        }
    }
}


Catoms3DBlockCode* CsgCatoms3DBlockCode::buildNewBlockCode(Catoms3DBlock *host) {
	return(new CsgCatoms3DBlockCode(host));
}

CSG_message::CSG_message(char *_csgBuffer, int _csgBufferSize, Vecteur pos) {
	id = CSG_MSG_ID;

    csgBuffer = new char[_csgBufferSize];
    memcpy(csgBuffer, _csgBuffer, _csgBufferSize);

    csgBufferSize = _csgBufferSize;

    position = pos;
}

CSG_message::~CSG_message() {
    delete[] csgBuffer;
}


