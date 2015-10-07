/*
 * catom2D1BlockCode.cpp
 *
 *  Created on: 05 August 2015
 *  Author: Thadeu
 */

#include <iostream>
#include <sstream>
#include "catoms3DWorld.h"
#include "csgCatoms3DBlockCode.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"
#include <boost/shared_ptr.hpp>

#define verbose 1

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

void CsgCatoms3DBlockCode::createCSG() {
    CsgNode difference(node_t::bool_op , new BoolOperator(BoolOperator::bool_operator_t::bool_difference));

    CsgNode union1(node_t::bool_op, new BoolOperator(BoolOperator::bool_operator_t::bool_union));
    CsgNode cube1(node_t::shape, new Cube(8, 8, 1));

    CsgNode translate(node_t::transformation, 
        new Transformation(Transformation::transformation_t::translate, 4, 4, 1));
    CsgNode cylinder1(node_t::shape, new Cylinder(7, 4));
    translate.addChild(cylinder1);

    union1.addChild(cube1);
    union1.addChild(translate);

    CsgNode translate2(node_t::transformation, new Transformation(Transformation::transformation_t::translate, 4, 4, 1));
    CsgNode cylinder2(node_t::shape, new Cylinder(7, 1.5));
    translate2.addChild(cylinder2);

    difference.addChild(union1);
    difference.addChild(translate2);

    csgTree.addChild(difference);
}

void CsgCatoms3DBlockCode::startup() {
	stringstream info;

	info << "Starting ";

    Vecteur basePosition(4, 4, 4);
    info << "POSITION = " << catom->position << endl;
	scheduler->trace(info.str(),hostBlock->blockId);
    hasPosition = false;

	if (catom->blockId==1) {
        createCSG();
        catom->setColor(isInCSG() ? YELLOW: PINK);
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
                    csgTree = recv_message->getCsgTree();
                    myPosition = recv_message->getPosition();
                    catom->setColor(isInCSG() ? YELLOW: PINK);
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
            CSG_message *message = new CSG_message(csgTree, pos);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 1000000, message, catom->getInterface(i)));
        }
    }
}

bool CsgCatoms3DBlockCode::isInCSG() {
    Vecteur v(0,0,0);
    return isInCSG(csgTree, v);
}

bool CsgCatoms3DBlockCode::isInCSG(CsgNode &node, Vecteur basePosition) {
    switch (node.getType())
    {
        case node_t::shape: {
            Shape3D *shape = static_cast<Shape3D *>(node.getValue());
            return shape->isInside(basePosition, myPosition);
        } break;

        case node_t::transformation: {
            Transformation* t_op = static_cast<Transformation *>(node.getValue());
            if (t_op->my_type == Transformation::transformation_t::translate) {
                for (unsigned int i = 0; i < node.vchildren.size(); i++) {
                    Vecteur transf_position(t_op->x, t_op->y, t_op->z);
                    if (isInCSG(node.vchildren[i], transf_position))
                        return true;
                }
                return false;
            }
        } break;

        case node_t::bool_op: {
            BoolOperator* b_op = static_cast<BoolOperator *>(node.getValue());
            if (b_op->my_type == BoolOperator::bool_operator_t::bool_union) {
                for (unsigned int i = 0; i < node.vchildren.size(); i++) {
                    if (isInCSG(node.vchildren[i], basePosition))
                        return true;
                }
            }
            if (b_op->my_type == BoolOperator::bool_operator_t::bool_difference) {
                if (node.vchildren.size() >= 1) {
                    if (isInCSG(node.vchildren[0], basePosition)) {
                        for (unsigned int i = 1; i < node.vchildren.size(); i++) {
                            if (isInCSG(node.vchildren[i], basePosition))
                                return false;
                        }
                        return true;
                    }
                }
            }
        } break;
    }

    return false;
}

Catoms3DBlockCode* CsgCatoms3DBlockCode::buildNewBlockCode(Catoms3DBlock *host) {
	return(new CsgCatoms3DBlockCode(host));
}

CSG_message::CSG_message(CsgNode node, Vecteur pos) {
	id = CSG_MSG_ID;
	csgTree = node;
    position = pos;
}

CSG_message::~CSG_message() {
}
