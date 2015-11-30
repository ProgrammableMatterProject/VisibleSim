/*
 * reconfiguration.cpp
 *
 *  Created on: 27 nov 2015
 *      Author: andre
 */

#include <iostream>
#include <boost/shared_ptr.hpp>

#include "catoms2DWorld.h"
#include "scheduler.h"
#include "events.h"

#include "reconfiguration.h"
#include "reconfigurationMsg.h"
#include "gridUtils.h"

using namespace std;
using namespace Catoms2D;

Reconfiguration::Reconfiguration(Catoms2DBlock *c, Map &m): map(m) {
   catom = c;
   state = UNKNOWN;
}

Reconfiguration::~Reconfiguration() {}

void Reconfiguration::handle(MessagePtr m) {
   ReconfigurationMsg_ptr rm = boost::static_pointer_cast<ReconfigurationMsg>(m);
   switch(rm->subtype) {
      case ReconfigurationMsg::STATE_QUERY: {
      }
      break;
      case ReconfigurationMsg::STATE_ANSWER: {
      
      }
      break;
      default:
         cerr << "unknown reconfiguration message type" << endl;
   }
}

void Reconfiguration::start() {
  tryToMove();
}

void Reconfiguration::tryToMove() {
   if (canMove()) {
      catom->setColor(GREEN);
   } else {
      catom->setColor(RED);
   }
}

bool Reconfiguration::canMove() {
  // at least 3 empty faces!
  return (catom->nbConsecutiveEmptyFaces(true) >= 3);
}

bool Reconfiguration::isDone() {
   Catoms2DWorld *world = Catoms2DWorld::getWorld();
      int *gridSize = world->getGridSize();
      for (int iy = 0; iy < gridSize[2]; iy++) {
         for (int ix = 0; ix < gridSize[0]; ix++) {
            if (world->getTargetGrid(ix,0,iy) == fullCell ) {
               if (!world->getGridPtr(ix,0,iy)) {
	               return false;
	         }
         }
      }
   }
   return true;
}
