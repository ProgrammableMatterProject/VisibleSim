/*
 * reconfiguration.h
 *
 *  Created on: 12 avril 2013
 *      Author: andre
 */

#ifndef RECONFIGURATION_H_
#define RECONFIGURATION_H_

#include "catoms2DBlock.h"
#include "network.h"
#include "map.h"

class Reconfiguration {
    
public:
  enum state_t {UNKNOWN = -1, GOAL, CANT_MOVE, CAN_MOVE, MOVING};
   
private:
   state_t state;
   Catoms2D::Catoms2DBlock *catom;
   Map *map;

   void init();
   void tryToMove();

   bool isFree(); 
   bool isInTarget();
   bool isOnBorder();

   Catoms2D::Rotation2DMove* nextMove();

public:
   
   Reconfiguration(Catoms2D::Catoms2DBlock *c, Map *m);
   ~Reconfiguration();
   void handle(MessagePtr m);
   
   void start();
   static bool isDone();
};

#endif
