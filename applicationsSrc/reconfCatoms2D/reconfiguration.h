/*
 * reconfiguration.h
 *
 *  Created on: 12 avril 2013
 *      Author: andre
 */

#ifndef RECONFIGURATION_H_
#define RECONFIGURATION_H_

#include <list>
#include "catoms2DBlock.h"
#include "network.h"
#include "map.h"
#include "border.h"

enum reconfigurationState_t {NOT_SET = -2, UNKNOWN = -1, BLOCKED, WAITING, ASK_TO_MOVE, MOVING, GOAL};

class PerimeterCaseState {
 public:
  Coordinate cell;
  std::list<reconfigurationState_t> states;

  PerimeterCaseState();
  PerimeterCaseState(Coordinate c, list<reconfigurationState_t> s);
  PerimeterCaseState(const PerimeterCaseState &pcs);
  ~PerimeterCaseState();

  void reset(Coordinate &c);
  std::string toString();
};

class Reconfiguration {  
private:
   reconfigurationState_t state;
   Catoms2D::Catoms2DBlock *catom;
   Map *map;
   bool started;
   
   //Border *border;
   //PerimeterCaseState rotationDirectionCell;
   //PerimeterCaseState antiRotationDirectionCell;
   std::list<Coordinate> moving;
   int nbWaitingAuthorization;
   
   void init();
   void updatePosition();
   void updateState();
   
   int advertiseBeforeMoving();
   bool isMoving(Coordinate &c);
   bool isNeighborToMoving(Coordinate &c);
   void removeMoving(Coordinate &c);
   void forwardStopMoving(P2PNetworkInterface *p2p, Coordinate &c);
   void printMoving();
   
   void move();
   void queryStates();
   P2PNetworkInterface* getPivot();
   Coordinate getCellAfterRotationAround(P2PNetworkInterface *pivot);
   bool isFree(); 
   bool isInTarget();
   bool isOnBorder();
   bool hasConverged();
   
   P2PNetworkInterface *canMove(PerimeterCaseState &pcs);
   bool shouldMove(P2PNetworkInterface *pivot);
   void forwardStateUpdate(P2PNetworkInterface *p2p, PerimeterCaseState &pcs);
   
   Catoms2D::Catoms2DMove* nextMove();
   
public:
   
   Reconfiguration(Catoms2D::Catoms2DBlock *c, Map *m);
   ~Reconfiguration();

   void start();
   void handle(MessagePtr m);
   void handleStopMovingEvent();

   static bool isDone();
   static std::string toString(reconfigurationState_t s);
};

#endif
