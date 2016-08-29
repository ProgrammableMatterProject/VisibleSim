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
#include "rotation2DEvents.h"

#include "network.h"
#include "map.h"
#include "border.h"

#define CATOMS_RADIUS 0.5
 
enum reconfigurationState_t {NOT_SET = -2, UNKNOWN = -1, BLOCKED, WAITING, ASK_TO_MOVE, MOVING, GOAL};

class ClearanceRequest {
 public:
  Coordinate src;
  Coordinate dest;
  int cnt;
  
  ClearanceRequest();
  ClearanceRequest(Coordinate &s,Coordinate &d, int c);
  ClearanceRequest(const ClearanceRequest &cr);
  ~ClearanceRequest();

  std::string toString();
};

class Clearance {
 public:
  Coordinate src;
  Coordinate dest;
  
  Clearance();
  Clearance(Coordinate &s,Coordinate &d);
  Clearance(const Clearance &c);
  ~Clearance();

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
   std::list<Coordinate> movings;
   
   std::list<ClearanceRequest> pendingRequests;
   
   Clearance currentClearance;
   
   void init();

   bool tryToMove();
   Coordinate getPositionAfterRotationAround(Neighbor &pivot);
   bool shouldMove(Coordinate &src, Coordinate &pivot, Coordinate &dest);
   bool isFree();
   void move(Clearance &c);

   // new version
   bool isInStream ();
   void requestClearance();
   bool checkConvergence();
   
   // State management
   void hasConverged();
   void setState(reconfigurationState_t s);

   // movings management
   bool isMoving(Coordinate &c);
   bool isNeighborToMoving(Coordinate &c);
   void removeMoving(Coordinate &c);
   void insertMoving(Coordinate &c);
   void printMoving();

   // pending request management
   bool isAPendingRequestDestNeighborWith(Coordinate &p);
   ClearanceRequest getPendingRequestDestNeighborWith(Coordinate &p);
   void insertPendingRequest(ClearanceRequest &pr);
   void printPendingRequest();
   
   // message management
   void send(Message *m, P2PNetworkInterface *i);
   void forwardClearance(Clearance &c, P2PNetworkInterface *recv, unsigned int h);
   void forwardEndMove(Clearance &c, P2PNetworkInterface *recv, unsigned int h);
   
   void printNeighbors();
   
   // space checker
   static std::list<Catoms2D::Catoms2DBlock*> movingModules;
   bool checkSpace();
   std::vector<Coordinate> getSafetyZone();
   void insertMovings(Catoms2D::Catoms2DBlock *c);
   void removeMovings(Catoms2D::Catoms2DBlock *c);
   double distance(Vector3D &p1, Vector3D &p2);
   
   /*void updatePosition();
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
   bool shouldMove(P2PNetworkInterface *pivot, PerimeterCaseState &pcs);
   void forwardStateUpdate(P2PNetworkInterface *p2p, PerimeterCaseState &pcs);
   Rotation2DMove* nextMove();*/

   
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
