/*
 * c2sr.h
 *
 *  Created on: 25 Nov 2015
 *      Author: Andre Naz
 */

#ifndef C2SR_H_
#define C2SR_H_

#include <list>
#include "robots/catoms2D/catoms2DBlock.h"
#include "robots/catoms2D/catoms2DRotationEvents.h"

#include "comm/network.h"
#include "map.h"
#include "border.h"

#define CATOMS_RADIUS 0.5

enum C2SRState_t {NOT_SET = -2,
         UNKNOWN = -1,
         BLOCKED,
         WAITING,
         ASK_TO_MOVE,
         MOVING,
         GOAL
};

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

class C2SR {
private:
   C2SRState_t state;
   Catoms2D::Catoms2DBlock *catom;
   Map *map;
   bool started;

   std::list<Coordinate> movings;

   std::list<ClearanceRequest> pendingRequests;

   Clearance currentClearance;

   void init();

   Coordinate getPositionAfterRotationAround(Neighbor &pivot);
   bool isFree();
   void move(Clearance &c);

   // new version
   bool isInStream ();
   void requestClearance();
   bool checkConvergence();

   // State management
   void hasConverged();
   void setState(C2SRState_t s);

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

public:

   C2SR(Catoms2D::Catoms2DBlock *c, Map *m);
   ~C2SR();

   void start();
   void handle(MessagePtr m);
   void handleStopMovingEvent();

   static bool isDone();
   static std::string toString(C2SRState_t s);

};

#endif
