/*
 * catom2D1BlockCode.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef CATOM2D1BLOCKCODE_H_
#define CATOM2D1BLOCKCODE_H_

#include "catoms2DBlockCode.h"
#include "catoms2DSimulator.h"
#include "catoms2DScheduler.h"
#include "catoms2DBlock.h"
#include "localTupleSpace.hpp"
#include "tuple.hpp"
#include "contextTuple.hpp"
#include "reconfCatoms2DMessages.h"

class Catoms2D1BlockCode : public Catoms2D::Catoms2DBlockCode {
 private:
  static Coordinate ccth;
 public:

  Catoms2D::Catoms2DScheduler *scheduler;
  Catoms2D::Catoms2DBlock *catom2D;

  Catoms2D1BlockCode (Catoms2D::Catoms2DBlock *host);
  ~Catoms2D1BlockCode ();
  // Map construction
  bool connectedToHost;
  bool positionKnown;
  Coordinate position;
  //Coordinate getPosition(P2PNetworkInterface *recv_it, Coordinate c);
  Coordinate getPosition(P2PNetworkInterface *it);
  void setPosition(Coordinate p);
  int waiting;
  P2PNetworkInterface *toHost;
  void buildMap();
  void mapBuilt(P2PNetworkInterface *d);

  // Geo routing
  int distance(Coordinate p1, Coordinate p2);
  P2PNetworkInterface* getClosestInterface(Coordinate dest);
  void forward(GeoMessage_ptr m, P2PNetworkInterface *p2p);
  
  // Tuple space
  LocalTupleSpace localTuples;
  void out(ContextTuple *t);

  void startup();
  void processLocalEvent(EventPtr pev);
	
  void startMotion(int direction, Catoms2D::Catoms2DBlock *pivot);
	
  void updateBorder();
  bool canMove();

  static Catoms2D::Catoms2DBlockCode *buildNewBlockCode(Catoms2D::Catoms2DBlock *host);
};

#endif /* CATOM2DBLOCKCODE_H_ */
