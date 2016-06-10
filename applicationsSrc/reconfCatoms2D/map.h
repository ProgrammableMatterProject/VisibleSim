/*
 * map.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef MAP_H_
#define MAP_H_

#include "catoms2DBlockCode.h"
#include "catoms2DSimulator.h"
#include "catoms2DScheduler.h"
#include "catoms2DBlock.h"
#include "network.h"
#include <boost/shared_ptr.hpp>
#include "coordinate.h"
#include "border.h"

class Map {
 public:  
  // Map construction
  static Coordinate ccth;
  static bool isConnected;  

  Catoms2D::Catoms2DBlock* catom2D;
  bool connectedToHost;
  bool positionKnown;
  Coordinate position;
  int waiting;
  P2PNetworkInterface *toHost;
  Border border;

  Map(Catoms2D::Catoms2DBlock* host);
  Map(const Map &m);
  ~Map();

  void connectToHost();
  bool handleMessage(MessagePtr message);
  Coordinate getPosition(P2PNetworkInterface *it);
  void setPosition(Coordinate p);
  Coordinate getPosition();
  Coordinate real2Virtual(Coordinate p);
  Coordinate virtual2Real(Coordinate p);
  static Coordinate real2Virtual(Coordinate o, Coordinate p);
  static Coordinate virtual2Real(Coordinate o, Coordinate p);
  static Coordinate getPosition(Catoms2D::Catoms2DBlock* catom2D, Coordinate p, P2PNetworkInterface *it);

  void buildMap();
  void mapBuilt(P2PNetworkInterface *d);

  P2PNetworkInterface* getInterface(Coordinate &pos);
  P2PNetworkInterface* getClosestInterface(Coordinate dest, P2PNetworkInterface *ignore);
  int distance(Coordinate p1, Coordinate p2);
  int distance(Coordinate p2);

  static bool areNeighbors(Coordinate p1, Coordinate p2);
  static bool isInTarget(Coordinate p);

  Catoms2D::Catoms2DBlock* getOnBorderNeighbor(Catoms2D::RelativeDirection::Direction d);
  P2PNetworkInterface* getOnBorderNeighborInterface(Catoms2D::RelativeDirection::Direction d);
  Coordinate getOnBorderNeighborCoordinate(Catoms2D::RelativeDirection::Direction d);

};

// Messages
#define GO_MAP_MSG 15000
#define BACK_MAP_MSG 15001

class GoMapMessage;
typedef boost::shared_ptr<GoMapMessage> GoMapMessage_ptr;

class GoMapMessage : public Message {
 protected:
  Coordinate position;
 public:
  

 GoMapMessage(Coordinate p) : Message() { 
    type = GO_MAP_MSG;
    position = p;
  };

 GoMapMessage(GoMapMessage *m) : Message() { 
    type = m->type;
    position = m->position;
  };
  
  ~GoMapMessage() {};
  Coordinate getPosition() {return position;};
  unsigned int size() { return 17;};
};

class BackMapMessage;
typedef boost::shared_ptr<BackMapMessage> BackMapMessage_ptr;

class BackMapMessage : public Message {
 public:
 BackMapMessage() : Message() { 
    type = BACK_MAP_MSG;
  };

 BackMapMessage(BackMapMessage *m) : Message() { 
    type = m->type;
  };
  
  ~BackMapMessage() {};
  unsigned int size() { return 17;};
};

#endif
