/*
 * reconf2DCatomsMessages.h
 *
 *  Created on: 04/05/2015
 *      Author: andre
 */

#ifndef RECONFCATOMS2DMESSAGES_H_
#define RECONFCATOMS2DMESSAGES_H_

#include "network.h"
#include <boost/shared_ptr.hpp>
#include "tuple.hpp"
#include "contextTuple.hpp"


#define GO_MAP_MSG 15000
#define BACK_MAP_MSG 15001
#define GEO_TUPLE_MSG 15002


// MAP CONSTRUCTION

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

// GEO MESSAGE
class GeoMessage;
typedef boost::shared_ptr<GeoMessage> GeoMessage_ptr;

class GeoMessage : public Message {
 protected:  
  // routing information (header)
  enum mode_t {GREEDY = 0, PERIMETER};
  Coordinate source;
  Coordinate last;
  Coordinate destination;
  mode_t mode; 
  Coordinate perimeterStart;
  int hops;

  // data information (payload)
  enum data_mode_t {STORE = 0, QUERY, ANSWER};
  ContextTuple tuple;  
  data_mode_t dataMode;
 public :

 GeoMessage(Coordinate s, Coordinate d, ContextTuple &t, data_mode_t dm) : Message(), tuple(t) { 
    type = GEO_TUPLE_MSG;
    source = s;
    last = s;
    destination = d;
    mode = GREEDY;
    perimeterStart = Coordinate();
    hops = 0;
    tuple = t;
    dataMode = dm;
  };

 GeoMessage(Coordinate s, Coordinate d, Coordinate l, mode_t m, Coordinate p, int h, ContextTuple &t, data_mode_t dm) : Message(), tuple(t) { 
    type = GEO_TUPLE_MSG;
    source = s;
    destination = d;
    last = l;
    mode = m;
    perimeterStart = p;
    hops = h;
    tuple = t;
    dataMode = dm;
  };

 GeoMessage(GeoMessage *m) : Message(), tuple(m->tuple) { 
    type = m->type; 
    source = m->source;
    destination = m->destination;
    last = m->last;
    mode = m->mode;
    perimeterStart = m->perimeterStart;
    hops = m->hops;
    tuple = m->tuple;
    dataMode = m->dataMode;
  };
  
  ~GeoMessage() {};

  Coordinate getSource() {return source; };
  Coordinate getDestination() {return destination; };
  Coordinate getLast() {return last;};
  mode_t getMode() {return mode;};
  bool isInPerimeterMode() {return mode == mode_t::PERIMETER;};
  Coordinate getPermieterStart() {return perimeterStart;};
  ContextTuple getContextTuple() {return tuple;};
  data_mode_t getDataMode() {return dataMode;};
  
  uint getHops() { return hops; };
  unsigned int size() { return 17;};
};

#endif // RECONFCATOMS2DMESSAGES_H_
