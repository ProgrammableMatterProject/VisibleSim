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

#define GEO_TUPLE_MSG 15002

// GEO MESSAGE
class GeoMessage;
typedef boost::shared_ptr<GeoMessage> GeoMessage_ptr;

class GeoMessage : public Message {
 public:
    enum data_mode_t {STORE = 0, QUERY, ANSWER};
    enum mode_t {GREEDY = 0, PERIMETER};
 protected:  
  // routing information (header)
  Coordinate source;
  Coordinate last;
  Coordinate destination;
  mode_t mode;
  Coordinate perimeterStart;
  int firstEdge;
  int hops;

  // data information (payload)
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
    firstEdge = 0;
  };

 GeoMessage(Coordinate s, Coordinate d, Coordinate l, mode_t m, Coordinate p, int h, ContextTuple &t, data_mode_t dm, int e0) : Message(), tuple(t) { 
    type = GEO_TUPLE_MSG;
    source = s;
    destination = d;
    last = l;
    mode = m;
    perimeterStart = p;
    hops = h;
    tuple = t;
    dataMode = dm;
    firstEdge = e0;
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
    firstEdge = m->firstEdge;
  };
  
  ~GeoMessage() {};

  Coordinate getSource() {return source; };
  Coordinate getDestination() {return destination; };
  Coordinate getLast() {return last;};
  
  mode_t getMode() {return mode;};
  bool isInPerimeterMode() {return mode == mode_t::PERIMETER;};
  Coordinate getPerimeterStart() {return perimeterStart;};
  void setGreedyMode() {mode = mode_t::GREEDY;};
  void setPerimeterMode(Coordinate p) {mode = mode_t::PERIMETER; perimeterStart = p;}
  
  void setFirstEdge(int e0) {firstEdge = e0;}
  int getFirstEdge() {return firstEdge;}

  ContextTuple getTuple() {return tuple;};
  data_mode_t getDataMode() {return dataMode;};
  uint getHops() {return hops;};
  unsigned int size() {return 17;};
};

#endif // RECONFCATOMS2DMESSAGES_H_
