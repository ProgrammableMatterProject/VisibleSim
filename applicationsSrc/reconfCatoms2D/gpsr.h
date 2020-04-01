#ifndef GPRS_HPP
#define GPRS_HPP

#include "robots/catoms2D/catoms2DBlock.h"
#include "comm/network.h"
#include "map.h"
#include "angle.h"

#define GPSR_PACKET 15002

// GEO MESSAGE
class GPSRPacket;
typedef std::shared_ptr<GPSRPacket> GPSRPacket_ptr;

class GPSR {
 private:
  void send(GPSRPacket_ptr m, P2PNetworkInterface *p2p);
  void send(GPSRPacket *m, P2PNetworkInterface *p2p);

  Catoms2D::Catoms2DBlock *catom2D;
  Map &map;
  Angle angle;
 public:

  GPSR(Catoms2D::Catoms2DBlock *host, Map &m);
  GPSR(GPSR const &g);
  ~GPSR();
  void send(Coordinate s, Coordinate d, Message *msg);
  MessagePtr handleGPSRPacket(MessagePtr m);
};

class GPSRPacket : public Message {
 public:
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
  MessagePtr data;
 public :
 GPSRPacket(Coordinate s, Coordinate d, Message *da) : Message() {
    type = GPSR_PACKET;
    source = s;
    last = s;
    destination = d;
    mode = GREEDY;
    perimeterStart = Coordinate();
    hops = 0;
    firstEdge = 0;
    data = MessagePtr(da);
  };

    GPSRPacket(GPSRPacket *m) : Message() {
    type = m->type;
    source = m->source;
    destination = m->destination;
    last = m->last;
    mode = m->mode;
    perimeterStart = m->perimeterStart;
    hops = m->hops;
    data = m->data;
    firstEdge = m->firstEdge;
  };

  ~GPSRPacket() {
    data.reset();
  };

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

  uint getHops() {return hops;};
  unsigned int size() {return 17;};
  MessagePtr getData() {return data;};
};

#endif
