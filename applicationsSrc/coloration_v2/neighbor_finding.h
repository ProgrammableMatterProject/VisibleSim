#ifndef NEIGHBOR_FINDING_H_
#define NEIGHBOR_FINDING_H_

#include <list>
#include <memory>
#include "network.h"
#include "coordinates.h"
#include "smartBlocksBlock.h"

namespace neighbor_finding {

using namespace SmartBlocks;

class RouteSearchMsg;
class RouteSearchAck;

typedef std::shared_ptr<RouteSearchMsg> RouteSearchMsgPtr;
typedef std::shared_ptr<RouteSearchAck> RouteSearchAckPtr;

const int cNeighborQuantity( 4);

const unsigned int cRouteSearchMsgId( 1);
const unsigned int cRouteSearchAckId( 2);

enum BlockStatus {
  cUnknown = 0,
  cNotExists,
  cExists,
};

//Used to initialize ColorationV2BlockCode.routes_map_, and,
//when a block sends its own RouteSearchMsg,
//RouteSearchMsg.origine_ and RouteSearchMsg.searched_blocks
const Coordinates cMyCoordinates( 0, 0);
const Coordinates cCoordinatesOfNeighbor[ cNeighborQuantity] = {
    //My SmartBlocks::North neighbor
    Coordinates(  0,  1),
    //East
    Coordinates(  1,  0),
    //South
    Coordinates(  0, -1),
    //West
    Coordinates( -1,  0)};
const Coordinates cDiagonalNeighborsCoordinates[ cNeighborQuantity] = {
    Coordinates( -1, -1),
    Coordinates( -1,  1),
    Coordinates(  1, -1),
    Coordinates(  1,  1)};
//~ const Coordinates cMyCoordinatesFromNeighbor[cNeighborQuantity] = {
    //~ //Me from my SmartBlocks::North neighbor
    //~ Coordinates(0, -1),
    //~ Coordinates(-1, 0),
    //~ Coordinates(0, 1),
    //~ Coordinates(1, 0)};

// Summarizes everything one block could need to know about another one
// Meant to be be mapped to a block
struct BlockRoutingInfos {
  BlockStatus status;
  // Direction and length of the best route to the block
  P2PNetworkInterface *route_network_interface;
  unsigned int distance;
  // If i transmitted a RouteSearchMsg from the block, i'm waiting for acks
  unsigned int n_ack_waited;
  // If i received one non-final acknowledgement for this block,
  // my own acknowledgement about it has to be non-final
  bool dead_end;
  BlockRoutingInfos( BlockStatus, P2PNetworkInterface *, unsigned int,
                     unsigned int, bool);
};

struct BlockAckInfos {
  Coordinates block;
  unsigned int distance;
  BlockStatus status;

  BlockAckInfos( Coordinates, unsigned int, BlockStatus);
};

class RouteSearchMsg : public Message {
  std::list<Coordinates> searched_blocks_;
  Coordinates origine_;
  unsigned int distance_;
  unsigned int n_jumps_;
  //GRAPHICS
  int origine_id_;
  //
public:
  RouteSearchMsg( std::list<Coordinates>,
                  Coordinates, unsigned int, unsigned int, NeighborDirection::Direction,
                  int origine_id);
  RouteSearchMsg( const RouteSearchMsgPtr, NeighborDirection::Direction);
  ~RouteSearchMsg() {}

  std::list<Coordinates> searched_blocks() { return searched_blocks_; }
  Coordinates origine() { return origine_; }
  unsigned int distance() { return distance_; }
  unsigned int n_jumps() { return n_jumps_; }
  //GRAPHICS
  int origine_id() { return origine_id_; }
  //
  void print();
};

class RouteSearchAck : public Message {
  std::list<BlockAckInfos> blocks_infos_;
  Coordinates origine_;
  bool dead_end_;
  int origine_id_;
public:
  RouteSearchAck( std::list<BlockAckInfos>, Coordinates, bool,
                  NeighborDirection::Direction, int);
  ~RouteSearchAck() {}

  std::list<BlockAckInfos> blocks_infos() { return blocks_infos_; }
  Coordinates origine() { return origine_; }
  bool dead_end() { return dead_end_; }
  int origine_id() { return origine_id_; }

  void print();
};
}
#endif //NEIGHBOR_FINDING_H_
