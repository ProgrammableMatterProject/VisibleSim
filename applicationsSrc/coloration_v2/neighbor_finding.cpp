#include "neighbor_finding.h"
#include <iostream>
#include <cassert>

using namespace std;
using namespace neighbor_finding;

//////////////////////////
// class RouteSearchMsg //
//////////////////////////
RouteSearchMsg::RouteSearchMsg(
    list<Coordinates> searched_blocks_list,
    Coordinates origine,
    unsigned int distance,
    unsigned int n_jumps,
    NeighborDirection direction,
    int origine_id)
    : searched_blocks_( searched_blocks_list),
      origine_( origine),
      distance_( distance),
      n_jumps_( n_jumps) {
  // GRAPHICS
  origine_id_ = origine_id;
  //
  type = cRouteSearchMsgId;
  for( list<Coordinates>::iterator iter = searched_blocks_.begin(); iter != searched_blocks_.end(); ++iter) {
      iter->AdaptToBeSentBy( direction);
  }
  origine_.AdaptToBeSentBy( direction);
}

RouteSearchMsg::RouteSearchMsg( const RouteSearchMsgPtr message, NeighborDirection direction)
    : searched_blocks_( message->searched_blocks_),
      origine_( message->origine_),
      distance_( message->distance_ + 1),
      n_jumps_( message->n_jumps_) {
  // GRAPHICS
  origine_id_ = message->origine_id_;
  //
  type = cRouteSearchMsgId;
  for( list<Coordinates>::iterator iter(searched_blocks_.begin()); iter != searched_blocks_.end(); ++iter) {
    iter->AdaptToBeSentBy( direction);
  }
  origine_.AdaptToBeSentBy( direction);
}

void RouteSearchMsg::print() {
  cout << "RouteSearchMsg from ( " << origine_.x << ", " << origine_.y << ") :" << std::endl;
  cout << "distance = " << distance_ << std::endl;
  cout << "n_jumps = " << n_jumps_ << std::endl;
  cout << "searched_blocks = { ";
  for( list<Coordinates>::iterator i = searched_blocks_.begin(); i != searched_blocks_.end(); ++i) {
    cout<< "( " << i->x << ", " << i->y << "), ";
  }
  std::cout << std::endl;
}

//////////////////////////
// class RouteSearchAck //
//////////////////////////
RouteSearchAck::RouteSearchAck( list<BlockAckInfos> blocks_d,
                                Coordinates origine, bool dead_end,
                                NeighborDirection direction,
                                int origine_id)
    : blocks_infos_( blocks_d),
      origine_( origine),
      dead_end_( dead_end) {
  //GRAPHICS
  origine_id_ = origine_id;
  //
  type = cRouteSearchAckId;
  for( list<BlockAckInfos>::iterator iter( blocks_infos_.begin()); iter != blocks_infos_.end(); ++iter) {
    iter->block.AdaptToBeSentBy( direction);
  }
  origine_.AdaptToBeSentBy( direction);
}

void RouteSearchAck::RouteSearchAck::print() {
  cout << "RouteSearchAck from ( " << origine_.x << ", " << origine_.y << ") :" << endl;
  cout << "dead_end = " << dead_end_ << endl;
  cout << "blocks_infos = { ";
  for( std::list<BlockAckInfos>::iterator i = blocks_infos_.begin(); i != blocks_infos_.end(); ++i) {
    cout<< "( " << i->block.x << ", " << i->block.y << ") : distance =  " << i->distance << ", status = " << i->status <<  "; ";
  }
  cout << std::endl;
}

//////////////////////////////
// struct BlockRoutingInfos //
//////////////////////////////
BlockRoutingInfos::BlockRoutingInfos( BlockStatus arg_status,
                                      P2PNetworkInterface * arg_route_interface,
                                      unsigned int arg_distance,
                                      unsigned int arg_n_ack_waited,
                                      bool arg_dead_end)
    : status( arg_status),
      route_network_interface( arg_route_interface),
      distance( arg_distance),
      n_ack_waited( arg_n_ack_waited),
      dead_end( arg_dead_end) {}

//////////////////////////
// struct BlockAckInfos //
//////////////////////////
BlockAckInfos::BlockAckInfos( Coordinates arg_coordinates,
                              unsigned int arg_distance, BlockStatus arg_status)
    : block( arg_coordinates), distance( arg_distance), status( arg_status) {}
