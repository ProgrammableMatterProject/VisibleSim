/*
 *  neighbor_finding_BlockCode.cpp
 *
 *  Created on: 21 avril 2013
 *      Author: nico
 */

#include <iostream>
#include <sstream>
#include <cassert>
#include <cstdlib>
#include "scheduler.h"
#include "events.h"
#include "coloration_v2_BlockCode.h"

using namespace std;
using namespace SmartBlocks;
using namespace neighbor_finding;
using namespace coloring;

typedef pair<Coordinates, BlockRoutingInfos> RouteMapElement;

//~ const int cSearchedBlock1( 4);
//~ const int cSearchedBlock2( 2);

ColorationV2BlockCode::ColorationV2BlockCode(SmartBlocksBlock *host):SmartBlocksBlockCode(host) {
  cout << "neighbor_finding_BlockCode constructor" << endl;
  scheduler = SmartBlocks::getScheduler();
  smartBlock = (SmartBlocksBlock*)hostBlock;
}

ColorationV2BlockCode::~ColorationV2BlockCode() {
  cout << "neighbor_finding_BlockCode destructor" << endl;
}

void ColorationV2BlockCode::startup() {
  stringstream info;

  info << "  Starting neighbor_finding_BlockCode in block " << hostBlock->blockId;
  scheduler->trace(info.str());

  // Coloring system initialization
  n_color_msg_received_ = 0;
  my_color_ = 0;
  for( int i = 1; i < coloring::cColorQuantity; ++i) {
    allowed_colors_.push_back( i);
  }
  for( int i( 0); i < neighbor_finding::cNeighborQuantity; ++i) {
    ColorQuantityArray array1;
    ColorQuantityArray array2;
    pair<Coordinates, ColorQuantityArray> element1( cDiagonalNeighborsCoordinates[ i], array1);
    pair<Coordinates, ColorQuantityArray> element2( cCoordinatesOfNeighbor[ i], array2);
    color_neighborhood_.insert( element1);
    color_neighborhood_.insert( element2);
  }

  //GRAPHICS
  //~ if( hostBlock->blockId != cSearchedBlock1 && hostBlock->blockId != cSearchedBlock2) {
    //~ display_color_ = 0;
  //~ } else if( hostBlock->blockId == cSearchedBlock1 || hostBlock->blockId == cSearchedBlock2) {
    //~ display_color_ = 1;
  //~ } else if( hostBlock->blockId == cSearchedBlock2) {
    //~ display_color_ = 2;
  //~ }
  smartBlock->setColor( my_color_);
  //

  // Neighbor finding system initialization
  n_round_ = 1;
  neighbor_finding_done_ = false;
  // Filling my routes_map_ with the informations i already know
  // ATTENTION: la valeur BlockRoutingInfos.direction n'est pas censée être accedée pour routes_map_[cMyCoordinates] ou bien si BlockRoutingInfos.route_network_interface == 0
  // Infos about myself:
  BlockRoutingInfos infos( cExists, (P2PNetworkInterface *)0, 0, 0, false);
  routes_map_.insert(RouteMapElement(cMyCoordinates, infos));
  // Infos about my direct neighbors:
  bool i_got_neighbors( false);
  for( int i( North); i <= West; i++) {
    dead_end_neighbors_[ i] = false;
    P2PNetworkInterface *interface( smartBlock->getInterface( NeighborDirection( i)));
    bool connected( interface->connectedInterface);
    if( connected) {
      i_got_neighbors = true;
      BlockRoutingInfos infos( cExists, interface, 1, 0, false);
      routes_map_.insert( RouteMapElement( cCoordinatesOfNeighbor[ i], infos));
    } else {
      BlockRoutingInfos infos( cNotExists, (P2PNetworkInterface *)0, 1, 0, false);
      routes_map_.insert( RouteMapElement( cCoordinatesOfNeighbor[ i], infos));
    }
  }

  //If i have at least one direct neighbors,
  // i'm going to search for my diagonal ones
  if (i_got_neighbors) {
    //Building my diagonal neighbors list
    list<Coordinates> diagonal_neighbors( 1, Coordinates( -1, -1));
    diagonal_neighbors.push_back( Coordinates( -1, 1));
    diagonal_neighbors.push_back( Coordinates( 1, -1));
    diagonal_neighbors.push_back( Coordinates( 1, 1));
    //~ uint64_t time_offset = 10000 + ( rand() % 25 ) * 10000;
    //~ //***Donc là les deux prochain tests c'est juste une petite "optim" pas vitale, mais je pense qu'elle fera gagner pas mal d'échanges de messages
    //~ //*** dans les configurations dense, et puis sinon bin c'est juste 2 tests dans lesquels on n'entre pas
    //~ //If i have two neighbors facing each other, i can find all of my diagonal neighbors in one search message round :
    //~ if( smartBlock->getInterface( SmartBlocks::East )->connectedInterface && smartBlock->getInterface( SmartBlocks::West )->connectedInterface ) {
      //~ //Preparing a message specially for East
//~ cout << "Voisins horizontaux détectés :" << endl;
      //~ list< Coordinates> my_list( 1, Coordinates( 1, -1, Neighbor_finding::East ) );
      //~ my_list.push_back( Coordinates( 1, 1, Neighbor_finding::East ) );
      //~ RouteSearchMsg* msg = new RouteSearchMsg( my_list, cMyCoordinatesFromNeighbor[ Neighbor_finding::East ], 1, n_round );
//~ msg->print();
      //~ scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, msg, smartBlock->getInterface( NeighborDirection( SmartBlocks::East ) ) ) );
      //~ //And one for West
      //~ time_offset += 10000 + ( rand() % 25 ) * 10000;
      //~ list< Coordinates> my_list2( 1, Coordinates( -1, -1, Neighbor_finding::West ) );
      //~ my_list2.push_back( Coordinates( -1, 1, Neighbor_finding::West ) );
      //~ msg = new RouteSearchMsg( my_list, cMyCoordinatesFromNeighbor[ Neighbor_finding::West ], 1, n_round );
//~ msg->print();
      //~ scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, msg, smartBlock->getInterface( NeighborDirection( SmartBlocks::East ) ) ) );
//~
      //~ acks_map_entry->second.n_ack_waited = 2;
    //~ }
    //~ else if( smartBlock->getInterface( SmartBlocks::North )->connectedInterface && smartBlock->getInterface( SmartBlocks::South )->connectedInterface ) {
//~ cout << "Voisins verticaux détectés :" << endl;
      //~ //Preparing a message specially for North
      //~ list< Coordinates> my_list( 1, Coordinates( -1, 1, Neighbor_finding::North ) );
      //~ my_list.push_back( Coordinates( 1, 1, Neighbor_finding::North ) );
      //~ RouteSearchMsg* msg = new RouteSearchMsg( my_list, cMyCoordinatesFromNeighbor[ Neighbor_finding::North ], 1, n_round );
//~ msg->print();
      //~ scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, msg, smartBlock->getInterface( NeighborDirection( SmartBlocks::North ) ) ) );
      //~ //And one for South
      //~ time_offset += 10000 + ( rand() % 25 ) * 10000;
      //~ list< Coordinates> my_list2( 1, Coordinates( -1, -1, Neighbor_finding::South ) );
      //~ my_list2.push_back( Coordinates( 1, -1, Neighbor_finding::South ) );
      //~ msg = new RouteSearchMsg( my_list, cMyCoordinatesFromNeighbor[ Neighbor_finding::South ], 1, n_round );
//~ msg->print();
      //~ scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, msg, smartBlock->getInterface( NeighborDirection( SmartBlocks::South ) ) ) );
//~
      //~ acks_map_entry->second.n_ack_waited = 2;
    //~ }
    //~ //***Fin de la petite optim
    //~ //Else, i may have to do many rounds
    //~ else {
//~ cout << "Pas de voisins face à face détectés :" << endl;
      uint64_t time_offset( 10000 + ( rand() % 25) * 10000);
      map<Coordinates, BlockRoutingInfos>::iterator routes_map_iterator( routes_map_.find( cMyCoordinates));
      for( int i( North); i <= West; i++) {
        bool connected (smartBlock->getInterface( NeighborDirection(i))->connectedInterface);
        if( connected) {
          RouteSearchMsg *msg( new RouteSearchMsg( diagonal_neighbors, cMyCoordinates, 1, n_round_, NeighborDirection( i), hostBlock->blockId));
          scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, msg, smartBlock->getInterface( NeighborDirection( i))));
          time_offset += 10000 + ( rand() % 25) * 10000;
          routes_map_iterator->second.n_ack_waited++;
          routes_map_iterator->second.dead_end = true;
        }
      }
    //~ }
  }
}


void ColorationV2BlockCode::processLocalEvent( EventPtr pev) {
  switch( pev->eventType) {
    case EVENT_NI_RECEIVE: {
      MessagePtr message( std::static_pointer_cast<NetworkInterfaceReceiveEvent>( pev)->message);
      switch( message->type) {
        case cRouteSearchMsgId: {
          RouteSearchMsgPtr recv_search_msg( std::static_pointer_cast<RouteSearchMsg>( message));

          //Affichage
          unsigned int sourceId( recv_search_msg->sourceInterface->hostBlock->blockId);
          stringstream info;
          info.str("");
          info << endl << endl << "Block #" << hostBlock->blockId << " received a RouteSearchMsg from block #" << sourceId << endl;
          //info << "data : " << msg->data();
          //~ SmartBlocks::getScheduler()->trace(info.str());
          //Fin affichage
//GRAPHICS
//V1
//~ if( recv_search_msg->origine_id() == cSearchedBlock1){
  //~ display_color_ = 1;
//~ } else if( recv_search_msg->origine_id() == cSearchedBlock2){
  //~ display_color_ = 2;
//~ }
//FIN V1
//V2
//~ if( display_color_ == 0){
//~ if( recv_search_msg->origine_id() == cSearchedBlock1){
  //~ display_color_ = 1;
//~ } else if( recv_search_msg->origine_id() == cSearchedBlock2){
  //~ display_color_ = 2;
//~ }
//~ } else if( display_color_ == 1){
  //~ if( recv_search_msg->origine_id() == cSearchedBlock2){
    //~ display_color_ = 3;
  //~ }
//~ } else if( display_color_ == 2){
  //~ if( recv_search_msg->origine_id() == cSearchedBlock1){
    //~ display_color_ = 3;
  //~ }
//~ }
//FIN V2
//~ smartBlock->setColor( display_color_);
//~ usleep( 50000);
//
          TreatRouteSearchMsg( recv_search_msg);
          break;
        }
        case cRouteSearchAckId: {
          RouteSearchAckPtr recv_search_ack( std::static_pointer_cast<RouteSearchAck>( message));

          //Affichage
          unsigned int sourceId( recv_search_ack->sourceInterface->hostBlock->blockId);
          stringstream info;
          info.str("");
          info << endl << endl << "Block #" << hostBlock->blockId << " received a RouteSearchAck from block #" << sourceId << endl;
          //info << "data : " << msg->data();
          //~ SmartBlocks::getScheduler()->trace(info.str());
          //Fin affichage

          TreatRouteSearchAck( recv_search_ack);
//GRAPHICS
//~ if( recv_search_ack->origine_id() == cSearchedBlock1){
  //~ display_color_ = 4;
//~ } else if( recv_search_ack->origine_id() == cSearchedBlock2){
  //~ display_color_ = 5;
//~ }
//~ smartBlock->setColor( display_color_);
//~ usleep( 50000);
//
          break;
        }
        case cColoringMsgId:{
          ColoringMsgPtr recv_msg( std::static_pointer_cast<ColoringMsg>( message));

          //Affichage
          unsigned int sourceId( recv_msg->sourceInterface->hostBlock->blockId);
          stringstream info;
          info.str("");
          info << endl << endl << "Block #" << hostBlock->blockId << " received a ColoringMsg from block #" << sourceId;
          //info << "data : " << msg->data();
          SmartBlocks::getScheduler()->trace(info.str());
          //Fin affichage

          TreatColoringMsg( recv_msg);
//GRAPHICS pour voir les messages de recherche de route
//~ if( recv_ack->origine_id() == cSearchedBlock1){
  //~ display_color_ = 4;
//~ } else if( recv_ack->origine_id() == cSearchedBlock2){
  //~ display_color_ = 5;
//~ }
//~ smartBlock->setColor( display_color_);
//~ usleep( 50000);
//
          break;
        }
        case cColoringAckId:{
          ColoringAckPtr recv_ack( std::static_pointer_cast<ColoringAck>( message));

          //Affichage
          unsigned int sourceId( recv_ack->sourceInterface->hostBlock->blockId);
          stringstream info;
          info.str("");
          info << endl << endl << "Block #" << hostBlock->blockId << " received a ColoringAck from block #" << sourceId << endl;
          //info << "data : " << msg->data();
          SmartBlocks::getScheduler()->trace(info.str());
          //Fin affichage

          TreatColoringAck( recv_ack);
//GRAPHICS pour voir les acquittements
//~ if( recv_ack->origine_id() == cSearchedBlock1){
  //~ display_color_ = 4;
//~ } else if( recv_ack->origine_id() == cSearchedBlock2){
  //~ display_color_ = 5;
//~ }
//~ smartBlock->setColor( display_color_);
//~ usleep( 50000);
//
          break;
        }
        default:{
          assert( false);
          break;
        }
      }
    }
  }
}

SmartBlocks::SmartBlocksBlockCode* ColorationV2BlockCode::buildNewBlockCode(
    SmartBlocksBlock *host){
  return( new ColorationV2BlockCode( host));
}

// Processing of a received RouteSearchMsg:
// Two possibilities:
// 1- I will acknowledge for it if:
//   It has done its maximum amount of jumps ||
//   It does not come from the first best route i knew to its origine block.
// 2- I will flood it forward otherwise.
void ColorationV2BlockCode::TreatRouteSearchMsg( RouteSearchMsgPtr recv_msg){
  // Lookup for the concerned block into our routes_map_
  map<Coordinates, BlockRoutingInfos>::iterator routes_map_iterator(
      routes_map_.find( recv_msg->origine()));

  // If we didn't know anything about this block, take the received informations
  if (routes_map_iterator == routes_map_.end()){
    BlockRoutingInfos infos( cExists, recv_msg->destinationInterface,
                             recv_msg->distance(), 0, false);
    routes_map_iterator = routes_map_.insert(
        RouteMapElement( recv_msg->origine(), infos)).first;
  }
  // Case 1: acknowledgement:
  // If i blocked the message because it isn't from the first best route i knew:
  if( recv_msg->distance() > routes_map_iterator->second.distance ||
     ( recv_msg->distance() == routes_map_iterator->second.distance &&
       recv_msg->destinationInterface != routes_map_iterator->second.route_network_interface)) {
    list<BlockAckInfos> ack_infos_list( BuildDistancesList( recv_msg->searched_blocks()));
    // Acknowledge with dead_end_ == true.
    RouteSearchAck *ack( new RouteSearchAck(
        ack_infos_list, recv_msg->origine(), true,
        smartBlock->getDirection( recv_msg->destinationInterface),
        recv_msg->origine_id()));
    uint64_t time_offset( 10000 + ( rand() % 25) * 10000);
    scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent(
        scheduler->now() + time_offset, ack, recv_msg->destinationInterface));
  // If the message have done its amount of jumps
  } else if( recv_msg->n_jumps() <= recv_msg->distance()) {
    list<BlockAckInfos> ack_infos_list(
        BuildDistancesList( recv_msg->searched_blocks()));
    // Acknowledge with dead_end_ == false.
    RouteSearchAck *ack( new RouteSearchAck(
        ack_infos_list, recv_msg->origine(), false,
        smartBlock->getDirection( recv_msg->destinationInterface),
        recv_msg->origine_id()));
    uint64_t time_offset( 10000 + ( rand() % 25) * 10000);
    scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent(
        scheduler->now() + time_offset, ack, recv_msg->destinationInterface));

  // Case 2: forward flooding:
  } else {
    uint64_t time_offset( 10000 + ( rand() % 25) * 10000);
    for( int i( North); i <= West; i++) {
      if( smartBlock->getInterface( NeighborDirection(i))->connectedInterface &&
          smartBlock->getInterface( NeighborDirection(i)) != recv_msg->destinationInterface) {
        RouteSearchMsg *new_msg( new RouteSearchMsg(recv_msg, NeighborDirection(i)));
        scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset,
                                                                     new_msg,
                                                                     smartBlock->getInterface(NeighborDirection(i))));
        time_offset += 10000 + (rand() % 25) * 10000;
        routes_map_iterator->second.n_ack_waited++;
        routes_map_iterator->second.dead_end = true;
      }
    }
    // If i hadn't any neighbor but recv_msg's sender, i'm a dead-end
    if( routes_map_iterator->second.n_ack_waited == 0) {
      list<BlockAckInfos> ack_infos_list( BuildDistancesList(recv_msg->searched_blocks()));
      // Acknowledge with dead_end_ == true.
      RouteSearchAck *ack(new RouteSearchAck(ack_infos_list, recv_msg->origine(),
                                             true,
                                             smartBlock->getDirection(recv_msg->destinationInterface),
                                             recv_msg->origine_id()));
      uint64_t time_offset(10000 + (rand() % 25) * 10000);
      scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, ack, recv_msg->destinationInterface));
    }
  }
}

// Processing of a received RouteSearchAck:
// It carries many route information into its blocks_infos_ member:
// If the information about a block is better than what i already know,
//  i will update my information.
// I will also decrease the number of acknowledgements i'm waiting for this message
// Then if i received all the acknowledgements for this message, there are two cases:
//   If the acknowledgement is for a RouteSearchMsg i flooded forward,
//    i have to answer an acknowledgement on the route to the RouteSearchMsg's origine block.
//   If the acknowledgement is for my own RouteSearchMsg,
//    i have to start another round if i do not know all my diagonal neighbors yet.
void ColorationV2BlockCode::TreatRouteSearchAck( RouteSearchAckPtr recv_ack) {
  // 1- Process route information carried by this acknowledgement
  list<BlockAckInfos> blocks_infos_cpy( recv_ack->blocks_infos());
  for( list<BlockAckInfos>::iterator blocks_infos_iterator( blocks_infos_cpy.begin());
       blocks_infos_iterator != blocks_infos_cpy.end();
       ++blocks_infos_iterator) {
    map<Coordinates, BlockRoutingInfos>::iterator routes_map_iterator( routes_map_.find( blocks_infos_iterator->block));
    // If the information is better than mine, i update mine
    if( routes_map_iterator != routes_map_.end() &&
        blocks_infos_iterator->distance < routes_map_iterator->second.distance) {
      //~ routes_map_iterator->second.status = blocks_infos_iterator->status;
      routes_map_iterator->second.route_network_interface = recv_ack->destinationInterface;
      routes_map_iterator->second.distance = blocks_infos_iterator->distance;
    // If the information is new to me
    } else if( routes_map_iterator == routes_map_.end()) {
      BlockRoutingInfos new_infos( blocks_infos_iterator->status, recv_ack->destinationInterface, blocks_infos_iterator->distance, 0, false);
      routes_map_.insert( RouteMapElement( blocks_infos_iterator->block, new_infos));
    }
    // Else, i ignore the information
  }

  //2- Account for received akcnowledgement
  map<Coordinates, BlockRoutingInfos>::iterator routes_map_iterator( routes_map_.find(recv_ack->origine()));
  --routes_map_iterator->second.n_ack_waited;
  //If any of the blocks on the path said it isn't a dead-end, then it's not^^
  if( !recv_ack->dead_end()) {
    routes_map_iterator->second.dead_end = false;
  }
  // If one of my neighbors acknowledged with dead_end == true, it's useless to send it the next round message
  if( recv_ack->origine() == cMyCoordinates && recv_ack->dead_end() == true) {
    dead_end_neighbors_[ smartBlock->getDirection( recv_ack->destinationInterface)] = true;
  }

  //3- If all the acknowledgements for this message have arrived
  if( routes_map_iterator->second.n_ack_waited == 0) {
    // If the acknowledgement is for a RouteSearchMsg i flooded forward,
    if( routes_map_iterator->first != cMyCoordinates) {
      // I have to answer an acknowledgement on the route to the RouteSearchMsg's origine block.
      // Listing target block's diagonal neighbors:
      Coordinates center_block( routes_map_iterator->first);
      list<Coordinates> diagonal_neighbors( 1, Coordinates( center_block.x - 1, center_block.y - 1));
      diagonal_neighbors.push_back( Coordinates( center_block.x - 1, center_block.y + 1));
      diagonal_neighbors.push_back( Coordinates( center_block.x + 1, center_block.y - 1));
      diagonal_neighbors.push_back( Coordinates( center_block.x + 1, center_block.y + 1));
      // Extracting the data i know about them:
      list<BlockAckInfos> ack_infos_list( BuildDistancesList( diagonal_neighbors));
      RouteSearchAck *new_ack( new RouteSearchAck( ack_infos_list,
                                             routes_map_iterator->first,
                                             routes_map_iterator->second.dead_end,
                                             smartBlock->getDirection( routes_map_iterator->second.route_network_interface),
                                             recv_ack->origine_id()));
      uint64_t time_offset( 10000 + (rand() % 25) * 10000);
      scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, new_ack, routes_map_iterator->second.route_network_interface));
    // If the acknowledgement is for my own RouteSearchMsg,
    } else {
      // I have to start another round if i do not know all my diagonal neighbors yet.
      StartNextRound();
    }
  }
}

// Search for another block's searched_blocks into my routes_map_.
list<BlockAckInfos> ColorationV2BlockCode::BuildDistancesList( list<Coordinates> searched_blocks) {
  list<BlockAckInfos> result;
  for( list<Coordinates>::iterator it( searched_blocks.begin()); it != searched_blocks.end(); ++it) {
    map<Coordinates, BlockRoutingInfos>::iterator routes_map_iterator( routes_map_.find( *it));
    if( routes_map_iterator != routes_map_.end()) {
      BlockAckInfos result_item( *it, routes_map_iterator->second.distance + 1, routes_map_iterator->second.status);
      result.push_back( result_item);
    }
  }
  return result;
}

// List diagonal neighbors that are still missing.
// Then broadcast a RouteSearchMsg, further than the round before
void ColorationV2BlockCode::StartNextRound() {
  ++n_round_;
  // Listing the diagonal neighbor i haven't found yet:
  list<Coordinates> missing_neighbors;
  for( int i( North); i <= West; ++i) {
    if( routes_map_.find( cDiagonalNeighborsCoordinates[ i]) == routes_map_.end()) {
      missing_neighbors.push_back( cDiagonalNeighborsCoordinates[ i]);
    }
  }
  if( !missing_neighbors.empty()) {
    map<Coordinates, BlockRoutingInfos>::iterator routes_map_iterator( routes_map_.find( cMyCoordinates));
    uint64_t time_offset( 10000 + ( rand() % 25) * 10000);
    for( int i( North); i <= West; ++i) {
      // I send next round message only to connected neighbors wich weren't dead-ends in previous round.
      if( !dead_end_neighbors_[ i] && smartBlock->getInterface( NeighborDirection( i))->connectedInterface) {
        /*/!\ Il sera peut être intéressant de faire n_jumps = 2^n_round plutot que n_jumps = n_round */
        RouteSearchMsg *msg( new RouteSearchMsg( missing_neighbors, cMyCoordinates, 1, n_round_, NeighborDirection( i), hostBlock->blockId));
        scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, msg, smartBlock->getInterface( NeighborDirection(i))));
        time_offset += 10000 + ( rand() % 25) * 10000;
        routes_map_iterator->second.n_ack_waited++;
        routes_map_iterator->second.dead_end = true;
      }
    }
    if( routes_map_iterator->second.n_ack_waited == 0) {
      ContinueWhenNeighborsAreFound();
    }
  } else {
    ContinueWhenNeighborsAreFound();
  }
}

// When i'm done finding my neighbor, i start the coloring protocol
void ColorationV2BlockCode::ContinueWhenNeighborsAreFound() {
  neighbor_finding_done_ = true;
  // Adjusting my neighbors map for coloration according to my routes_info_
  for( int i( 0); i < neighbor_finding::cNeighborQuantity; ++i) {
    if( routes_map_.find( cDiagonalNeighborsCoordinates[ i]) == routes_map_.end()
        || routes_map_.find( cDiagonalNeighborsCoordinates[ i])->second.status != cExists) {
      color_neighborhood_.erase( cDiagonalNeighborsCoordinates[ i]);
    }
    if( routes_map_.find( cCoordinatesOfNeighbor[ i]) == routes_map_.end()
        || routes_map_.find( cCoordinatesOfNeighbor[ i])->second.status != cExists) {
      color_neighborhood_.erase( cCoordinatesOfNeighbor[ i]);
    }
  }
  DrawAndSendColorScores();
  // And if i already received scores from all of my neighbors:
  if( n_color_msg_received_ >= color_neighborhood_.size()) {
    // I can try to pick a color.
    ProcessScores();
  }
}

void ColorationV2BlockCode::TreatColoringMsg( ColoringMsgPtr recv_msg) {
  // If the message isn't for me, i have to route it toward its destination
   if( recv_msg->destination() != cMyCoordinates) {
    // I have to transmit it toward its destination.
    P2PNetworkInterface * transmission_interface( routes_map_.find( recv_msg->destination())->second.route_network_interface);
    uint64_t time_offset = 10000 + ( rand() % 25) * 10000;
    ColoringMsg *new_msg( new ColoringMsg( recv_msg, smartBlock->getDirection( transmission_interface)));
    scheduler->schedule(
        new NetworkInterfaceEnqueueOutgoingEvent(
            scheduler->now() + time_offset, new_msg, transmission_interface));
  // If the message is for me:
  } else {
    // Only if i am not yet colored, i am interested into this message:
    if( my_color_ == 0) {
      ++n_color_msg_received_;
      // I have to memorize its scores
      map<Coordinates, ColorQuantityArray>::iterator neighbor_iterator( color_neighborhood_.find( recv_msg->origine()));
      for( int i = 1; i < cColorQuantity; ++i) {
        neighbor_iterator->second[ i] = recv_msg->scores()[ i];
      }
      // And if i received scores from all of my neighbors:
      if( n_color_msg_received_ >= color_neighborhood_.size() && neighbor_finding_done_) {
        // I can try to pick a color.
        ProcessScores();
      }
    }
  }
}

void ColorationV2BlockCode::TreatColoringAck( ColoringAckPtr recv_ack) {
  // If the acknowledgement isn't for me:
  if( recv_ack->destination() != cMyCoordinates) {
    // I have to transmit it toward its destination.
    P2PNetworkInterface * transmission_interface( routes_map_.find( recv_ack->destination())->second.route_network_interface);
    uint64_t time_offset = 10000 + ( rand() % 25) * 10000;
    ColoringAck *new_ack( new ColoringAck( recv_ack, smartBlock->getDirection( transmission_interface)));
    scheduler->schedule(
        new NetworkInterfaceEnqueueOutgoingEvent(
            scheduler->now() + time_offset, new_ack, transmission_interface));
  // If the acknowledgement is for me:
  } else {
    // Only if i am not yet colored, i am interested into this acknowledgment:
    if( my_color_ == 0) {
      // I have to remove the chosen color from my allowed colors.
      list<int>::iterator allowed_colors_iterator( allowed_colors_.begin());
      while( *allowed_colors_iterator != recv_ack->color()
             && allowed_colors_iterator != allowed_colors_.end()) {
        ++allowed_colors_iterator;
      }
      if( allowed_colors_iterator != allowed_colors_.end()) {
        allowed_colors_.erase( allowed_colors_iterator);
      }
      // And remove the block from my coloring neighbors.
      color_neighborhood_.erase( recv_ack->origine());
      // Then if i already have the scores from all of my remaining neighbors:
      if( n_color_msg_received_ == color_neighborhood_.size() && neighbor_finding_done_) {
        // I can try to pick a color.
        ProcessScores();
      }
    }
  }
}

// Choose one score for each allowed color
void ColorationV2BlockCode::DrawAndSendColorScores() {
  // For each of my allowed colors, i randomize a score.
  list<int>::iterator it( allowed_colors_.begin());
  for( int i( 1); i < cColorQuantity; ++i) {
    if( i == *it) {
      my_scores_[ i] = 1 + (rand() % (coloring::cNeighborsQuantity + 1));
      ++it;
    } else {
      my_scores_[ i] = 0;
    }
  }

  // Flood it to all my not-yet-colored neighbors
  uint64_t time_offset = 10000 + ( rand() % 25) * 10000;
  for( map<Coordinates, ColorQuantityArray>::iterator neighbor_iterator( color_neighborhood_.begin());
       neighbor_iterator != color_neighborhood_.end();
       ++neighbor_iterator) {
    Coordinates tmpDestination( neighbor_iterator->first);
    P2PNetworkInterface * tmpInterface( routes_map_.find( neighbor_iterator->first)->second.route_network_interface);
    NeighborDirection tmpDirection( smartBlock->getDirection( tmpInterface));
    ColoringMsg *msg( new ColoringMsg( cMyCoordinates, tmpDestination, my_scores_, tmpDirection));
    scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, msg, tmpInterface));
    time_offset += 10000 + ( rand() % 25) * 10000;
  }
}

void ColorationV2BlockCode::ProcessScores() {
  n_color_msg_received_ = 0;
  // If i have any uncolored neighbor left:
  if( !color_neighborhood_.empty()) {
    // For each of my allowed colors
    for( list<int>::iterator color_iterator( allowed_colors_.begin());
              color_iterator != allowed_colors_.end(); ++color_iterator) {
      // I compute the maximum score chosen by my neighbors for this color:
      map<Coordinates, ColorQuantityArray>::iterator neighbor_iterator(
          color_neighborhood_.begin());
      int max_neighbor_score( neighbor_iterator->second[ *color_iterator]);
      while( neighbor_iterator != color_neighborhood_.end()) {
        if( neighbor_iterator->second[ *color_iterator] > max_neighbor_score) {
          max_neighbor_score = neighbor_iterator->second[ *color_iterator];
        }
        ++neighbor_iterator;
      }
      // If i chose the best score then i can take this color.
      if( max_neighbor_score < my_scores_[ *color_iterator]) {
        my_color_ = *color_iterator;
        smartBlock->setColor( my_color_);
        usleep( 50000);
        // I tell it to all my neighbors:
        uint64_t time_offset = 10000 + ( rand() % 25) * 10000;
        for( map<Coordinates, ColorQuantityArray>::iterator neighbor_iterator( color_neighborhood_.begin());
             neighbor_iterator != color_neighborhood_.end();
             ++neighbor_iterator) {
          Coordinates tmpDestination( neighbor_iterator->first);
          P2PNetworkInterface * tmpInterface( routes_map_.find( neighbor_iterator->first)->second.route_network_interface);
          NeighborDirection tmpDirection( smartBlock->getDirection( tmpInterface));
          ColoringAck *ack( new ColoringAck( cMyCoordinates, tmpDestination, my_color_, tmpDirection));
          scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, ack, tmpInterface));
          time_offset += 10000 + ( rand() % 25) * 10000;
        }
        return;
      }
    }
    // If i haven't found any color wich i can take, i have to retry !
    DrawAndSendColorScores();
  // If i have no neighbors left:
  }
  else {
    // I'll just take one of my allowed colors
    my_color_ = allowed_colors_.front();
  smartBlock->setColor( my_color_);
    usleep( 50000);
  }
}

void ColorationV2BlockCode::PrintRoutesMap() {
  cout << "block #" << hostBlock->blockId << " routes_map_ :" << endl;
  for( map< Coordinates, BlockRoutingInfos>::iterator i = routes_map_.begin(); i != routes_map_.end(); ++i) {
    cout << "\tBlock(" << i->first.x << ", " << i->first.y << "): "
    "status = " << i->second.status << ", direction = " << smartBlock->getDirection(i->second.route_network_interface) << ","
    " distance = " << i->second.distance << ", n_ack_waited = " << i->second.n_ack_waited << endl;
  }
  cout << endl;
}
