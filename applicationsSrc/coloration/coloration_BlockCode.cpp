/*
 * coloration_BlockCode.cpp
 *
 *  Created on: 21 avril 2013
 *      Author: nico
 */

#include <iostream>
#include <sstream>
#include <cstdlib>
#include "scheduler.h"
#include "events.h"
#include "coloration_BlockCode.h"
#include "coloration_messages.h"


using namespace std;
using namespace SmartBlocks;

Coloration_BlockCode::Coloration_BlockCode(SmartBlocksBlock *host):SmartBlocksBlockCode(host) {
	cout << "coloration_BlockCode constructor" << endl;
	scheduler = getScheduler();
	smartBlock = (SmartBlocksBlock*)hostBlock;
}

Coloration_BlockCode::~Coloration_BlockCode() {
	cout << "coloration_BlockCode destructor" << endl;
}

void Coloration_BlockCode::startup() {
	stringstream info;

	info << "  Starting coloration_BlockCode in block " << hostBlock->blockId;
	scheduler->trace(info.str(),hostBlock->blockId);

	colored = false;
	smartBlock->setColor(5);

	//If i am the master block
	if( hostBlock->blockId == 1 ) {
		my_color                         = (color)( rand() % n_different_color);
		color vertical_neighbors_color   = (color)(( my_color + 1 ) % n_different_color);
		color horizontal_neighbors_color = (color)(( my_color + 2 ) % n_different_color);
		colored = true;
OUTPUT << "Block #" << hostBlock->blockId << " I am " << color_to_string( my_color ) << endl;
		smartBlock->setColor(my_color);
		sleep(1);

		uint64_t time_offset = 0;
		P2PNetworkInterface * p2p;
		for( int i = North ; i <= West ; i++ ) {
			p2p = smartBlock->getInterface( (NeighborDirection)i );
			if( p2p->connectedInterface ) {
				time_offset += 10000 + (rand() % 25) * 2000;
				Color_message * color_msg = new Color_message( my_color, vertical_neighbors_color, horizontal_neighbors_color );
				scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, color_msg, p2p ));
			}
		}
	}
}

void Coloration_BlockCode::processLocalEvent( EventPtr pev ) {
	unsigned int sourceId;
	MessagePtr message;
	stringstream info;

	if( pev->eventType == EVENT_NI_RECEIVE) {
		message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
		Color_message_ptr recv_message = std::static_pointer_cast<Color_message>(message);

		//Affichage
		sourceId = recv_message->sourceInterface->hostBlock->blockId;
		info.str("");
		info << "received a Color_message (" << recv_message->get_my_color() << "," << recv_message->get_horizontal_color() << "," << recv_message->get_vertical_color() << ") from " << sourceId;
		//info << "data : " << msg->data();
		getScheduler()->trace(info.str(),hostBlock->blockId);

		if( !colored ) {
			colored = true;

			int i;
			uint64_t time_offset = 0;
			P2PNetworkInterface * p2p;

			color vertical_neighbors_color;
			color horizontal_neighbors_color;

			if( recv_message->destinationInterface == smartBlock->getInterface( North ) || recv_message->destinationInterface == smartBlock->getInterface( South )) {
				info.str("");
				info << "I am " << color_to_string(my_color) << endl;
				vertical_neighbors_color = recv_message->get_my_color();

				//Searching for the vacant color
				for( i = 0 ; i < n_different_color ; i++ ) {
					if( (color)i != my_color && (color)i != vertical_neighbors_color && (color)i != recv_message->get_horizontal_color() ) {
						horizontal_neighbors_color = (color)i;
					}
				}

				//Informing my neighbors
				for( i = North ; i <= West ; i++ ) {
				p2p = smartBlock->getInterface( (NeighborDirection)i );
					if( p2p->connectedInterface ) {
						if( p2p != recv_message->destinationInterface ) {
							time_offset += 10000 + (rand() % 25) * 2000;
							Color_message * color_msg = new Color_message( my_color, vertical_neighbors_color, horizontal_neighbors_color );
							scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, color_msg, p2p ));
						}
					}
				}
			}
			else if( recv_message->destinationInterface == smartBlock->getInterface( East ) || recv_message->destinationInterface == smartBlock->getInterface( West )) {
				my_color = recv_message->get_horizontal_color();
OUTPUT << "Block #" << hostBlock->blockId << " I am " << color_to_string( my_color );
				horizontal_neighbors_color = recv_message->get_my_color();

				//Searching for the vacant color
				for( i = 0 ; i < n_different_color ; i++ ) {
					if( (color)i != my_color && (color)i != horizontal_neighbors_color && (color)i != recv_message->get_vertical_color() ) {
						vertical_neighbors_color = (color)i;
					}
				}
				//Informing my neighbors
				for( i = North ; i <= West ; i++ ) {
				p2p = smartBlock->getInterface( (NeighborDirection)i );
					if( p2p->connectedInterface ) {
						if( p2p != recv_message->destinationInterface ) {
							time_offset += 10000 + (rand() % 25) * 2000;
							Color_message * color_msg = new Color_message( my_color, vertical_neighbors_color, horizontal_neighbors_color );
							scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, color_msg, p2p ));
						}
					}
				}
			}
			smartBlock->setColor(my_color);
			sleep(1);
		}
	}
}

SmartBlocks::SmartBlocksBlockCode* Coloration_BlockCode::buildNewBlockCode(SmartBlocksBlock *host) {
	return(new Coloration_BlockCode(host));
}
