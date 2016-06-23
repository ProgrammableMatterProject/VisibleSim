/*
 * blinky02BlockCode.cpp
 *
 *  Created on: 06 juin 2013
 *      Author: ben
 */

#include <iostream>
#include <sstream>
#include "blinky02BlockCode.h"
#include "scheduler.h"
#include "events.h"
#include <boost/shared_ptr.hpp>


using namespace std;
using namespace BlinkyBlocks;

Blinky02BlockCode::Blinky02BlockCode(BlinkyBlocksBlock *host):BlinkyBlocksBlockCode(host) {
	cout << "Blinky02BlockCode constructor" << endl;
	scheduler = getScheduler();
	block = (BlinkyBlocksBlock*)hostBlock;
}

Blinky02BlockCode::~Blinky02BlockCode() {
	cout << "Blinky02BlockCode destructor" << endl;
}

void Blinky02BlockCode::startup() {
	stringstream info;

	info << "  Starting Blinky02BlockCode in block " << hostBlock->blockId;
	scheduler->trace(info.str());
	block->setColor(block->blockId);
	distance_dealer = NULL;
	my_distance = MAX_DIST;

	//If i am master block
	if( hostBlock->blockId == 1)
	{
		#ifndef NDEBUG
		cout << "Block #" << hostBlock->blockId << " : I am a master block !" << endl;
		#endif

		my_distance = 0;

		//I send distance to all of my neighbors
		uint64_t time_offset;
		for( int i=0; i<NEIGHBORS_NUMBER; i++)
		{
			P2PNetworkInterface *p2p = block->getInterface(NeighborDirection(i));
			if( p2p->connectedInterface)
			{	time_offset = (i+1)*10000;
				send_dist( my_distance + 1, p2p, time_offset);
				isAck[i] = false;
			}
			else
				isAck[i] = true;
		}
	}
}

void Blinky02BlockCode::processLocalEvent(EventPtr pev) {
	int i;
	uint64_t time_offset;
	unsigned int sourceId;
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
		case EVENT_NI_RECEIVE:
			message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
			P2PNetworkInterface * recv_interface = message->destinationInterface;

			switch( message->id) {
				//If i receive a distance message :
				case DIST_MSG_ID :
				{
					//TODO completement experimental, je ne sais pas précisement les effets de cette ligne
					Dist_message_ptr recv_message = std::static_pointer_cast<Dist_message>(message);

					sourceId = recv_message->sourceInterface->hostBlock->blockId;
					info.str("");
					info << "Block " << hostBlock->blockId << " received a Dist_message '" << recv_message->getDistance() << "' from " << sourceId << endl;
					//info << "data : " << msg->data();
					scheduler->trace(info.str());

					//I compare its distance with mine
					if( recv_message->getDistance() < my_distance)
					{
						my_distance = recv_message->getDistance();
						block->setColor(my_distance);

						distance_dealer = recv_message->destinationInterface;

						//I send my new distance to all my neighbors except to its sender
						for( i = 0; i < NEIGHBORS_NUMBER; i++)
						{
							P2PNetworkInterface * p2p = block->getInterface( NeighborDirection(i));
							if( p2p->connectedInterface) {
								//except to the sender
								if( p2p != recv_interface) {
									time_offset = (i+1)*1000000;
									send_dist( my_distance + 1, p2p, time_offset);

									isAck[i] = false;
								}
								//I'm not waiting for an Ack from the sender of the distance
								else { isAck[i] = true;	}
							}
							//From where there is no neighbor, i'm not waiting for an Ack
							else { isAck[i] = true; }
						}
						//Now, i check if i can already acknowledge my new distance
						if( i_can_ack()){ send_ack( my_distance, recv_interface, 1000); }
					}
					//If my distance was better than the one i received, i ack
					else{	send_ack( my_distance, recv_interface, 1000);	}
					break;
				}

				case ACK_MSG_ID :
				{
					//TODO completement experimental, je ne sais pas précisement les effets de cette ligne
					Ack_message_ptr recv_message = std::static_pointer_cast<Ack_message>(message);

					//~ std::shared_ptr<Ack_message> recv_message = std::static_pointer_cast<Ack_message>(message);

					sourceId = message->sourceInterface->hostBlock->blockId;
					info.str("");
					info << "Block " << hostBlock->blockId << " received a Ack_message '" << recv_message->getDistance() << "' from " << sourceId << endl;

					//info << "data : " << msg->data();
					scheduler->trace(info.str());

					//I want to be sure that my neighbors is acknowledging the last distance i sent
					if( recv_message->getDistance() < my_distance + 2)
					{
						//Searching for which neighbor answered
						isAck[block->getDirection(recv_interface)] = true;

						//If all of my neighbors ackowledged
						if( i_can_ack()){
							//If i am master block, flood is over
							if( hostBlock->blockId == 1) {
								cout << "Flood done !" << endl;
								block->setColor(my_distance);

							}
							//else i have to acknowledge
							else{	send_ack( my_distance, distance_dealer, 1000); }
						}
					}
					break;
				}

				default :
					sourceId = message->sourceInterface->hostBlock->blockId;
					cerr << "Block " << hostBlock->blockId << " received an unrecognized message from " << sourceId << endl;
				break;
			}
		break;
	}
}

BlinkyBlocks::BlinkyBlocksBlockCode* Blinky02BlockCode::buildNewBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host) {
	return(new Blinky02BlockCode(host));
}


void Blinky02BlockCode::send_dist( unsigned int distance,  P2PNetworkInterface * by_interface, uint64_t time_offset) {
	Dist_message * message = new Dist_message( distance);
	scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, message, by_interface));
}

void Blinky02BlockCode::send_ack( unsigned int distance,  P2PNetworkInterface * by_interface, uint64_t time_offset) {
	Ack_message * ack = new Ack_message( distance);
	scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, ack, by_interface));
}


bool Blinky02BlockCode::i_can_ack(){
	bool result = true;
	int i = 0;
	while( i < NEIGHBORS_NUMBER && result == true)
	{
		 if( isAck[ i] == false){ result = false; }
		 i++;
	}
	return result;
}


Dist_message::Dist_message(unsigned int d):Message(){
	id = DIST_MSG_ID;
	distance = d;
}

Dist_message::~Dist_message() {
}

Ack_message::Ack_message(unsigned int d):Message() {
	id = ACK_MSG_ID;
	distance = d;
}

Ack_message::~Ack_message() {
}

