/*
 * leader_election_BlockCode.cpp
 *
 *  Created on: 20 avril 2013
 *      Author: nico
 */

#include <iostream>
#include <sstream>
#include <cstdlib>
#include "scheduler.h"
#include "events.h"
#include "leader_election_BlockCode.h"
#include "leader_election_messages.h"


using namespace std;
using namespace SmartBlocks;

#define TIMESCALE 10000

Leader_election_BlockCode::Leader_election_BlockCode(SmartBlocksBlock *host):SmartBlocksBlockCode(host) {
	cout << "leader_election_BlockCode constructor" << endl;

	scheduler = getScheduler();
	smartBlock = (SmartBlocksBlock*)hostBlock;
}

Leader_election_BlockCode::~Leader_election_BlockCode() {
	//cout << "leader_election_BlockCode destructor" << endl;
}

void Leader_election_BlockCode::startup() {
	stringstream info;

	info << "  Starting leader_election_BlockCode in block " << hostBlock->blockId;
	scheduler->trace(info.str());

	min_id           = hostBlock->blockId;
	min_id_sender    = NULL;
	i_am_master      = false;
	master_found     = false;
	n_answers_needed = 0;
	smartBlock->setColor(min_id);

	//I send my id to all of my neighbors
	int i;
	Time time_offset = 0;
	P2PNetworkInterface *p2p;
	for( i = NeighborDirection::North; i <= NeighborDirection::West; i++ )
	{
		p2p = smartBlock->getInterface( NeighborDirection::Direction(i) );
		if( p2p->connectedInterface ) {
			n_answers_needed++;
			time_offset += TIMESCALE + ( rand() % 25 ) * TIMESCALE ;
			Id_out_message * message = new Id_out_message( min_id );
			scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, message, p2p ));
		}
	}
}


void Leader_election_BlockCode::processLocalEvent(EventPtr pev) {
	unsigned int sourceId;
	MessagePtr message;
	stringstream info;
	Time time_offset = 0;

	switch (pev->eventType) {
		case EVENT_NI_RECEIVE:
			message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;

			switch( message->type) {
				case ID_OUT_MSG_ID : {
					if( !master_found ) {
						Id_out_message_ptr recv_message = std::static_pointer_cast<Id_out_message>(message);

						//Affichage
						sourceId = recv_message->sourceInterface->hostBlock->blockId;
						info.str("");
						info << "Block " << hostBlock->blockId << " (" << min_id << ")" << " received a Id_message '" << recv_message->get_transmitted_id() << "' from " << sourceId;
						//info << "data : " << msg->data();
						getScheduler()->trace(info.str());
						//

						//If i already knew this min_id, it means that i already flooded it, i only acknowledge
						if( recv_message->get_transmitted_id() == min_id ) {
							time_offset = TIMESCALE + (rand() % 25) * TIMESCALE ;
							Id_ack_message * ack = new Id_ack_message( min_id );
							scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, ack, recv_message->destinationInterface));
						}
						else {
							//If the min_id i knew was bigger than the one i just received
							if( recv_message->get_transmitted_id() < min_id ) {
								//I accept the new one
								min_id = recv_message->get_transmitted_id();
								min_id_sender = recv_message->destinationInterface;
								smartBlock->setColor(min_id);
								//sleep(1);

								//and i flood it forward
								n_answers_needed = 0;
								int i;
								P2PNetworkInterface *p2p;
								for( i = NeighborDirection::North; i <= NeighborDirection::West; i++ ) {
									p2p = smartBlock->getInterface( NeighborDirection::Direction(i) );
									if( p2p->connectedInterface ) {
										if( p2p != min_id_sender ) {
											n_answers_needed++;
											time_offset += TIMESCALE + ( rand() % 25 ) * TIMESCALE ;
											Id_out_message * message = new Id_out_message( min_id );
											scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, message, p2p ));
										}
									}
								}
								//If i didn't have any neighbors but min_id_sender
								if( n_answers_needed ==	0 ) {
									//I can assure that no block after me will contest this min_id ( as there isn't any block )
									time_offset += TIMESCALE + ( rand() % 25 ) * TIMESCALE ;
									Id_ack_message * ack = new Id_ack_message( min_id );
									scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, ack, min_id_sender ));
								}
							}
						}
					}
				break;
				}

				case ID_ACK_MSG_ID : {
					if( !master_found ) {
						Id_ack_message_ptr recv_message = std::static_pointer_cast<Id_ack_message>(message);

						//Affichage
						sourceId = message->sourceInterface->hostBlock->blockId;
						info.str("");
						info << "Block #" << hostBlock->blockId << " (" << min_id << ")" << " received a Ack_message '" << recv_message->get_transmitted_id() << "' from " << sourceId;
						//info << "data : " << msg->data();
						getScheduler()->trace(info.str());
						//

						//If my neighbor already knew the min_id i sent to him
						if( recv_message->get_transmitted_id() == min_id ) {
							n_answers_needed--;
							//If all my neighbors already knew this min_id
							if( n_answers_needed == 0 ) {
								if( min_id == hostBlock->blockId ) {
									master_routine();
								}
								else {
									//I can assure that no block after me will contest this min_id
									//( because my neighbors already flooded it so THEY will say if some block contests it )
									time_offset = TIMESCALE + ( rand() % 25 ) * TIMESCALE ;
									Id_ack_message * ack = new Id_ack_message( min_id );
									scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, ack, min_id_sender ));
								}
							}
						}
					}
				break;
				}
				//Inutilisé
				//~ case MASTER_DECLARATION_MSG_ID : {
					//~ if( !master_found ) {
						//~ Master_declaration_message_ptr recv_message = std::static_pointer_cast<Master_declaration_message>(message);

						//~ min_id = recv_message->get_id();
						//~ master_found = true;
					//~ }
				//~ break;
				//~ }

				default :
					sourceId = message->sourceInterface->hostBlock->blockId;
					cerr << "Block " << hostBlock->blockId << " received an unrecognized message from " << sourceId << endl;
				break;
			}
		break;
	}
}

SmartBlocks::SmartBlocksBlockCode* Leader_election_BlockCode::buildNewBlockCode(SmartBlocksBlock *host) {
	return(new Leader_election_BlockCode(host));
}

void Leader_election_BlockCode::master_routine() {
	i_am_master = true;
	master_found = true;
	Time time_offset = 0;
	P2PNetworkInterface *p2p;
	for( int i = NeighborDirection::North; i <= NeighborDirection::West; i++ ) {
		p2p = smartBlock->getInterface( NeighborDirection::Direction(i) );
		if( p2p->connectedInterface ) {
			time_offset += TIMESCALE + ( rand() % 25 ) * TIMESCALE ;
			Master_declaration_message * master_msg = new Master_declaration_message( min_id );
			scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent( scheduler->now() + time_offset, master_msg, p2p ));
		}
	}
	cout << "Block #" << hostBlock->blockId << " : I am the boss !" << endl;
}
