/*
 * blinky02BlockCode.h
 *
 *  Created on: 06 juin 2013
 *      Author: ben
 */

#ifndef BLINKY02BLOCKCODE_H_
#define BLINKY02BLOCKCODE_H_

#define DIST_MSG_ID	9001
#define ACK_MSG_ID	9002

#define NEIGHBORS_NUMBER 6
#define MAX_DIST 1000

#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksScheduler.h"
#include "blinkyBlocksBlock.h"


class Dist_message;
class Ack_message;


typedef std::shared_ptr<Dist_message> Dist_message_ptr;
typedef std::shared_ptr<Ack_message> Ack_message_ptr;


class Blinky02BlockCode : public BlinkyBlocks::BlinkyBlocksBlockCode {
	unsigned int my_distance;
	bool isAck[ NEIGHBORS_NUMBER];
	P2PNetworkInterface * distance_dealer;

public:

	BlinkyBlocks::BlinkyBlocksScheduler *scheduler;
	BlinkyBlocks::BlinkyBlocksBlock *block;

	Blinky02BlockCode (BlinkyBlocks::BlinkyBlocksBlock *host);
	~Blinky02BlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);
	static BlinkyBlocks::BlinkyBlocksBlockCode *buildNewBlockCode( BlinkyBlocks::BlinkyBlocksBlock *host);

	void send_dist( unsigned int distance,  P2PNetworkInterface * by_interface, uint64_t time_offset);
	void send_ack( unsigned int distance,  P2PNetworkInterface * by_interface, uint64_t time_offset);

	bool i_can_ack();
};

class Dist_message : public Message {
	unsigned int distance;
public :
	Dist_message( unsigned int);
	~Dist_message();

	unsigned int getDistance() { return distance; };
	//~ virtual unsigned int size() { cout << "appel a size"<<endl; return(4); }
};

class Ack_message : public Message {
	unsigned int distance;
public :
	unsigned int getDistance() { return distance; };
	Ack_message( unsigned int);
	~Ack_message();
};

#endif /* BLINKY02BLOCKCODE_H_ */
