/*
 * leader_election_BlockCode.h
 *
 *  Created on: 20 avril 2013
 *      Author: nico
 */

#ifndef leader_election_messages_h
#define leader_election_messages_h

#define ID_OUT_MSG_ID	            9003
#define ID_ACK_MSG_ID         	  9004
#define FINAL_ACK_MSG_ID          9005
#define MASTER_DECLARATION_MSG_ID	9006

#include "network.h"
#include <boost/shared_ptr.hpp>

class Id_out_message;
class Id_ack_message;
class Final_ack_message;
class Master_declaration_message;

typedef std::shared_ptr<Id_out_message>             Id_out_message_ptr;
typedef std::shared_ptr<Id_ack_message>             Id_ack_message_ptr;
typedef std::shared_ptr<Master_declaration_message> Master_declaration_message_ptr;


class Id_out_message : public Message {
	int transmitted_id;
public :
	Id_out_message( int id ) { type = ID_OUT_MSG_ID;	transmitted_id = id; };
	~Id_out_message() {};

	int get_transmitted_id() { return transmitted_id; };
};


class Id_ack_message : public Message {
	int transmitted_id;
public :
	Id_ack_message( int id ) { type = ID_ACK_MSG_ID;	transmitted_id = id; };
	~Id_ack_message() {};

	int get_transmitted_id() { return transmitted_id; };
};

//Non utilisée pour le moment
class Master_declaration_message : public Message {
	int master_id;
public :
	Master_declaration_message( int id ) {	type = MASTER_DECLARATION_MSG_ID; this->master_id = id; };
	~Master_declaration_message() {};
	int get_id() { return master_id; };
};
#endif //leader_election_messages_h
