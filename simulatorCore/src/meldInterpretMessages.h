#ifndef MELDINTERPMESSAGES_H_
#define MELDINTERPMESSAGES_H_

#include "network.h"
#include "meldInterpretVMCore.h"

/* This is the definition of message type as extracted from the oldVM, they may stay unused */

#define NEIGHBOR_MSG	      0x01
#define LOG_MSG 		      0x50
#define CLOCK_SYNC_MSG	      0x60
#define MSG_NEIGHBOR_START 	0x01				// we can send this on startup
#define MSG_NEIGHBOR_RESPONSE	0x02			// neighbor data
#define MSG_NEIGHBOR_KEEPALIVE	0x03
#define MSG_RESP_ACK		0x08
#define MSG_RESP_NACK		0x10
#define MSG_RESP_NOREPLY	0x18
#define MSG_RESP_SENDING	0x00
#define MSG	    0x15

/* ClockSync Message type */
#define MIN_ID_TIME_LEADER_ELECTION_GO_MSG 1
#define MIN_ID_TIME_LEADER_ELECTION_BACK_MSG 2
#define CENTER_GO_MSG         5
#define CENTER_BACK_MSG       6
#define CENTER_DELETE_MSG     7
#define CENTER_ELECTED_MSG    8
#define CENTER_ELECTED_BACK_MSG 9
#define CENTER_NEXT_STEP_MSG  10

/* Here are the message type added */

#define ADD_TUPLE_MSG_ID      0x20
#define REMOVE_TUPLE_MSG_ID      0x21

namespace MeldInterpret{

class AddTupleMessage : public Message{
public:
      tuple_t tuple;

      AddTupleMessage(tuple_t tpl);
      unsigned int size();
      string getMessageName();
};

class RemoveTupleMessage : public Message{
public:
      tuple_t tuple;

      RemoveTupleMessage(tuple_t tpl);
      unsigned int size();
      string getMessageName();
};

}


#endif // MELDINTERPMESSAGES_H_
