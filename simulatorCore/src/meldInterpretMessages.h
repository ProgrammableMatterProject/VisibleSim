#ifndef MELDINTERPMESSAGES_H_
#define MELDINTERPMESSAGES_H_

#include "network.h"
#include "meldInterpretVM.h"


#define MELD_MESSAGE_DEFAULT_SIZE 17

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

/* Here are the message type added */

#define ADD_TUPLE_MSG_ID      0x20
#define REMOVE_TUPLE_MSG_ID      0x21

namespace MeldInterpret{

class AddTupleMessage : public Message{
public:
      tuple_t tuple;
      unsigned int messageSize;

      AddTupleMessage(tuple_t tpl, unsigned int s);
      unsigned int size();
      string getMessageName();
};

class RemoveTupleMessage : public Message{
public:
      tuple_t tuple;
      unsigned int messageSize;

      RemoveTupleMessage(tuple_t tpl, unsigned int s);
      unsigned int size();
      string getMessageName();
};

}


#endif // MELDINTERPMESSAGES_H_
