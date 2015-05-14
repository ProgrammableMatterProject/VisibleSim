/*
 * messages.h
 *
 *  Created on: 20 avril 2013
 *      Author: nico
 */

#ifndef MSRSYNCMESSAGES_H_
#define MSRSYNCMESSAGES_H_

#define SYNC_MSG_ID	15000

#include "network.h"
#include <boost/shared_ptr.hpp>

class SyncMessage;

typedef boost::shared_ptr<SyncMessage> SyncMessagePtr;

class SyncMessage : public Message {
	uint64_t time;
	uint round;
	
public :
	SyncMessage(uint64_t t, uint r) : Message() { type = SYNC_MSG_ID; time = t; round = r; };
	SyncMessage(SyncMessage *m) : Message() { type = m->type; time = m->time; round = m->round; } ;
	~SyncMessage() {};

	uint64_t getTime() {return time; };
	uint getRound() { return round; };
	unsigned int size() { return 17;};
};

#endif // MSRSYNCMESSAGES_H_
