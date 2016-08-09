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

class SyncMessage;

typedef std::shared_ptr<SyncMessage> SyncMessagePtr;

class SyncMessage : public Message {
	Time time;
	uint round;
	uint hop;
	
public :
 SyncMessage(Time t, uint r, uint h) : Message() { type = SYNC_MSG_ID; time = t; round = r; hop = h;};
 SyncMessage(SyncMessage *m) : Message() { type = m->type; time = m->time; round = m->round; hop = m->hop;} ;
	~SyncMessage() {};

	Time getTime() {return time; };
	uint getRound() { return round; };
	uint getHop() { return hop;};
	unsigned int size() { return 17;};
	
};

#endif // MSRSYNCMESSAGES_H_
