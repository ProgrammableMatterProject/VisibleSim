/*
 *bbCycleBlockCode.h
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#ifndef BBCYCLEBLOCKCODE_H_
#define BBCYCLEBLOCKCODE_H_

#define SYNC_MSG_ID	9002

#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"
#include "color.h"

class SynchroMessage;
typedef std::shared_ptr<SynchroMessage> SynchroMessage_ptr;

class BbCycleBlockCode : public BlinkyBlocks::BlinkyBlocksBlockCode {
	P2PNetworkInterface *block2Answer;
	bool received[1000];
	bool cycle;
	int64_t delay;
	int idMessage;

public:
	BbCycleBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
	~BbCycleBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	Color getColor(Time time);
	
	void sendClockToNeighbors (P2PNetworkInterface *except, int hop, Time clock, int id);	
	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

class SynchroMessage : public Message {
public:
	int idSync;
	Time time;
	int nbhop;
	SynchroMessage(Time t, int hop, int id);
	unsigned int size() { return(17); }
	~SynchroMessage();
};

#endif /* BBCYCLEBLOCKCODE_H_ */
