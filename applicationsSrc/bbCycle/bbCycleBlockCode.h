/*
 *bbCycleBlockCode.h
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#ifndef BBCYCLEBLOCKCODE_H_
#define BBCYCLEBLOCKCODE_H_

#define SYNC_MSG_ID	9001

#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"
#include <boost/random.hpp>
#include "color.h"

class SynchroMessage;
typedef boost::shared_ptr<SynchroMessage> SynchroMessage_ptr;

class BbCycleBlockCode : public BlinkyBlocks::BlinkyBlocksBlockCode {
	P2PNetworkInterface *block2Answer;
	bool received;

public:
	BbCycleBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
	~BbCycleBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	Color getColor(uint64_t time);
	
	void sendClockToNeighbors (P2PNetworkInterface *except);	
	static BlinkyBlocks::BlinkyBlocksBlockCode *buildNewBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
};

class SynchroMessage : public Message {
public:
	uint64_t time;
	SynchroMessage(uint64_t &t);
	~SynchroMessage();
};

#endif /* BBCYCLEBLOCKCODE_H_ */
