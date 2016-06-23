/*
 * leader_election_BlockCode.h
 *
 *  Created on: 20 avril 2013
 *      Author: nico
 */

#ifndef leader_election_BlockCode_H_
#define leader_election_BlockCode_H_

#include "smartBlocksBlockCode.h"
#include "smartBlocksSimulator.h"
#include "smartBlocksScheduler.h"
#include "smartBlocksBlock.h"


class Leader_election_BlockCode : public SmartBlocks::SmartBlocksBlockCode {
	bool i_am_master;
	bool master_found;

	int min_id;
	P2PNetworkInterface * min_id_sender;
	unsigned int n_answers_needed;

	void master_routine();

public:

	SmartBlocks::Scheduler *scheduler;
	SmartBlocks::SmartBlocksBlock *smartBlock;

	Leader_election_BlockCode (SmartBlocks::SmartBlocksBlock *host);
	~Leader_election_BlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);

	static SmartBlocks::SmartBlocksBlockCode *buildNewBlockCode( SmartBlocks::SmartBlocksBlock *host);
};


#endif /* leader_election_BlockCode_H_ */
