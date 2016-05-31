/*
 * smartBlocksScheduler.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSSCHEDULER_H_
#define SMARTBLOCKSSCHEDULER_H_

#include "scheduler.h"


namespace SmartBlocks {

class SmartBlocksScheduler : public BaseSimulator::Scheduler {
protected:
	SmartBlocksScheduler();
	virtual ~SmartBlocksScheduler();
	void* startPaused(/*void *param */);

public:
	static void createScheduler();
	static void deleteScheduler();
	void start(int mode);
	static SmartBlocksScheduler* getScheduler() {
		assert(scheduler != NULL);
		return((SmartBlocksScheduler*)scheduler);
	}

	void printInfo() {
		cout << "I'm a SmartBlocksScheduler" << endl;
	}


};

inline void createScheduler() {
	SmartBlocksScheduler::createScheduler();
}

inline void deleteScheduler() {
	SmartBlocksScheduler::deleteScheduler();
}

inline SmartBlocksScheduler* getScheduler() { return(SmartBlocksScheduler::getScheduler()); }

} // SmartBlocks namespace

#endif /* SMARTBLOCKSSCHEDULER_H_ */
