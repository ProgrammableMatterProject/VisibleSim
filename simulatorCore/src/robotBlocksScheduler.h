/*
 * robotBlocksScheduler.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef ROBOTBLOCKSSCHEDULER_H_
#define ROBOTBLOCKSSCHEDULER_H_

#include "scheduler.h"
#include "network.h"
#include "robotBlocksBlock.h"
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <boost/interprocess/sync/interprocess_semaphore.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include "trace.h"

using namespace boost;

namespace RobotBlocks {

class RobotBlocksScheduler : public BaseSimulator::Scheduler {
protected:
	boost::thread *schedulerThread;
	int schedulerMode;

	RobotBlocksScheduler();
	virtual ~RobotBlocksScheduler();
	void* startPaused(/*void *param */);

public:
	boost::interprocess::interprocess_semaphore *sem_schedulerStart;

	static void createScheduler();
	static void deleteScheduler();
	static RobotBlocksScheduler* getScheduler() {
		assert(scheduler != NULL);
		return((RobotBlocksScheduler*)scheduler);
	}

	void printInfo() {
		OUTPUT << "I'm a RobotBlocksScheduler" << endl;
	}

	void start(int mode);

	void waitForSchedulerEnd() {
		schedulerThread->join();
	}

	inline int getMode() { return schedulerMode; }

};

inline void createScheduler() {
	RobotBlocksScheduler::createScheduler();
}

inline void deleteScheduler() {
	RobotBlocksScheduler::deleteScheduler();
}

inline RobotBlocksScheduler* getScheduler() { return(RobotBlocksScheduler::getScheduler()); }

} // RobotBlocks namespace

#endif /* ROBOTBLOCKSSCHEDULER_H_ */
