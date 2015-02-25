/*
 * smartBlocksScheduler.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSSCHEDULER_H_
#define SMARTBLOCKSSCHEDULER_H_

#include "scheduler.h"
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <boost/interprocess/sync/interprocess_semaphore.hpp>

namespace SmartBlocks {

class SmartBlocksScheduler : public BaseSimulator::Scheduler {
protected:
	SmartBlocksScheduler();
	virtual ~SmartBlocksScheduler();
	void* startPaused(/*void *param */);

	boost::interprocess::interprocess_semaphore *sem_schedulerStart;
	boost::thread *schedulerThread;
	int schedulerMode;
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

	//MODIF NICO
	inline void waitForSchedulerEnd() {
			schedulerThread->join();
		}
	//FIN MODIF NICO
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
