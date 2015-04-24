/*
 * MeldProcessScheduler.h
 *
 *  Created on: 23 mars 2013
 *      Author: andre
 */

#ifndef MELDPROCESS_SCHEDULER_H_
#define MELDPROCESS_SCHEDULER_H_

#include "scheduler.h"
#include "network.h"
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <boost/interprocess/sync/interprocess_semaphore.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include "trace.h"

using namespace boost;

namespace MeldProcess {

class MeldProcessScheduler : public BaseSimulator::Scheduler {
protected:
	boost::thread *schedulerThread;
	int schedulerMode;
	
	MeldProcessScheduler();
	virtual ~MeldProcessScheduler();
	void* startPaused(/*void *param */);
	
public:
	boost::interprocess::interprocess_semaphore *sem_schedulerStart;
	
	static void createScheduler();
	static void deleteScheduler();
	static MeldProcessScheduler* getScheduler() {
		assert(scheduler != NULL);
		return((MeldProcessScheduler*)scheduler);
	}

	void printInfo() {
		OUTPUT << "I'm a MeldProcessScheduler" << endl;
	}
	
	void start(int mode);

	void waitForSchedulerEnd() {
		schedulerThread->join();
	}
		
	// stop for good
	void stop(uint64_t date);	
	void pause(uint64_t date);
	void unPause();
		
	// NOT TESTED
	bool isPaused() {
		bool r = sem_schedulerStart->try_wait();
		if (r) {
			sem_schedulerStart->post();
		}
		return !r;
	}
	
	/* In the scheduler thread, schedule function is called. In the
	 * other thread scheduleLock should be called to not interfer
	 * with the scheduler thread.
	 */
	bool schedule(Event *ev);
	bool scheduleLock(Event *ev);
	
	void SemWaitOrReadDebugMessage();
	
	inline int getMode() { return schedulerMode; }
		
};

inline void createScheduler() {
	MeldProcessScheduler::createScheduler();
}

inline void deleteScheduler() {
	MeldProcessScheduler::deleteScheduler();
}

inline MeldProcessScheduler* getScheduler() { return(MeldProcessScheduler::getScheduler()); }

} // MeldProcess namespace

#endif /* MELDPROCESSSCHEDULER_H_ */
