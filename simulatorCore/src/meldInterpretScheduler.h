/*
 * MeldInterpretScheduler.h
 *
 *  Created on: 23 mars 2013
 *      Author: andre
 */

#ifndef MELDINTERPSCHEDULER_H_
#define MELDINTERPSCHEDULER_H_

#include <thread>
#include <functional>

#include "scheduler.h"
#include "network.h"
#include "trace.h"

namespace MeldInterpret {

class MeldInterpretScheduler : public BaseSimulator::Scheduler {
protected:
	MeldInterpretScheduler();
	virtual ~MeldInterpretScheduler();
	void* startPaused(/*void *param */);
public:
	static void createScheduler();
	static void deleteScheduler();
	static MeldInterpretScheduler* getScheduler() {
		assert(scheduler != NULL);
		return((MeldInterpretScheduler*)scheduler);
	}

	void printInfo() {
		OUTPUT << "I'm a MeldInterpretScheduler" << endl;
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
		bool r = sem_schedulerStart->tryWait();
		if (r) {
			sem_schedulerStart->signal();
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
	MeldInterpretScheduler::createScheduler();
}

inline void deleteScheduler() {
	MeldInterpretScheduler::deleteScheduler();
}

inline MeldInterpretScheduler* getScheduler() { return(MeldInterpretScheduler::getScheduler()); }

} // MeldInterpret namespace

#endif /* MELDINTERPSCHEDULER_H_ */
