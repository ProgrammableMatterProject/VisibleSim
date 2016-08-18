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

	void waitForSchedulerEnd() {
		schedulerThread->join();
	}

	// stop for good
	void stop(Time date);
	void pause(Time date);
	void unPause();

	// NOT TESTED
	bool isPaused() {
		bool r = sem_schedulerStart->tryWait();
		if (r) {
			sem_schedulerStart->signal();
		}
		return !r;
	}
   
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
