/*
 * CppScheduler.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CPPSCHEDULER_H_
#define CPPSCHEDULER_H_

#include "scheduler.h"
#include "network.h"
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <boost/interprocess/sync/interprocess_semaphore.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include "trace.h"

using namespace boost;

namespace cppScheduler {

class CppScheduler : public BaseSimulator::Scheduler {
protected:
	boost::thread *schedulerThread;
	int schedulerMode;

	CppScheduler();
	virtual ~CppScheduler();
	void* startPaused(/*void *param */);

public:
	boost::interprocess::interprocess_semaphore *sem_schedulerStart;

	static void createScheduler();
	static void deleteScheduler();
	static CppScheduler* getScheduler() {
		assert(scheduler != NULL);
		return((CppScheduler*)scheduler);
	}

	void printInfo() {
		OUTPUT << "I'm a CppScheduler" << endl;
	}

	void start(int mode);

	void waitForSchedulerEnd() {
		schedulerThread->join();
	}

	inline int getMode() { return schedulerMode; }

};

inline void createScheduler() {
	CppScheduler::createScheduler();
}

inline void deleteScheduler() {
	CppScheduler::deleteScheduler();
}

inline CppScheduler* getScheduler() { return(CppScheduler::getScheduler()); }

} // CppScheduler namespace

#endif /* CPPSCHEDULER_H_ */
