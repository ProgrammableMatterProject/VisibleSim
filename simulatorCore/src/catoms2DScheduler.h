/*
 * catoms2DScheduler.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS2DSCHEDULER_H_
#define CATOMS2DSCHEDULER_H_

#include "scheduler.h"
#include "network.h"
#include "catoms2DBlock.h"
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <boost/interprocess/sync/interprocess_semaphore.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include "trace.h"

using namespace boost;

namespace Catoms2D {

class Catoms2DScheduler : public BaseSimulator::Scheduler {
protected:
	boost::thread *schedulerThread;
	//int schedulerMode;

	Catoms2DScheduler();
	virtual ~Catoms2DScheduler();
	void* startPaused(/*void *param */);

public:
	boost::interprocess::interprocess_semaphore *sem_schedulerStart;

	static void createScheduler();
	static void deleteScheduler();
	static Catoms2DScheduler* getScheduler() {
		assert(scheduler != NULL);
		return((Catoms2DScheduler*)scheduler);
	}

	void printInfo() {
		OUTPUT << "I'm a Catoms2DScheduler" << endl;
	}

	void start(int mode);

	void waitForSchedulerEnd() {
		schedulerThread->join();
	}

	inline int getMode() { return schedulerMode; }

};

inline void createScheduler() {
	Catoms2DScheduler::createScheduler();
}

inline void deleteScheduler() {
	Catoms2DScheduler::deleteScheduler();
}

inline Catoms2DScheduler* getScheduler() { return(Catoms2DScheduler::getScheduler()); }

} // Catoms2D namespace

#endif /* CATOMS2DSCHEDULER_H_ */
