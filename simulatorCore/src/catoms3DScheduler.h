/*
 * catoms3DScheduler.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS3DSCHEDULER_H_
#define CATOMS3DSCHEDULER_H_

#include "scheduler.h"
#include "network.h"
#include "catoms3DBlock.h"
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <boost/interprocess/sync/interprocess_semaphore.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include "trace.h"

using namespace boost;

namespace Catoms3D {

class Catoms3DScheduler : public BaseSimulator::Scheduler {
protected:
	boost::thread *schedulerThread;

	Catoms3DScheduler();
	virtual ~Catoms3DScheduler();
	void* startPaused(/*void *param */);

public:
	boost::interprocess::interprocess_semaphore *sem_schedulerStart;

	static void createScheduler();
	static void deleteScheduler();
	static Catoms3DScheduler* getScheduler() {
		assert(scheduler != NULL);
		return((Catoms3DScheduler*)scheduler);
	}

	void printInfo() {
		OUTPUT << "I'm a Catoms3DScheduler" << endl;
	}

	void start(int mode);

	void waitForSchedulerEnd() {
		schedulerThread->join();
	}

	inline int getMode() { return schedulerMode; }

};

inline void createScheduler() {
	Catoms3DScheduler::createScheduler();
}

inline void deleteScheduler() {
	Catoms3DScheduler::deleteScheduler();
}

inline Catoms3DScheduler* getScheduler() { return(Catoms3DScheduler::getScheduler()); }

} // Catoms3D namespace

#endif /* CATOMS3DSCHEDULER_H_ */
