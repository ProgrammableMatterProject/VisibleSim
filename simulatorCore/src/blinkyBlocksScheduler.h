/*
 * BlinkyBlocksScheduler.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef BLINKYBLOCKSSCHEDULER_H_
#define BLINKYBLOCKSSCHEDULER_H_

#include "scheduler.h"
#include "network.h"
#include "blinkyBlocksBlock.h"
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <boost/interprocess/sync/interprocess_semaphore.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include "trace.h"

#include "cppScheduler.h"
#include "meldProcessScheduler.h"
#include "simulator.h"

using namespace boost;

namespace BlinkyBlocks {

inline void createScheduler() {
	switch(BaseSimulator::Simulator::getType()) {
		case BaseSimulator::Simulator::MELDPROCESS:
			MeldProcess::MeldProcessScheduler::createScheduler();
		break;
		case BaseSimulator::Simulator::CPP:
			cppScheduler::CppScheduler::createScheduler();
		break;		
	}
	
}

inline void deleteScheduler() {
	switch(BaseSimulator::Simulator::getType()) {
		case BaseSimulator::Simulator::MELDPROCESS:
			MeldProcess::MeldProcessScheduler::deleteScheduler();
		break;
		case BaseSimulator::Simulator::CPP:
			cppScheduler::CppScheduler::deleteScheduler();
		break;		
	}
}

inline BaseSimulator::Scheduler* getScheduler() { return (BaseSimulator::getScheduler()); }

} // BlinkyBlocks namespace

#endif /* BLINKYBLOCKSSCHEDULER_H_ */
