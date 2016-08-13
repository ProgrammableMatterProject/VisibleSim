/*
 * cppScheduler.cpp
 *
 *  Created on: 28 March 2015
 *      Author: Andre
 */

#include <iostream>
#include <cstdlib>
#include <algorithm>
#include <chrono>

#include "cppScheduler.h"
#include "buildingBlock.h"
#include "blockCode.h"
#include "trace.h"
#include "world.h"
#include "simulator.h"

using namespace std;
using namespace BaseSimulator::utils;
using us = chrono::microseconds;
using get_time = chrono::steady_clock;

CPPScheduler::CPPScheduler() {
	OUTPUT << "CPPScheduler constructor" << endl;
	state = NOTREADY;
	schedulerMode = SCHEDULER_MODE_REALTIME;
	schedulerThread = new thread(bind(&CPPScheduler::startPaused, this));
}

CPPScheduler::~CPPScheduler() {
	OUTPUT << "\033[1;31mCPPScheduler destructor\33[0m" << endl;
	delete schedulerThread;
	delete sem_schedulerStart;
}

void CPPScheduler::createScheduler() {
	scheduler = new CPPScheduler();
}

void CPPScheduler::deleteScheduler() {
	delete((CPPScheduler*)scheduler);
}

void *CPPScheduler::startPaused(/*void *param*/) {
	//usleep(1000000);
	cout << "\033[1;33mScheduler Mode :" << schedulerMode << "\033[0m"  << endl;
	cout << "\033[1;33mScheduler Length :" << schedulerLength << "\033[0m"  << endl;
	sem_schedulerStart->wait();

    state = RUNNING;

	multimap<Time, EventPtr>::iterator first;
	EventPtr pev;

	auto systemStartTime = get_time::now();
	cout << "\033[1;33m" << "Scheduler : start order received " << 0 << "\033[0m" << endl;

	switch (schedulerMode) {
	case SCHEDULER_MODE_FASTEST:
		while(!eventsMap.empty() || schedulerLength == SCHEDULER_LENGTH_INFINITE) {
			//JUSTE POUR DEBUG
			//~ cout << endl << "Contenu du scheduler :" << endl;
			//~ first=eventsMap.begin();
			//~ do {
			//~ std::cout << (*first).first << " : Evennement de type " << (*first).second->eventType << " au temps " << (*first).second->date << endl;
			//~ first++;
			//~ } while( first != eventsMap.end());
			//~ cout << endl;
			//

			// Check that we have not reached the maximum simulation date, if there is one
			if (currentDate > maximumDate) {
				cout << "\033[1;33m" << "Scheduler : maximum simulation date has been reached. Terminating..."
					 << "\033[0m" << endl;
				break;
			}

			if (!eventsMap.empty()) {
				first=eventsMap.begin();
				pev = (*first).second;
				currentDate = pev->date;
				pev->consume();
				StatsCollector::getInstance().incEventsCount();
				eventsMap.erase(first);
				eventsMapSize--;
			}

			if (terminate.load()) {
				break;
			}
	    }
		break;
	case SCHEDULER_MODE_REALTIME:
		cout << "Realtime mode scheduler\n";
	    while((state != ENDED && !eventsMap.empty()) || schedulerLength == SCHEDULER_LENGTH_INFINITE) {
			//gettimeofday(&heureGlobaleActuelle,NULL);
	        auto systemCurrentTime = get_time::now();
	    	auto systemCurrentTimeMax = systemCurrentTime - systemStartTime;
			//ev = *(listeEvenements.begin());
			if (!eventsMap.empty()) {
				first=eventsMap.begin();
				pev = (*first).second;
				while (!eventsMap.empty() && pev->date <= chrono::duration_cast<us>(systemCurrentTimeMax).count()) {
					first=eventsMap.begin();
					pev = (*first).second;
					currentDate = pev->date;
					//lock();
					pev->consume();
					StatsCollector::getInstance().incEventsCount();
					//unlock();
					eventsMap.erase(first);
					eventsMapSize--;
				}
			}

			if (!eventsMap.empty()) {
				//ev = *(listeEvenements.begin());
				first=eventsMap.begin();
				pev = (*first).second;
			}
			
			if (!eventsMap.empty() || schedulerLength == SCHEDULER_LENGTH_INFINITE) {
				std::chrono::milliseconds timespan(5);
				std::this_thread::sleep_for(timespan);
			}

			if (terminate.load()) {
				break;
			}
		}

		break;
	default:
		cout << "ERROR : Scheduler mode not recognized !!" << endl;
	}

	auto systemStopTime = get_time::now();
	auto elapsedTime = systemStopTime - systemStartTime;

	cout << "\033[1;33m" << "Scheduler end : " << chrono::duration_cast<us>(elapsedTime).count() << "\033[0m" << endl;

	pev.reset();

	StatsCollector::getInstance().updateElapsedTime(currentDate, chrono::duration_cast<us>(elapsedTime).count());
	StatsCollector::getInstance().setLivingCounters(Event::getNbLivingEvents(), Message::getNbMessages());
	StatsCollector::getInstance().setEndEventsQueueSize(eventsMap.size());

	// if simulation is a regression testing run, export configuration before leaving
	if (Simulator::regrTesting && !terminate.load())
		getWorld()->exportConfiguration();
	
	// if autoStop is enabled, terminate simulation
	if (willAutoStop() && !terminate.load())
		glutLeaveMainLoop();

	printStats();

	terminate.store(true);
	
	return(NULL);
}

void CPPScheduler::start(int mode) {
	CPPScheduler* sbs = (CPPScheduler*)scheduler;
	sbs->schedulerMode = mode;
	sbs->sem_schedulerStart->signal();
}
