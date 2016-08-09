/*
 * cppScheduler.cpp
 *
 *  Created on: 28 March 2015
 *      Author: Andre
 */

#include <iostream>
#include <cstdlib>
#include <algorithm>

#include "cppScheduler.h"
#include "buildingBlock.h"
#include "blockCode.h"
#include "trace.h"
#include "world.h"
#include "simulator.h"

using namespace std;
using namespace BaseSimulator::utils;

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
	Time systemCurrentTime, systemCurrentTimeMax;

	//usleep(1000000);
	cout << "\033[1;33mScheduler Mode :" << schedulerMode << "\033[0m"  << endl;
	cout << "\033[1;33mScheduler Length :" << schedulerLength << "\033[0m"  << endl;
	sem_schedulerStart->wait();

    state = RUNNING;
	Time systemStartTime, systemStopTime;
	multimap<Time, EventPtr>::iterator first;
	EventPtr pev;

	systemStartTime = (glutGet(GLUT_ELAPSED_TIME))*1000;
	cout << "\033[1;33m" << "Scheduler : start order received " << systemStartTime << "\033[0m" << endl;

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
	    }
		break;
	case SCHEDULER_MODE_REALTIME:
		cout << "Realtime mode scheduler\n";
	    while((state != ENDED && !eventsMap.empty()) || schedulerLength == SCHEDULER_LENGTH_INFINITE) {
			//gettimeofday(&heureGlobaleActuelle,NULL);
	    	systemCurrentTime = ((Time)glutGet(GLUT_ELAPSED_TIME))*1000;
	    	systemCurrentTimeMax = systemCurrentTime - systemStartTime;
			//ev = *(listeEvenements.begin());
			if (!eventsMap.empty()) {
				first=eventsMap.begin();
				pev = (*first).second;
				while (!eventsMap.empty() && pev->date <= systemCurrentTimeMax) {
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
	    	systemCurrentTime = systemCurrentTimeMax;
			if (!eventsMap.empty()) {
				//ev = *(listeEvenements.begin());
				first=eventsMap.begin();
				pev = (*first).second;
			}
			
			/*
			  dureeAttente = ev->heureEvenement - heureActuelle;
			  dureeAttenteTimeval.tv_sec = floor(dureeAttente / 1000000);
			  dureeAttenteTimeval.tv_usec = (dureeAttente%1000000);
			  select(0,NULL,NULL,NULL,&dureeAttenteTimeval);
			*/
			if (!eventsMap.empty() || schedulerLength == SCHEDULER_LENGTH_INFINITE) {
				std::chrono::milliseconds timespan(5);
				std::this_thread::sleep_for(timespan);
			}
		}

		break;
	default:
		cout << "ERROR : Scheduler mode not recognized !!" << endl;
	}

	systemStopTime = ((Time)glutGet(GLUT_ELAPSED_TIME))*1000;

	cout << "\033[1;33m" << "Scheduler end : " << systemStopTime << "\033[0m" << endl;

	pev.reset();

	StatsCollector::getInstance().updateElapsedTime(currentDate,
													((double)(systemStopTime-systemStartTime))/1000000);
	StatsCollector::getInstance().setLivingCounters(Event::getNbLivingEvents(), Message::getNbMessages());
	StatsCollector::getInstance().setEndEventsQueueSize(eventsMap.size());

	// if simulation is a regression testing run, export configuration before leaving
	if (Simulator::regrTesting)
		getWorld()->exportConfiguration();
	
	// if autoStop is enabled, terminate simulation
	if (willAutoStop())
		glutLeaveMainLoop();

	printStats();
	
	return(NULL);
}

void CPPScheduler::start(int mode) {
	CPPScheduler* sbs = (CPPScheduler*)scheduler;
	sbs->schedulerMode = mode;
	sbs->sem_schedulerStart->signal();
}
