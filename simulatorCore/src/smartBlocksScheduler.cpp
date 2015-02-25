/*
 * smartBlocksScheduler.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include <stdlib.h>
#include <boost/asio.hpp>
#include "smartBlocksScheduler.h"

using namespace std;
using namespace boost;
namespace SmartBlocks {

SmartBlocksScheduler::SmartBlocksScheduler() {
	//MODIF NICO
	sem_schedulerStart = new boost::interprocess::interprocess_semaphore(0);
	//FIN MODIF NICO
	schedulerMode = SCHEDULER_MODE_FASTEST;
	OUTPUT << "SmartBlocksScheduler constructor" << endl;
	state = NOTREADY;
	schedulerThread = new thread(bind(&SmartBlocksScheduler::startPaused, this));
}

SmartBlocksScheduler::~SmartBlocksScheduler() {
	OUTPUT << "\033[1;31mSmartBlocksScheduler destructor\33[0m" << endl;

	delete sem_schedulerStart;
	delete schedulerThread;
}

void SmartBlocksScheduler::createScheduler() {
	scheduler = new SmartBlocksScheduler();
}

void SmartBlocksScheduler::deleteScheduler() {
	delete((SmartBlocksScheduler*)scheduler);
}

void *SmartBlocksScheduler::startPaused(/*void *param*/) {
	//MultiCoresScheduler *scheduler = (MultiCoresScheduler*)param;
	bool mustStop;
	uint64_t systemCurrentTime, systemCurrentTimeMax;

	usleep(1000000);
	OUTPUT << "\033[1;33mScheduler Mode :" << schedulerMode << endl;

	sem_schedulerStart->wait();

	state = RUNNING;
	int systemStartTime, systemStopTime;
	multimap<uint64_t, EventPtr>::iterator first;
	EventPtr pev;

	systemStartTime = (glutGet(GLUT_ELAPSED_TIME))*1000;
	OUTPUT << "\033[1;33m" << "Scheduler : start order received " << systemStartTime << "\033[0m" << endl;

	switch (schedulerMode) {
	case SCHEDULER_MODE_FASTEST:

		while ( (!eventsMap.empty() ) && currentDate < maximumDate) {
//JUSTE POUR DEBUG
//~ cout << endl << "Contenu du scheduler :" << endl;
//~ first=eventsMap.begin();
//~ do {
	//~ std::cout << (*first).first << " : Evennement de type " << (*first).second->eventType << " au temps " << (*first).second->date << endl;
	//~ first++;
//~ } while( first != eventsMap.end());
//~ cout << endl;
//
	    	first=eventsMap.begin();
	    	pev = (*first).second;
	    	currentDate = pev->date;
			pev->consume();
			eventsMap.erase(first);
			eventsMapSize--;
	    }
		break;

	case SCHEDULER_MODE_REALTIME:
		cout << "Realtime mode scheduler\n";
		mustStop = false;
	    while(!mustStop && !eventsMap.empty()) {
	      //gettimeofday(&heureGlobaleActuelle,NULL);
	    	systemCurrentTime = ((uint64_t)glutGet(GLUT_ELAPSED_TIME))*1000;
	    	systemCurrentTimeMax = systemCurrentTime - systemStartTime;
	      //ev = *(listeEvenements.begin());
	    	first=eventsMap.begin();
	    	pev = (*first).second;
	    	while (!eventsMap.empty() && pev->date <= systemCurrentTimeMax) {
	    		first=eventsMap.begin();
	    		pev = (*first).second;

			  /* traitement du mouvement des objets physiques*/
			  //Physics::update(ev->heureEvenement);
	    		currentDate = pev->date;
	    		//lock();
	    		pev->consume();
	    		//unlock();
	    		//pev->nbRef--;

	    		//listeEvenements.pop_front();
	    		eventsMap.erase(first);
	    		eventsMapSize--;
	    		//ev = *(listeEvenements.begin());
	    		//first=eventsMap.begin();
	    		//pev = (*first).second;
	      }
	    systemCurrentTime = systemCurrentTimeMax;
	    if (!eventsMap.empty()) {
	        //ev = *(listeEvenements.begin());
	        first=eventsMap.begin();
	        pev = (*first).second;

#ifdef WIN32
			Sleep(5);
#else
	        usleep(5000);
#endif
	      }
	    }

		break;
	default:
		cout << "ERROR : Scheduler mode not recognized !!" << endl;
	}

	systemStopTime = ((uint64_t)glutGet(GLUT_ELAPSED_TIME))*1000;

	cout << "\033[1;33m" << "Scheduler end : " << systemStopTime << "\033[0m" << endl;

	pev.reset();

	cout << "end time : " << currentDate << endl;
	cout << "real time elapsed : " << ((double)(systemStopTime-systemStartTime))/1000000 << endl;

//	cout << "Nombre d'événements restants en mémoire : " << Evenement::nbEvenements << endl;
//	cout << "Nombre de messages restants en mémoire : " << Message::nbMessages << endl;
	cout << "Maximum sized reached by the events list : " << largestEventsMapSize << endl;
	cout << "Size of the events list at the end : " << eventsMap.size() << endl;
	cout << "Number of events processed : " << Event::getNextId() << endl;
	cout << "Events(s) left in memory before destroying Scheduler : " << Event::getNbLivingEvents() << endl;
	cout << "Message(s) left in memory before destroying Scheduler : " << Message::getNbMessages() << endl;

	return(NULL);
}

void SmartBlocksScheduler::start(int mode) {
	SmartBlocksScheduler* sbs = (SmartBlocksScheduler*)scheduler;
	sbs->schedulerMode = mode;
	sbs->sem_schedulerStart->post();
}

} // SmartBlocks namespace
