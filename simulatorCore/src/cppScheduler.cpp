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

using namespace std;
using namespace boost;

namespace cppScheduler {

CppScheduler::CppScheduler() {
	OUTPUT << "CppScheduler constructor" << endl;
	state = NOTREADY;
	sem_schedulerStart = new boost::interprocess::interprocess_semaphore(0);
	schedulerMode = SCHEDULER_MODE_REALTIME;
	schedulerThread = new thread(bind(&CppScheduler::startPaused, this));
}

CppScheduler::~CppScheduler() {
	OUTPUT << "\033[1;31mCppScheduler destructor\33[0m" << endl;
	delete schedulerThread;
	delete sem_schedulerStart;
}

void CppScheduler::createScheduler() {
	scheduler = new CppScheduler();
}

void CppScheduler::deleteScheduler() {
	delete((CppScheduler*)scheduler);
}

void *CppScheduler::startPaused(/*void *param*/) {
	uint64_t systemCurrentTime, systemCurrentTimeMax;

	//usleep(1000000);
	cout << "\033[1;33mScheduler Mode :" << schedulerMode << "\033[0m"  << endl;
	sem_schedulerStart->wait();

    state = RUNNING;
	int systemStartTime, systemStopTime;
	multimap<uint64_t, EventPtr>::iterator first;
	EventPtr pev;

	systemStartTime = (glutGet(GLUT_ELAPSED_TIME))*1000;
	cout << "\033[1;33m" << "Scheduler : start order received " << systemStartTime << "\033[0m" << endl;

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
	    while(state!=ENDED && !eventsMap.empty()) {
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
	    		//	    	  ev = *(listeEvenements.begin());
	    		//first=eventsMap.begin();
	    		//pev = (*first).second;
	      }
	    	systemCurrentTime = systemCurrentTimeMax;
	      if (!eventsMap.empty()) {
	        //ev = *(listeEvenements.begin());
	        first=eventsMap.begin();
	        pev = (*first).second;

	        /*
	        dureeAttente = ev->heureEvenement - heureActuelle;
	        dureeAttenteTimeval.tv_sec = floor(dureeAttente / 1000000);
	        dureeAttenteTimeval.tv_usec = (dureeAttente%1000000);
	        select(0,NULL,NULL,NULL,&dureeAttenteTimeval);
	        */
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

	return(NULL);}

void CppScheduler::start(int mode) {
	CppScheduler* sbs = (CppScheduler*)scheduler;
	sbs->schedulerMode = mode;
	sbs->sem_schedulerStart->post();
}

} // BaseSimulator
