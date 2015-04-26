/*
 * scheduler.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <stdlib.h>
#include "assert.h"
#include "scheduler.h"
#include "trace.h"

using namespace std;

namespace BaseSimulator {

Scheduler *Scheduler::scheduler=NULL;

Scheduler::Scheduler() {
	OUTPUT << "Scheduler constructor" << endl;

	if (sizeof(uint64_t) != 8) {
		ERRPUT << "\033[1;31m" << "ERROR : Scheduler requires 8bytes integer that are not available on this computer" << "\033[0m" << endl;
		exit(EXIT_FAILURE);
	}

	if (scheduler == NULL) {
		scheduler = this;
	} else {
		ERRPUT << "\033[1;31m" << "Only one Scheduler instance can be created, aborting !" << "\033[0m" << endl;
		exit(EXIT_FAILURE);
	}

	currentDate = 0;
	maximumDate = UINT64_MAX; // no time limitation by default

	eventsMapSize = 0;
	largestEventsMapSize = 0;
}

Scheduler::~Scheduler() {
	OUTPUT << "Scheduler destructor" << endl;
}

bool Scheduler::schedule(Event *ev) {
	assert(ev != NULL);
	stringstream info;

	EventPtr pev(ev);

	info << "Schedule a " << pev->getEventName() << " (" << ev->id << ")";
	//MODIF NICO : cette ligne me spam trop l'affichage^^
	//~ trace(info.str());

	if (pev->date < Scheduler::currentDate) {
		OUTPUT << "ERROR : An event cannot be schedule in the past !\n";
	    OUTPUT << "current time : " << Scheduler::currentDate << endl;
	    OUTPUT << "ev->eventDate : " << pev->date << endl;
	    OUTPUT << "ev->getEventName() : " << pev->getEventName() << endl;
	    return(false);
	}

	if (pev->date > maximumDate) {
		OUTPUT << "WARNING : An event should not be schedule beyond the end of simulation date !\n";
		OUTPUT << "pev->date : " << pev->date << endl;
		OUTPUT << "maximumDate : " << maximumDate << endl;
	    return(false);
	}

	lock();

	eventsMap.insert(pair<uint64_t, EventPtr>(pev->date,pev));

	eventsMapSize++;

	if (largestEventsMapSize < eventsMapSize) largestEventsMapSize = eventsMapSize;

	unlock();

	return(true);
}

void Scheduler::removeEventsToBlock(BuildingBlock *bb) {
	lock();
	multimap<uint64_t,EventPtr>::iterator im = eventsMap.begin();
	BuildingBlock *cb=NULL;
	OUTPUT << bb << endl;
	while (im!=eventsMap.end()) {
		cb=(*im).second->getConcernedBlock();
		OUTPUT << cb << endl;
		if (cb==bb) {
			multimap<uint64_t,EventPtr>::iterator im2 = im;
			if(im != eventsMap.begin()) {
				im--;
				eventsMap.erase(im2);
			} else {
				eventsMap.erase(im2);
				im = eventsMap.begin();
			}
		} else im++;
	}
	unlock();
}

uint64_t Scheduler::now() {
	return(currentDate);
}

bool Scheduler::scheduleLock(Event *ev) {
	return schedule(ev); //lock done in schedule
}

void Scheduler::trace(string message,int id,const Color &color) {
	mutex_trace.lock();
	OUTPUT.precision(6);
	OUTPUT << fixed << (double)(currentDate)/1000000 << " #" << id << ": " << message << endl;
	GlutContext::addTrace(message,id,color);
	mutex_trace.unlock();
}

void Scheduler::lock() {
	mutex_schedule.lock();
}

void Scheduler::unlock() {
	mutex_schedule.unlock();
}

} // BaseSimulator namespace
