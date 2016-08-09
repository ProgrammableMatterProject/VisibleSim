/*
 * scheduler.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <cstdint>
#include <iostream>
#include <stdlib.h>
#include "openglViewer.h"
#include "assert.h"
#include "scheduler.h"
#include "trace.h"
#include "stdint.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace BaseSimulator {

Scheduler *Scheduler::scheduler=NULL;

Scheduler::Scheduler() {
	OUTPUT << "Scheduler constructor" << endl;

	if (sizeof(Time) != 8) {
		ERRPUT << "\033[1;31m" << "ERROR : Scheduler requires 8bytes integer that are not available on this computer" << "\033[0m" << endl;
		exit(EXIT_FAILURE);
	}

	if (scheduler == NULL) {
		scheduler = this;
	} else {
		ERRPUT << "\033[1;31m" << "Only one Scheduler instance can be created, aborting !" << "\033[0m" << endl;
		exit(EXIT_FAILURE);
	}

	sem_schedulerStart = new LightweightSemaphore(0);
}

Scheduler::~Scheduler() {
	removeKeywords();
	OUTPUT << "Scheduler destructor" << endl;
}

bool Scheduler::schedule(Event *ev) {
	assert(ev != NULL);
	stringstream info;

	EventPtr pev(ev);

	static bool possibleOverflow = false;
	static bool tooLate = false;
	/*info << "Schedule a " << pev->getEventName() << " (" << ev->id << ")";
	trace(info.str());*/

	if (pev->date < Scheduler::currentDate) {
	        if (!possibleOverflow) {
	            cerr << "WARNING: Attempt to schedule an event in the past (possible overflow detected?)!" << endl;
	            possibleOverflow = true;
	        }
		OUTPUT << "ERROR : An event cannot be scheduled in the past !\n";
		OUTPUT << "current time : " << Scheduler::currentDate << endl;
		OUTPUT << "ev->eventDate : " << pev->date << endl;
		OUTPUT << "ev->getEventName() : " << pev->getEventName() << endl;
		return(false);
	}

	if (pev->date > maximumDate) {
	        if (!tooLate) {
	            cerr << "WARNING: Maximum simulation date reached!" << endl;
	            tooLate = true;
	        }
		OUTPUT << "WARNING : An event should not be scheduled beyond the end of simulation date !\n";
		OUTPUT << "pev->date : " << pev->date << endl;
		OUTPUT << "maximumDate : " << maximumDate << endl;
		return(false);
	}

	lock();

	eventsMap.insert(pair<Time, EventPtr>(pev->date,pev));

	eventsMapSize++;

	StatsCollector::getInstance().updateLargestEventsQueueSize(eventsMapSize);

	unlock();

	return(true);
}

void Scheduler::removeEventsToBlock(BuildingBlock *bb) {
	lock();
	multimap<Time,EventPtr>::iterator im = eventsMap.begin();
	BuildingBlock *cb=NULL;
	OUTPUT << bb << endl;
	while (im!=eventsMap.end()) {
		cb=(*im).second->getConcernedBlock();
		OUTPUT << cb << endl;
		if (cb==bb) {
			multimap<Time,EventPtr>::iterator im2 = im;
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

Time Scheduler::now() {
	return(currentDate);
}

bool Scheduler::scheduleLock(Event *ev) {
	return schedule(ev); //lock done in schedule
}

void Scheduler::trace(string message, bID id,const Color &color) {
	if (GlutContext::GUIisEnabled) {
		mutex_trace.lock();
		GlutContext::addTrace(message,id,color);
		mutex_trace.unlock();
	}
	
	OUTPUT.precision(6);
	OUTPUT << fixed << (double)(currentDate)/1000000 << " #" << id << ": " << message << endl;
}

void Scheduler::lock() {
	mutex_schedule.lock();
}

void Scheduler::unlock() {
	mutex_schedule.unlock();
}

void Scheduler::stop(Time date) {
	debugDate=date;
	schedulerMode = SCHEDULER_MODE_DEBUG;
}

void Scheduler::restart() {
	schedulerMode = SCHEDULER_MODE_REALTIME;
	currentDate=debugDate;
}

bool Scheduler::debug(const string &command,bID &id,string &result) {
	ostringstream sout;
	sout.str("");
	if (command=="help") {
		sout << "run\nstep\n";
		sout << "get@id:attribute\nAttributes:\n";
		vector<Keyword*>::const_iterator ci = tabKeywords.begin();
		while (ci!=tabKeywords.end()) {
			sout << (*ci)->id;
			if ((*ci)->comment!="") sout << "// " << (*ci)->comment << "\n";
			else sout << "\n";
			ci++;
		}

		result = sout.str();
		return false;
	}
	if (command=="run") {
		restart();
		return false;
	}
	if (command=="step") {
		debugDate = now();
		restart();
		return false;
	}
/*    if (command.substr(0,3)=="get") {
// command get@id:attribute
		size_t found = command.find(":");
		if (command.at(3)=='@') {
			if (found!=string::npos) {
				int idc=atoi(command.substr(4,found-4).c_str());
				if (idc>0 && idc<getWorld()->getNbBlocks()) {
					currentId=idc;
				} else {
					sout << "Error: bad blockId, use @" << currentId <<" instead.\n";
				}
			}
		}
		getWorld()->getBlockById(currentId)->getAttribute(command.substr(found+1),sout);
		//sout << "@" << currentId << ":" << sout.str();
		result = sout.str();
		id = currentId;
	}*/
	return true;
}

void Scheduler::printStats() {
  cout << StatsCollector::getInstance();
}


} // BaseSimulator namespace
