/*
 * scheduler.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <cstdint>
#include <iostream>
#include <cassert>
#include <cstdint>

#include "../gui/openglViewer.h"
#include "scheduler.h"
#include "../utils/trace.h"
#include "../stats/statsIndividual.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace BaseSimulator {

Scheduler *Scheduler::scheduler=NULL;
std::mutex Scheduler::delMutex;
std::mutex Scheduler::pause_mtx;
std::condition_variable Scheduler::pause_cv;

Scheduler::Scheduler() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "Scheduler constructor" << endl;
#endif

    if (sizeof(Time) != 8) {
        ERRPUT << TermColor::ErrorColor << "ERROR : Scheduler requires 8bytes integer that are not available on this computer" << TermColor::Reset << endl;
        exit(EXIT_FAILURE);
    }

    if (scheduler == NULL) {
        scheduler = this;
    } else {
        ERRPUT << TermColor::ErrorColor << "Only one Scheduler instance can be created, aborting !" << TermColor::Reset << endl;
        exit(EXIT_FAILURE);
    }

    sem_schedulerStart = new LightweightSemaphore(0);
}

Scheduler::~Scheduler() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "Scheduler destructor" << endl;
#endif
    removeKeywords();
    if (schedulerThread)
        delete schedulerThread;
    delete sem_schedulerStart;
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
    // print stats for debug
    //std::cout <<"stat\t" << (pev->getConcernedBlock()?pev->getConcernedBlock()->blockId:0) << "\t" << Scheduler::currentDate << "\t" << eventsMapSize << "\t" << pev->eventType <<std::endl;

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

void Scheduler::trace(string message, bID id,const Color &color) {
    if (GlutContext::GUIisEnabled) {
        mutex_trace.lock();
        GlutContext::addTrace(message,id,color);
        mutex_trace.unlock();
    }

    OUTPUT.precision(6);
    OUTPUT << fixed << (double)(currentDate)/1000000 << " #" << id << ": " << message << endl;
}

void Scheduler::start(int mode) {
    scheduler->schedulerMode = mode;
    scheduler->sem_schedulerStart->signal();
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

int Scheduler::getNbEventsById(int id) {
    lock();
    multimap<Time,EventPtr>::iterator im = eventsMap.begin();
    int count = 0;
    while (im!=eventsMap.end()) {
        if (im->second->eventType == id)
            count++;
        im++;
    }
    unlock();
    return count;
}

bool Scheduler::hasEvent(int id, unsigned long blockId) {
    lock();
    multimap<Time,EventPtr>::iterator im = eventsMap.begin();
    while (im!=eventsMap.end()) {
        if (im->second->eventType == id && im->second->getConcernedBlock()->blockId == blockId) {
            unlock();
            return true;
        }
        im++;
    }
    unlock();
    return false;
}

void Scheduler::printStats() {
  cout << StatsCollector::getInstance();
  if (StatsIndividual::enable) {
    cout << StatsIndividual::getStats();
  }
}

void Scheduler::toggle_pause() {
    if (state > Scheduler::ENDED) {
        std::unique_lock<std::mutex> lck(pause_mtx);
        if (state == Scheduler::PAUSED) {
            state = Scheduler::RUNNING;
            pause_cv.notify_one();
        } else {
            state = Scheduler::PAUSED;
        }
    }
}

} // BaseSimulator namespace
