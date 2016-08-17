/*
* MeldInterpretScheduler.cpp
*
*  Created on: 23 mars 2013
*      Author: dom
*/

#include <iostream>
#include <cstdlib>
#include <algorithm>
#include <chrono>

#include "meldInterpretScheduler.h"
#include "meldInterpretVM.h"
//#include "meldInterpretDebugger.h"
#include "meldInterpretEvents.h"
#include "simulator.h"
#include "world.h"
#include "buildingBlock.h"
#include "blockCode.h"
#include "trace.h"

using namespace std;
using us = chrono::microseconds;
using get_time = chrono::steady_clock;

namespace MeldInterpret {

MeldInterpretScheduler::MeldInterpretScheduler() {
    OUTPUT << "MeldInterpretScheduler constructor" << endl;
    state = NOTREADY;
    schedulerMode = SCHEDULER_MODE_REALTIME;
    schedulerThread = new thread(bind(&MeldInterpretScheduler::startPaused, this));
}

MeldInterpretScheduler::~MeldInterpretScheduler() {
    OUTPUT << "\033[1;31mMeldInterpretScheduler destructor\33[0m" << endl;
}

void MeldInterpretScheduler::createScheduler() {
    scheduler = new MeldInterpretScheduler();
}

void MeldInterpretScheduler::deleteScheduler() {
    delete((MeldInterpretScheduler*)scheduler);
}


void MeldInterpretScheduler::SemWaitOrReadDebugMessage() {
    if (MeldInterpretVM::isInDebuggingMode()) {
        while(!sem_schedulerStart->tryWait()) {
            //waitForOneVMCommand();
            //checkForReceivedVMCommands();
            std::chrono::milliseconds timespan(10);
            std::this_thread::sleep_for(timespan);
        }
    } else {
        sem_schedulerStart->wait();
    }
}

void *MeldInterpretScheduler::startPaused(/*void *param*/) {

    int seed = 500;
    srand (seed);
    bool hasProcessed = false;

    // 1) world ready
    // 2) user start order
    int nbSemWait = 1;

    //usleep(1000000);
    OUTPUT << "\033[1;33mScheduler Mode :" << schedulerMode << "\033[0m" << endl;
#ifndef TEST_DETER
    if (MeldInterpretVM::isInDebuggingMode()) {
        // 3) Debugger "run"
        nbSemWait = 3;
    }
    for (int i = 0; i < nbSemWait; i++) {
        SemWaitOrReadDebugMessage();
    }
#else
    sem_schedulerStart->wait();
    schedulerMode = SCHEDULER_MODE_FASTEST;
#endif
    state = RUNNING;
    //checkForReceivedVMCommands();
    multimap<Time, EventPtr>::iterator first, tmp;
    EventPtr pev;
	auto systemStartTime = get_time::now();
    auto pausedTime = systemStartTime - systemStartTime; // zero by default
	cout << "\033[1;33m" << "Scheduler : start order received " << 0 << "\033[0m" << endl;

    switch (schedulerMode) {
    case SCHEDULER_MODE_FASTEST:
        OUTPUT << "fastest mode scheduler\n" << endl;
        //MeldInterpretDebugger::print("Simulation starts in deterministic mode");
        while (state != ENDED) {
            do {
                  while (!eventsMap.empty()  || schedulerLength == SCHEDULER_LENGTH_INFINITE) {
                        hasProcessed = true;
                        lock();
                        first = eventsMap.begin();
                        pev = (*first).second;
                        currentDate = pev->date;
                        pev->consume();
                        StatsCollector::getInstance().incEventsCount();
                        eventsMap.erase(first);
                        eventsMapSize--;
                        unlock();
                        if (state == PAUSED) {
                              if (MeldInterpretVM::isInDebuggingMode()) {
                              //getDebugger()->handleBreakAtTimeReached(currentDate);
                              } else {
                                    sem_schedulerStart->wait();
                              }
                              setState(RUNNING);
                        }

                        // Check that we have not reached the maximum simulation date, if there is one
                        if (currentDate > maximumDate) {
                            cout << "\033[1;33m" << "Scheduler : maximum simulation date (" << maximumDate
                                 << ") has been reached. Terminating..." << "\033[0m" << endl;
                            break;
                        }

                        //checkForReceivedVMCommands();
                  }
                  OUTPUT << "EventMap is empty" << endl;
                  // PTHY: Equilibrium doesn't seem to be working, use SCHEDULER_LENGTH_INFINITE
                  //       to keep looping
                  if (eventsMap.empty() && schedulerLength != SCHEDULER_LENGTH_INFINITE) {
                      state = ENDED;
                      break;
                  }

                  if (terminate.load()) {
                      break;
                  }
                //checkForReceivedVMCommands();
            } while (!MeldInterpretVM::equilibrium() || !eventsMap.empty());

            if(hasProcessed) {
                hasProcessed = false;
                ostringstream s;
                s << "Equilibrium reached at "<< now() << "us ...";
                //MeldInterpretDebugger::print(s.str(), false);
                /*if (getSimulator()->testMode) {
                  BlinkyBlocks::getWorld()->dump();
                  stop(0);
                  return 0;
                  }*/
                if (MeldInterpretVM::isInDebuggingMode()) {
                    //getDebugger()->handlePauseRequest();
                }
            }

            //checkForReceivedVMCommands();
        }
#ifdef TEST_DETER
        getWorld()->killAllVMs();
        exit(0);
#endif
        break;

    case SCHEDULER_MODE_REALTIME: 
        OUTPUT << "Realtime mode scheduler\n" << endl;
        //MeldInterpretDebugger::print("Simulation starts in real time mode");
	    while((state != ENDED && !eventsMap.empty()) || schedulerLength == SCHEDULER_LENGTH_INFINITE) {
	        auto systemCurrentTime = get_time::now() - pausedTime;
	    	auto systemCurrentTimeMax = systemCurrentTime - systemStartTime;
            // currentDate = systemCurrentTimeMax;
            //checkForReceivedVMCommands();
            // while (true) {
            //     // Lock from here, to be sure that the element
            //     // is not destroyed in another thread
            //     // (previously the graphic interface was doing
            //     // it).
            //     lock();
            //     if (eventsMap.empty()) {
            //         unlock();
            //         break;
            //     }
            //     first=eventsMap.begin();
            //     pev = (*first).second;
            //     if(pev->date > systemCurrentTimeMax) {
            //         unlock();
            //         break;
            //     }
            //     currentDate = pev->date;
            //     pev->consume();
            //     eventsMap.erase(first);
            //     eventsMapSize--;
            //     unlock();
            //     //cout << "check to send" << endl;
            //     //checkForReceivedVMCommands();
            //     //cout << "ok" << endl;
            // }
            
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

            if (state == PAUSED) {
                cout << "paused" << endl;
                auto pauseBeginning = get_time::now();
                SemWaitOrReadDebugMessage();
                setState(RUNNING);
                pausedTime = get_time::now() - pauseBeginning;
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
        OUTPUT << "ERROR : Scheduler mode not recognized !!" << endl;
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

void MeldInterpretScheduler::start(int mode) {    
    // static bool done = false;
    // if ((state == NOTSTARTED) && !done) {
    //     done = true;
        MeldInterpretScheduler* sbs = (MeldInterpretScheduler*)scheduler;
        sbs->schedulerMode = mode;
        sbs->sem_schedulerStart->signal();
    // }
}

void MeldInterpretScheduler::pause(Time date) {
    //getScheduler()->scheduleLock(new VMDebugPauseSimEvent(date));
}

void MeldInterpretScheduler::unPause() {
    MeldInterpretScheduler* sbs = (MeldInterpretScheduler*)scheduler;
    if (state != RUNNING) {
        sbs->sem_schedulerStart->signal();
    }
    OUTPUT << "unpause sim" << endl;
}

void MeldInterpretScheduler::stop(Time date) {
    if (GlutContext::GUIisEnabled) {
        schedulerThread->detach();
        schedulerThread = NULL;
    }
    
    setState(ENDED);
}

bool MeldInterpretScheduler::schedule(Event *ev) {
    assert(ev != NULL);
    stringstream info;

    EventPtr pev(ev);

    OUTPUT << "MeldInterpretScheduler: Schedule a " << pev->getEventName() << " (" << ev->id << ")" << endl;

    // (pev->date > maximumDate) condition was removed because
    // Blinky Block system never ends.

    // (pev->date < Scheduler::currentDate) was removed because sometimes
    // it causes bugs: when the graphical interface thread starts to
    // schedule an event, and in this same interval, the date of the
    // scheduler changes...

    switch (schedulerMode) {
    case SCHEDULER_MODE_REALTIME:
        eventsMap.insert(pair<Time, EventPtr>(pev->date,pev));
        break;
    case SCHEDULER_MODE_FASTEST:
        if (eventsMap.count(pev->date) > 0) {
            std::pair<multimap<Time, EventPtr>::iterator,
                      multimap<Time, EventPtr>::iterator> range = eventsMap.equal_range(pev->date);
            multimap<Time, EventPtr>::iterator it = range.first;
            while (it != range.second) {
                if (it->second->randomNumber == 0) {
                    it++;
                    continue;
                }
                if (it->second->randomNumber > pev->randomNumber) {
                    break;
                }
                if (it->second->randomNumber == pev->randomNumber) {
                    while (it->second->randomNumber == pev->randomNumber) {
                        if (it->second->getConcernedBlock()->blockId > pev->getConcernedBlock()->blockId) {
                            break;
                        }
                        it++;
                    }
                    break;
                }
                it++;
            }
            eventsMap.insert(it, pair<Time, EventPtr>(pev->date,pev));
        } else {
            eventsMap.insert(pair<Time, EventPtr>(pev->date,pev));
        }
        break;
    default:
        ERRPUT << "unknown scheduler mode" << endl;
        return false;
        break;
    }
    eventsMapSize++;

    if (largestEventsMapSize < eventsMapSize) largestEventsMapSize = eventsMapSize;

    return(true);
}


bool MeldInterpretScheduler::scheduleLock(Event *ev) {
    bool ret;
    lock();
    ret = schedule(ev);
    unlock();
    return ret;
}

} // MeldInterpret namespace
