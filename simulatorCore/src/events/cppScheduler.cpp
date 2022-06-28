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
#include "../base/buildingBlock.h"
#include "../base/blockCode.h"
#include "../utils/trace.h"
#include "../base/world.h"
#include "../base/simulator.h"
#include "../replay/replayExporter.h"

using namespace std;
using namespace BaseSimulator::utils;
using us = chrono::microseconds;
using get_time = chrono::steady_clock;

CPPScheduler::CPPScheduler() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "CPPScheduler constructor" << endl;
#endif
    state = NOTREADY;
    schedulerMode = SCHEDULER_MODE_REALTIME;
    schedulerThread = new thread(bind(&CPPScheduler::startPaused, this));
}

CPPScheduler::~CPPScheduler() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "CPPScheduler destructor\33[0m" << endl;
#endif
}

void CPPScheduler::createScheduler() {
    scheduler = new CPPScheduler();
}

void CPPScheduler::deleteScheduler() {
    delete((CPPScheduler*)scheduler);
}

void *CPPScheduler::startPaused(/*void *param*/) {
    cout << TermColor::SchedulerColor << "Scheduler Mode :" << schedulerMode << TermColor::Reset  << endl;
    cout << TermColor::SchedulerColor << "Scheduler Length :" << schedulerLength << TermColor::Reset  << endl;
    sem_schedulerStart->wait();

    // if ENDED: Simulation terminated before scheduler start, quitting
    if (state != ENDED) {

        state = RUNNING;

        multimap<Time, EventPtr>::iterator first;
        EventPtr pev;

        auto systemStartTime = get_time::now();
        cout << TermColor::SchedulerColor << "" << "Scheduler : start order received " << 0 << TermColor::Reset << endl;

        // Write first key frame
        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writeKeyFrame(0);

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

                    // Check that we have not reached the maximum simulation date,
                    //  if there is one
                    if (currentDate > maximumDate) {
                        cout << TermColor::SchedulerColor << "" << "Scheduler : maximum simulation date (" << maximumDate
                             << ") has been reached. Terminating..." << TermColor::Reset << endl;
                        break;
                    }

                    if (ReplayExporter::isReplayEnabled())
                        ReplayExporter::getInstance()->writeKeyFrameIfNeeded(currentDate);

                    if (!eventsMap.empty()) {
                        first=eventsMap.begin();
                        pev = (*first).second;
                        currentDate = pev->date;
                        contextModule = pev->getConcernedBlock();
                        pev->consume();
                        contextModule = NULL;
                        StatsCollector::getInstance().incEventsCount();
                        eventsMap.erase(first);
                        eventsMapSize--;
                    }

                    if (terminate.load()) {
                        break;
                    }
                }
                break;
            case SCHEDULER_MODE_REALTIME: {
                cout << "Realtime mode scheduler\n";
                auto globalPauseTime = get_time::now() - get_time::now();
                while((state != ENDED && !eventsMap.empty())
                      || schedulerLength == SCHEDULER_LENGTH_INFINITE) {

                    //gettimeofday(&heureGlobaleActuelle,NULL);
                    // cout << "globalPauseTime1: " << static_cast<uint64_t>(chrono::duration_cast<us>(globalPauseTime).count()) << endl;
                    auto systemCurrentTime = get_time::now() - globalPauseTime;
                    auto systemCurrentTimeMax = systemCurrentTime - systemStartTime;
                    //ev = *(listeEvenements.begin());
                    if (!eventsMap.empty()) {
                        first=eventsMap.begin();
                        pev = (*first).second;
                        while (!eventsMap.empty() && pev->date <= static_cast<uint64_t>(chrono::duration_cast<us>(systemCurrentTimeMax).count())) {

                            auto prePauseTime = get_time::now();
                            std::unique_lock<std::mutex> lck(scheduler->pause_mtx);
                            pause_cv.wait(lck, [=]() { return state == RUNNING; });
                            auto pauseDuration = get_time::now() - prePauseTime;
                            globalPauseTime += pauseDuration;
                            // cout << "PAUSED FOR: " << static_cast<uint64_t>(chrono::duration_cast<us>(pauseDuration).count()) << endl;
                            // cout << "globalPauseTime2: " << static_cast<uint64_t>(chrono::duration_cast<us>(globalPauseTime).count()) << endl;

                            if (ReplayExporter::isReplayEnabled())
                                ReplayExporter::getInstance()->writeKeyFrameIfNeeded(currentDate);

                            first=eventsMap.begin();
                            pev = (*first).second;
                            currentDate = pev->date;
                            //lock();
                            contextModule = pev->getConcernedBlock();
                            pev->consume();
                            contextModule = NULL;
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

            } break;
            default:
                cout << "ERROR : Scheduler mode not recognized !!" << endl;
        }

        auto systemStopTime = get_time::now();
        auto elapsedTime = systemStopTime - systemStartTime;

        cout << TermColor::SchedulerColor << "Scheduler end : " << chrono::duration_cast<us>(elapsedTime).count() << TermColor::Reset << endl;

        pev.reset();

        StatsCollector::getInstance().updateElapsedTime(currentDate, chrono::duration_cast<us>(elapsedTime).count());
        StatsCollector::getInstance().setLivingCounters(Event::getNbLivingEvents(), Message::getNbMessages());
        StatsCollector::getInstance().setEndEventsQueueSize(eventsMap.size());

        // if simulation is a regression testing run, export configuration before leaving
        if ((Simulator::regrTesting or Simulator::exportFinalConfiguration)
            && !terminate.load()) {
            getWorld()->exportConfiguration();
        }
        getWorld()->onEndOfSimulation();

        // if autoStop is enabled, terminate simulation
        if (willAutoStop() && !terminate.load()) {
            glutLeaveMainLoop();
        }

        printStats();
    }

    terminate.store(true);
    schedulerThread = NULL;	// No need for the scheduler to delete this thread, it will have terminated already

    // Terminate replay export if enabled
    if (ReplayExporter::isReplayEnabled()) {
        // Export final key frame
        ReplayExporter::getInstance()->endExport();
    }

    return(NULL);
}
