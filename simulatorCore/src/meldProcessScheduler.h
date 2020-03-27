/*
 * MeldProcessScheduler.h
 *
 *  Created on: 23 mars 2013
 *      Author: andre
 */

#ifndef MELDPROCESS_SCHEDULER_H_
#define MELDPROCESS_SCHEDULER_H_

#include <thread>
#include <functional>

#include "scheduler.h"
#include "network.h"
#include "trace.h"

namespace MeldProcess {

class MeldProcessScheduler : public BaseSimulator::Scheduler {
protected:
    MeldProcessScheduler();
    virtual ~MeldProcessScheduler();
    void* startPaused(/*void *param */);

public:
    static void createScheduler();
    static void deleteScheduler();
    static MeldProcessScheduler* getScheduler() {
        assert(scheduler != NULL);
        return((MeldProcessScheduler*)scheduler);
    }

    void printInfo() override {
        OUTPUT << "I'm a MeldProcessScheduler" << endl;
    }

    void start(int mode) override;

    void waitForSchedulerEnd() {
        schedulerThread->join();
    }

    // stop for good
    void stop(Time date) override;
    void pause(Time date);
    void unPause();

    // NOT TESTED
    bool isPaused() {
        bool r = sem_schedulerStart->tryWait();
        if (r) {
            sem_schedulerStart->signal();
        }
        return !r;
    }

    bool schedule(Event *ev) override;

    void SemWaitOrReadDebugMessage();

    inline int getMode() { return schedulerMode; }

};

inline void createScheduler() {
    MeldProcessScheduler::createScheduler();
}

inline void deleteScheduler() {
    MeldProcessScheduler::deleteScheduler();
}

inline MeldProcessScheduler* getScheduler() { return(MeldProcessScheduler::getScheduler()); }

} // MeldProcess namespace

#endif /* MELDPROCESSSCHEDULER_H_ */
