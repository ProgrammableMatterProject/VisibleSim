/*
 * @file scheduler.h
 * @brief Abstract discrete event scheduler that defines the base operations for processing simulation events.
 * @date 23/04/2013
 * @author dom
 */

#ifndef SCHEDULER_H_
#define SCHEDULER_H_

#include <iostream>
#include <sstream>
#include <map>
#include <cinttypes>
#include <cassert>
#include <thread>
#include <functional>
#include <condition_variable>

#include "events.h"
#include "../base/buildingBlock.h"
#include "../utils/sema.h"
#include "../stats/statsCollector.h"
#include <mutex>

using namespace std;

// Scheduler execution modes (defines the pace at which the event list is processed)
#define SCHEDULER_MODE_FASTEST		1 //!< Execute events as fast as possible. (Default in terminal mode)
#define SCHEDULER_MODE_REALTIME		2 //!< Execute events in a realistic fashion. Sleep for a while after processing an event, so that execution can be analysed in the graphical window.
#define SCHEDULER_MODE_DEBUG		3 //!< Debugger mode, execute events in steps. Not Implemented Yet!

// Scheduler termination modes (defines the conditions for the scheduler to terminate)
#define SCHEDULER_LENGTH_DEFAULT		1 //!< Scheduler stops when all events have been processed. (Or when maximum possible date for time type is reached)
#define SCHEDULER_LENGTH_BOUNDED		2 //!< Scheduler stops when all events have been processed OR when a specific date (deadline) has been reached
#define SCHEDULER_LENGTH_INFINITE		3 //!< Scheduler does not stop when all events have been processed. (Still needs to terminate is maximum possible date for time type has been reached)

namespace BaseSimulator {

//!< A keyword for the debugger (incomplete feature)
    class Keyword {
    public :
        string id;
        string comment;
        Keyword(string i,string c):id(i),comment(c) {};
    };

//!< A data-relative keyword for the debugger (incomplete feature)
    template <typename T> class KeywordT:public Keyword {
        T *ptrData;
    public:
        KeywordT(string i,T *ptr,string comment=""):ptrData(ptr),Keyword(i,comment) {};
    };

/**
 * @brief Abstract Scheduler Class
 *  Scheduler is executed on a separate thread
 */
    class Scheduler {
    protected:
        static Scheduler *scheduler; //!< Static pointer to the single scheduler instance
        static std::mutex delMutex; //!< Static mutex used to ensure non-concurrent deletion of the instance of the scheduler
        int schedulerMode; //!< Execution mode of the scheduler (1: Fastest, 2: Realtime, 3: Debug)
        int schedulerLength; //!< Termination mode of the scheduler (1:Default, 2: Bounded, 3: Infinite)
        LightweightSemaphore *sem_schedulerStart; //!< Semaphore used to synchronise scheduler thread start
        thread *schedulerThread; //!< Thread for scheduler execution
        vector <Keyword*> tabKeywords; //!< Collection of keywords for debugging (incomplete feature)

        Time currentDate = 0; //!< Current discrete date of the scheduler in (us)
        Time maximumDate = TIME_MAX; //!< Maximum possible date that the scheduler can reach before it terminates (Defaults to maximum value for discrette time type)
        multimap<Time,EventPtr> eventsMap; //!< Collection of event lists indexed by date
        int eventsMapSize = 0; //!< Number of events in the event list
        int largestEventsMapSize = 0; //!< Maximum size that the event list has reached during current simulation
        mutex mutex_schedule;	  //!< Mutex to ensure mutual exclusion during event list modification
        mutex mutex_trace;		  //!< Mutex to ensure mutual exclusion of trace buffer modification

        bool autoStart = false;		//!< Indicates if the scheduler has to wait for user input to start (false = yes, true = no)
        bool autoStop = false;		//!< Indicates if the simulation has to terminate at scheduler end (Graphical window closes if true)

        Scheduler();
        virtual ~Scheduler();

        Time debugDate; //!< Current date of debugger (incomplete feature)

        /**
         * Pointer to the module for which the scheduler is currently handling an event
         */
        BuildingBlock* contextModule = NULL;
    public:
        //!< Defines possible states of the scheduler
        enum State {
            NOTREADY = 0,           //!< Scheduler is not completely initialized yet
            NOTSTARTED = 1,			//!< Scheduler is ready to start
            ENDED = 2,				//!< Termination condition(s) have been reached, scheduler execution has ended
            PAUSED = 3,				//!< Scheduler execution is paused (during debuging for example)
            RUNNING = 4				//!< Scheduler execution is in progress
        };
        State state;				//!< Current state of the scheduler accoring to the State enum
        atomic<bool> terminate{false}; //!< Indicates if the scheduler has been instructed to terminate. Atomic value used for synchronising deletion of the scheduler and other simulation components. If terminate equals true, it means that other components are waiting for the scheduler to terminate before they can be deleted. Scheduler will finish processing current event and terminate.
        static std::mutex pause_mtx; //!< Mutex used to force the scheduler into a waiting state when it is paused
        static std::condition_variable pause_cv; //!< Condition variable used alongside pause_mtx

        //!< @brief Static getter for the global instance of Scheduler
        static Scheduler* getScheduler() {
            assert(scheduler != NULL);
            return(scheduler);
        }

        BuildingBlock* getContextModule() const {
            return contextModule;
        }

        BlockCode* getContextBlockCode() const {
            return contextModule ? contextModule->blockCode : NULL;
        }

        //!< @brief Global function for triggering scheduler deletion (Takes a bit of synchronisation, see Scheduler::terminate)
        static void deleteScheduler() {
            // Ensure Scheduler has not been deleted yet
            if (scheduler != NULL) {
                // Take the deletion lock
                delMutex.lock();
                // Ensure again that it has not been deleted meanwhile
                if (scheduler != NULL) {
                    // If scheduler is not yet in termination mode (means it is still running), instruct it to terminate
                    if (!scheduler->terminate.load()) {
                        scheduler->terminate.store(true);

                        // In case scheduler thread if waiting on semaphore before start, release it
                        if (scheduler->state == NOTSTARTED) {
                            scheduler->state = ENDED;
                            scheduler->start(SCHEDULER_MODE_FASTEST);
                        }

                        // Wait until scheduler termination
                        scheduler->schedulerThread->join();
                    }

                    // Scheduler can now be safely deleted
                    delete scheduler;
                    scheduler = NULL;
                }
                // Release lock, even though another thread was waiting, double deletion cannot occur
                delMutex.unlock();
            }
        }

        //!< @brief set a custom maximum date for the scheduler, to be used in BOUNDED termination mode
        void setMaximumDate(Time tmax) {
            maximumDate=tmax;
            cout << "scheduler: MaximumDate set to " << tmax << endl;
        }

        //!< @brief Prints information on this instance to stdout
        virtual void printInfo() {
            cout << "I'm a Scheduler" << endl;
        }

        int getNbEventsById(int id);
        bool hasEvent(int id, unsigned long blockId);

        //!< @brief Getter for Scheduler::schedulerMode
        int getMode() { return schedulerMode; };
        //!< @brief Setter for Scheduler::schedulerLength
        inline void setSchedulerLength(int sl) { schedulerLength = sl; }
        //!< @brief Getter for Scheduler::schedulerLength
        inline int getSchedulerLength() { return schedulerLength; }

        //!< @brief Setter for Scheduler::schedulerMode
        inline void setSchedulerMode(int sm) { schedulerMode = sm; }
        //!< @brief Getter for Scheduler::schedulerMode
        inline int getSchedulerMode() { return schedulerMode; }

        //!< @brief Setter for Scheduler::autoStart
        inline void setAutoStart(bool as) { autoStart = as; }
        //!< @brief Getter for Scheduler::autoStart
        inline bool willAutoStart() { return autoStart; }

        //!< @brief Setter for Scheduler::autoStop
        inline void setAutoStop(bool as) { autoStop = as; }
        //!< @brief Getter for Scheduler::autoStart
        inline bool willAutoStop() { return autoStop; }

        /** @brief Schedule a new event ev
         *  @param ev event to schedule
         *	Adds the event to the event list corresponding to its date (ev->date), or ignore it if event date is in the past,
         *   or after the maximum simulation date. Event list update done in mutual exclusion.
         *  @return true if event has been added to the event list, false otherwise
         */
        virtual bool schedule(Event *ev);


        /** @brief Return current scheduler date
         *  @return Scheduler::currentDate
         */
        inline Time now() { return(currentDate); };

        /**
         * Pauses the scheduler and processing of events, or resumes it if scheduler state
         *  was PAUSED
         * @warning only works in Realtime mode scheduler ('r').
         * @note can be triggered from the GUI using the <SPACE> key
         */
        void toggle_pause();

        /** @brief Print a block-relative colored message to the console
         *  @param message String to print
         *  @param id module identifier of the concerned block
         *  @param color color of the message, WHITE by default
         */
        virtual void trace(string message,bID id=0,const Color &color=WHITE);

        /** @brief Remove all events relative to module bb from events list, in case of module deletion for example
         *  @param bb module from which the events have to be cleared
         */
        void removeEventsToBlock(BuildingBlock *bb);

        //!< @brief Lock the event list mutex
        inline void lock() { mutex_schedule.lock(); };
        //!< @brief Unlock the event list mutex
        inline void unlock() { mutex_schedule.unlock(); };

        //!< @brief Start scheduler execution according to the specified mode
        virtual void start(int mode);

        //!< @attention Related to debugger, not completely implemented yet. (incomplete feature)
        //!< @todo Document when debugger is implemented
        virtual void stop(Time date);
        //!< @attention Related to debugger, not completely implemented yet. (incomplete feature)
        //!< @todo Document when debugger is implemented
        virtual void restart();
        //!< @attention Related to debugger, not completely implemented yet. (incomplete feature)
        //!< @todo Document when debugger is implemented
        virtual bool debug(const string &command,bID &id,string &result);

        //!< @brief Setter for scheduler state
        inline void setState (State s) { state = s; };
        //!< @brief Getter for scheduler state
        inline State getState () { return state; };

        //!< @brief Returns the number of events that have been generated
        inline int getNbreMessages() { return Event::getNextId(); };

        //!< @brief Used to synchronise the Scheduler thread with the graphical interface, or other simulation components
        //!<  (destructors), to ensure that scheduler is effectively stopped before releasing memory
        inline void waitForSchedulerEnd() {
            schedulerThread->join();
        }

        //!< @attention Related to debugger, not completely implemented yet. (incomplete feature)
        //!< @todo Document when debugger is implemented
        void addKeyword(Keyword *kw) {
            tabKeywords.push_back(kw);
        }

        //!< @attention Related to debugger, not completely implemented yet. (incomplete feature)
        //!< @todo Document when debugger is implemented
        void removeKeywords() {
            vector<Keyword*>::const_iterator ci = tabKeywords.begin();
            while (ci!=tabKeywords.end()) {
                delete (*ci);
            }
            tabKeywords.clear();
        }

        //!< Print the statistics collected by StatsCollector during simulation to the standard output
        void printStats();
    };

//!< @brief Call Scheduler::deleteScheduler on the simulation's Scheduler instance
    static inline void deleteScheduler() {
        Scheduler::deleteScheduler();
    }

//!< @brief Return a pointer to the simulation's Scheduler instance
    static inline Scheduler* getScheduler() { return(Scheduler::getScheduler()); }

} // BaseSimulator namespace

#endif /* SCHEDULER_H_ */
