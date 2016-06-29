/*
 * scheduler.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef SCHEDULER_H_
#define SCHEDULER_H_

#include <iostream>
#include <sstream>
#include <map>
#include <inttypes.h>
#include <assert.h>
#include <thread>
#include <functional>
#include <mutex>

#include "sema.h"
#include "events.h"

using namespace std;

#define SCHEDULER_MODE_FASTEST		1
#define SCHEDULER_MODE_REALTIME		2
#define SCHEDULER_MODE_DEBUG		3

namespace BaseSimulator {

class Keyword {
public :
    string id;
    string comment;
    Keyword(string i,string c):id(i),comment(c) {};
};

template <typename T> class KeywordT:public Keyword {
    T *ptrData;
public:
    KeywordT(string i,T *ptr,string comment=""):ptrData(ptr),Keyword(i,comment) {};
};

class Scheduler {
protected:
	static Scheduler *scheduler;
	int schedulerMode;
	LightweightSemaphore *sem_schedulerStart;
	std::thread *schedulerThread;
	vector <Keyword*> tabKeywords;

	uint64_t currentDate;
	uint64_t maximumDate;
	multimap<uint64_t,EventPtr> eventsMap;
	int eventsMapSize, largestEventsMapSize;
	std::mutex mutex_schedule;
	std::mutex mutex_trace;

	Scheduler();
	virtual ~Scheduler();

	uint64_t debugDate;

public:
	enum State {NOTREADY = 0, NOTSTARTED = 1, ENDED = 2, PAUSED = 3, RUNNING = 4};
	State state;
	
	static Scheduler* getScheduler() {
		assert(scheduler != NULL);
		return(scheduler);
	}
	static void deleteScheduler() {
    	delete(scheduler);
		scheduler=NULL;
	}
	void setMaximumDate(uint64_t tmax) { maximumDate=tmax; };
	void printInfo() {
		cout << "I'm a Scheduler" << endl;
	}
	int getMode() { return schedulerMode; };

	virtual bool schedule(Event *ev);
	virtual bool scheduleLock(Event *ev);
	
	uint64_t now();
	
	virtual void trace(string message,int id=-1,const Color &color=WHITE);
	void removeEventsToBlock(BuildingBlock *bb);

	void lock();
	void unlock();

	virtual void start(int) {};

	// stop for good
	virtual void stop(uint64_t date);
	virtual void restart();
	virtual bool debug(const string &command,int &id,string &result);

	inline void setState (State s) { state = s; };
	inline State getState () { return state; };
	
	inline int getNbreMessages() { return Event::getNextId(); };

	inline void waitForSchedulerEnd() {
		schedulerThread->join();
    }
    void addKeyword(Keyword *kw) {
        tabKeywords.push_back(kw);
    }

    void removeKeywords() {
        vector<Keyword*>::const_iterator ci = tabKeywords.begin();
        while (ci!=tabKeywords.end()) {
            delete (*ci);
        }
        tabKeywords.clear();
    }
};

static inline void deleteScheduler() {
	Scheduler::deleteScheduler();
}

static inline Scheduler* getScheduler() { return(Scheduler::getScheduler()); }
    
} // BaseSimulator namespace

class ConsoleStream {
	BaseSimulator::Scheduler *scheduler;
    int blockId;
    stringstream stream;
    public :

    ConsoleStream() { stream.str(""); };
    void setInfo(BaseSimulator::Scheduler*s,int id) {
        scheduler=s;
        blockId=id;
    }
    void flush() {
        scheduler->trace(stream.str(),blockId);
        stream.str("");
    };

    ConsoleStream& operator<<(const char* value ) {
        int l=strlen(value);
        if (value[l-1]=='\n') {
            string s(value);
            s = s.substr(0,l-1);
            stream << s;
            flush();
        } else {
            stream << value;
        }
        return *this;
    }

    template<typename T>
    ConsoleStream& operator<<( T const& value ) {
        stream << value;
        return *this;
    }
};


#endif /* SCHEDULER_H_ */
