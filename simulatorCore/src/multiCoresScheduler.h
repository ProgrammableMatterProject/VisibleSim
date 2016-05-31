/*
 * multiCoresScheduler.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef MULTICORESSCHEDULER_H_
#define MULTICORESSCHEDULER_H_

#include <set>
#include <boost/asio.hpp>
#include <boost/thread.hpp>
#include "scheduler.h"
#include "network.h"

using namespace boost;
using boost::asio::ip::udp;
using boost::asio::ip::tcp;

namespace MultiCores {

#define VM_MESSAGE_TYPE_START_SIMULATION        1
#define VM_MESSAGE_TYPE_END_SIMULATION          2
#define VM_MESSAGE_TYPE_CREATE_LINK             3
#define VM_MESSAGE_TYPE_COMPUTATION_LOCK        4
#define VM_MESSAGE_TYPE_COMPUTATION_UNLOCK      5
#define VM_MESSAGE_TYPE_SEND_MESSAGE            6
#define VM_MESSAGE_TYPE_RECEIVE_MESSAGE         7
#define VM_MESSAGE_TYPE_WAIT_FOR_MESSAGE		8

#ifdef DEBUG_VM_MESSAGES
#define VM_TRACE_MESSAGE(__mes)				getScheduler()->trace(__mes.printVMMessage());
#else
#define VM_TRACE_MESSAGE(__mes)
#endif

typedef struct VMMessage_tt {
        uint64_t messageType;
        uint64_t param1;
        uint64_t param2;
        uint64_t param3;
} VMMessage_t_old;

class VMMessage {
public:
    uint64_t messageType;
    uint64_t param1;
    uint64_t param2;
    uint64_t param3;

    string printVMMessage() {
    	stringstream ss;
    	switch(messageType) {
    	case VM_MESSAGE_TYPE_START_SIMULATION:
    		ss << "Message from VM : START_SIMULATION ";
    		ss << "( " << param1 << " blocks)";
    		break;
    	case VM_MESSAGE_TYPE_END_SIMULATION:
    		ss << "Message from VM : END_SIMULATION";
    		break;
    	case VM_MESSAGE_TYPE_CREATE_LINK:
    		ss << "Message from VM : CREATE_LINK ";
    		ss << "from " << param1 << " to " << param2;
    		break;
    	case VM_MESSAGE_TYPE_COMPUTATION_LOCK:
    		ss << "Message from VM : COMPUTATION_LOCK ";
    		ss << "to block " << param1 << " for " << param2 << " microseconds";
    		break;
    	case VM_MESSAGE_TYPE_COMPUTATION_UNLOCK:
    		ss << "Message to VM : COMPUTATION_UNLOCK ";
    		ss << "from block " << param1 << " at t=" << param2;
    		break;
    	case VM_MESSAGE_TYPE_SEND_MESSAGE:
    		ss << "Message from VM : SEND_MESSAGE ";
    		ss << param3 << " bytes from block " << param1 << " to block " << param2;
    		break;
    	case VM_MESSAGE_TYPE_RECEIVE_MESSAGE:
    		ss << "Message to VM : RECEIVE_MESSAGE ";
    		ss << "block " << param1 << " received " << param3 << " bytes at t=" << param2;
    		break;
    	case VM_MESSAGE_TYPE_WAIT_FOR_MESSAGE:
    		ss << "Message from VM : WAIT_FOR_MESSAGE ";
    		ss << "block " << param1;
    		break;
    	default:
    		ss << " *** UNKNOWN MESSAGE TYPE ***";
    		break;
    	}
    	return(ss.str());
    }
};

class VMDataMessage : public Message {
protected:
	unsigned int dataSize;
public:
	VMDataMessage(unsigned int s):Message() {
		dataSize = s;
	}
	virtual unsigned int size() {
		unsigned int baseSize = Message::size();
		return(baseSize+dataSize);
	}
};

class MultiCoresScheduler : public BaseSimulator::Scheduler {
protected:
	MultiCoresScheduler();
	virtual ~MultiCoresScheduler();
	void* startPaused(/*void *param */);

	set<int> undefinedBlocksSet;
	boost::thread *schedulerThread;
	boost::asio::io_service *ios;
	boost::asio::io_service ios2;
	udp::socket *socket;
	udp::endpoint VMEndpoint;
	tcp::socket socket2;
	tcp::acceptor *acceptor;
	//int schedulerMode;

public:
	static void createScheduler();
	static void deleteScheduler();
	static MultiCoresScheduler* getScheduler() {
		assert(scheduler != NULL);
		return((MultiCoresScheduler*)scheduler);
	}

	void printInfo() {
		cout << "I'm a MultiCoresScheduler" << endl;
	}

	void waitForSchedulerEnd() {
		schedulerThread->join();
	}

	void addUndefinedBlock(int id) {
		undefinedBlocksSet.insert(id);
	}
	void removeUndefinedBlock(int id) {
		undefinedBlocksSet.erase(id);
	}
	bool undefinedBlocksSetIsEmpty() {
		return(undefinedBlocksSet.empty());
	}
	bool isBlockUndefined(int id) {
//		printUndefinedBlocksSet();
		set<int>::iterator res;
		res = undefinedBlocksSet.find(id);
		if (res != undefinedBlocksSet.end()) {
			return(true);
		} else {
			return(false);
		}
	}
	void printUndefinedBlocksSet() {
		set<int>::iterator it = undefinedBlocksSet.begin();
		cout << "undefined Blocks set : ";
		while (it != undefinedBlocksSet.end()) {
			cout << (*it) << " ";
			it++;
		}
		cout << endl;
	}

	void waitForVMMessage();
	void sendMessageToVM(VMMessage message);

};

inline void createScheduler() {
	MultiCoresScheduler::createScheduler();
}

inline void deleteScheduler() {
	MultiCoresScheduler::deleteScheduler();
}

inline MultiCoresScheduler* getScheduler() { return(MultiCoresScheduler::getScheduler()); }

} // MultiCores namespace

#endif /* MULTICORESSCHEDULER_H_ */
