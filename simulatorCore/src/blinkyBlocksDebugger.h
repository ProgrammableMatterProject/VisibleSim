/*
 * blinkyBlocksDebugger.h
 *
 *  Created on: 21 juin 2013
 *      Author: Andre
 */

#ifndef BLINKYBLOCKSDEBUGGER_H_
#define BLINKYBLOCKSDEBUGGER_H_

#include <iostream>
#include <boost/shared_ptr.hpp>
#include <inttypes.h>
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksScheduler.h"
#include "blinkyBlocksVMCommands.h"

using namespace std;

namespace BlinkyBlocks {

class BlinkyBlocksDebugger {

protected:
	void (*debuggerCommandHandler)(uint64_t*);	
	static BlinkyBlocksDebugger *debugger;

public:
	
	BlinkyBlocksDebugger();
	~BlinkyBlocksDebugger();
	
	int sendCmd(int id, DebbuggerVMCommand &c);
	void pauseSim(int t);
	void unPauseSim();
	
	static BlinkyBlocksDebugger* getDebugger() {
		assert(debugger != NULL);
		return(debugger);
	}
	
	static void createDebugger() {
			debugger = new BlinkyBlocksDebugger();
	}
	
	static void deleteDebugger() {
		delete debugger;
		debugger = NULL;
	}
	
	void handleDebugCommand(DebbuggerVMCommand *c);
	
	void timeOut(int num);
	
	void waitForDebuggerEnd();
	
	void detachDebuggerThread();
	
	void sendTerminateCmd(int id);
	
	void handlePauseRequest();
	
	void handleBreakAtTimeReached(uint64_t t);
   
   static void print(string s, bool arrow = true);
	
};

inline void createDebugger() { BlinkyBlocksDebugger::createDebugger(); }

inline void deleteDebugger() { BlinkyBlocksDebugger::deleteDebugger(); }

inline BlinkyBlocksDebugger* getDebugger() { return(BlinkyBlocksDebugger::getDebugger()); }
	
inline int sendCommand(int id, uint64_t *data, int size) { DebbuggerVMCommand c(data); return getDebugger()->sendCmd(id, c); }

inline void pauseSimulation(int t) { getDebugger()->pauseSim(t); }

inline void unPauseSimulation() { getDebugger()->unPauseSim(); }

inline void handleDebugCommand(DebbuggerVMCommand *c) { getDebugger()->handleDebugCommand(c); }

inline void quit() {
	// DOES NOT WORK WITH NVIDIA OPTIMUS CARD...
	if(getScheduler()->getState() != Scheduler::ENDED) {
		glutLeaveMainLoop();
	}
}
	
}

#endif /* BLINKYBLOCKSDEBUGGER_H_ */
