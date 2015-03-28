#include "meldProcessDebugger.h"
#include "meldProcessVM.h"
#include "Debugger/debug_Simprompt.hpp"
#include <stdio.h>
#include <boost/thread/thread.hpp>
#include <boost/bind.hpp>


namespace MeldProcess {

MeldProcessDebugger *MeldProcessDebugger::debugger=NULL;

MeldProcessDebugger::MeldProcessDebugger() {
	if (debugger == NULL) {
		debugger = this;
		debuggerCommandHandler = debugger::initDebugger(&sendCommand, &pauseSimulation, &unPauseSimulation, &quit);
	} else {
		ERRPUT << "\033[1;31m" << "Only one Debugger instance can be created, aborting !" << "\033[0m" << endl;
		exit(EXIT_FAILURE);
	}
}


int MeldProcessDebugger::sendCmd(int id, DebbuggerVMCommand &c) {
	return -1;
	/*if (id > 0) {
		if (getWorld()->sendCommand(id, c) == 1) {
			return 1;
		} else {
			return -1;
		}
	} else if (id == -1) {
		// send to all vm
		return getWorld()->broadcastDebugCommand(c);
	} else {
		return -1;
	}*/
}

void MeldProcessDebugger::handleDebugCommand(DebbuggerVMCommand *c) {
	debuggerCommandHandler(c->getData());
	delete c; // delete command object, not the data. The debugger will do it after having processed the command.
}

void MeldProcessDebugger::pauseSim(int t) {
   if (t == -1) {
		if (getScheduler()->getMode() == SCHEDULER_MODE_REALTIME) {
			getScheduler()->pause(getScheduler()->now());
		}
	} else {
		ostringstream msg;
		msg << "Time break point set at " << t << endl;
		debuggerCommandHandler(debugger::pack(debugger::PRINTCONTENT, msg.str(),1));
		debuggerCommandHandler(debugger::pack(debugger::PRINTCONTENT,"2",0));
		getScheduler()->pause(t);
	}
}

void MeldProcessDebugger::unPauseSim() {
	getScheduler()->unPause();
}

void MeldProcessDebugger::waitForDebuggerEnd() {
	debugger::joinThread();
}

void MeldProcessDebugger::detachDebuggerThread() {
	debugger::detachThread();
}

void MeldProcessDebugger::sendTerminateCmd(int id) {
	//debugger::sendCmd(id,debugger::TERMINATE,"");
}

/* Unfreezes the debugger thread when the user presses "p" in the
 * simulator graphical window.
 */
void MeldProcessDebugger::handlePauseRequest() {
	debugger::handlePauseCommand();
}

void MeldProcessDebugger::handleBreakAtTimeReached(uint64_t t) {
	ostringstream msg;
	msg << "Time break point reached at " << t << endl;
	debuggerCommandHandler(debugger::pack(debugger::TIME, msg.str(),1));
	debuggerCommandHandler(debugger::pack(debugger::TIME,"2",0));
}

void MeldProcessDebugger::print(string s, bool arrow) {   
      cout << s << endl;
      if (MeldProcessVM::isInDebuggingMode() && arrow) {
         cout << ">";
      }
}

MeldProcessDebugger::~MeldProcessDebugger() { };

}
