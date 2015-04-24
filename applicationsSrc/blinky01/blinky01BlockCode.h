/*
 * blinky01BlockCode.h
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#ifndef BLINKY01BLOCKCODE_H_
#define BLINKY01BLOCKCODE_H_

#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"
#include "meldProcessVMCommands.h"
#include "meldProcessVM.h"
#include <boost/random.hpp>

class Blinky01BlockCode : public BlinkyBlocks::BlinkyBlocksBlockCode {
private:
	commandType outBuffer[VM_COMMAND_MAX_LENGHT];
	MeldProcess::MeldProcessVM *vm;
	boost::interprocess::interprocess_mutex mutex_vm;
		
	void lockVM();
	void unlockVM();
	int sendCommand(MeldProcess::VMCommand &c);
	void killVM();
	
public:
	Blinky01BlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
	~Blinky01BlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void handleCommand(MeldProcess::VMCommand &command);
	void handleDeterministicMode(MeldProcess::VMCommand &command);
	static BlinkyBlocks::BlinkyBlocksBlockCode *buildNewBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);

};

#endif /* BLINKY01BLOCKCODE_H_ */
