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
#include "blinkyBlocksVMCommands.h"
#include "blinkyBlocksVM.h"
#include <boost/random.hpp>

//class BlinkyBlocksVM;
//class VMCommand;

class Blinky01BlockCode : public BlinkyBlocks::BlinkyBlocksBlockCode {
private:
	commandType outBuffer[VM_COMMAND_MAX_LENGHT];
	BlinkyBlocks::BlinkyBlocksVM *vm;
	boost::interprocess::interprocess_mutex mutex_vm;
	
	int sendCommand(BlinkyBlocks::VMCommand &c);

public:
	Blinky01BlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
	~Blinky01BlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void handleCommand(BlinkyBlocks::VMCommand &command);
	void handleDeterministicMode(BlinkyBlocks::VMCommand &command);
	static BlinkyBlocks::BlinkyBlocksBlockCode *buildNewBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
	
	
	/* Lock activated only in debugging mode */
	void lockVM();
	void unlockVM();	
	
};

#endif /* BLINKY01BLOCKCODE_H_ */
