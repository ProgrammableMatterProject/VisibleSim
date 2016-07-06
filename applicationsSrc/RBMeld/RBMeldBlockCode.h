#ifndef RBMELDBLOCKCODE_H_
#define RBMELDBLOCKCODE_H_

#include <boost/random.hpp>

#include "robotBlocksBlock.h"
#include "meldInterpretVM.h"
#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"

class RBMeldBlockCode : public RobotBlocks::RobotBlocksBlockCode {
private:
	bool hasWork, polling;
	MeldInterpret::MeldInterpretVM *vm;
	RobotBlocks::RobotBlocksBlock *bb;
	uint64_t currentLocalDate; // fastest mode

public:
	RBMeldBlockCode(RobotBlocks::RobotBlocksBlock *host);
	~RBMeldBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void setCurrentLocalDate(uint64_t t) {currentLocalDate = t;}
	void handleDeterministicMode(/*MeldProcess::VMCommand &command*/);
	static RobotBlocks::RobotBlocksBlockCode *buildNewBlockCode(RobotBlocks::RobotBlocksBlock *host);
};

#endif /* RBMELDBLOCKCODE_H_ */
