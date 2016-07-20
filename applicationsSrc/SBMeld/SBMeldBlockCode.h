#ifndef SBMELDBLOCKCODE_H_
#define SBMELDBLOCKCODE_H_



#include "smartBlocksBlock.h"
#include "meldInterpretVM.h"
#include "smartBlocksBlockCode.h"
#include "smartBlocksSimulator.h"

class SBMeldBlockCode : public SmartBlocks::SmartBlocksBlockCode {
private:
	bool hasWork, polling;
	MeldInterpret::MeldInterpretVM *vm;
	SmartBlocks::SmartBlocksBlock *bb;
	uint64_t currentLocalDate; // fastest mode

public:
	SBMeldBlockCode(SmartBlocks::SmartBlocksBlock *host);
	~SBMeldBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void setCurrentLocalDate(uint64_t t) {currentLocalDate = t;}
	void handleDeterministicMode(/*MeldProcess::VMCommand &command*/);
	static SmartBlocks::SmartBlocksBlockCode *buildNewBlockCode(SmartBlocks::SmartBlocksBlock *host);
};

#endif /* SBMELDBLOCKCODE_H_ */
