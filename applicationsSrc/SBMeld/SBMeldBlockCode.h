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
	Time currentLocalDate; // fastest mode

public:
	SBMeldBlockCode(SmartBlocks::SmartBlocksBlock *host);
	~SBMeldBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void setCurrentLocalDate(Time t) {currentLocalDate = t;}
	void handleDeterministicMode(/*MeldProcess::VMCommand &command*/);
	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* SBMELDBLOCKCODE_H_ */
