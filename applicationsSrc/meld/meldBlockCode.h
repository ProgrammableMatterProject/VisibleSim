#ifndef MELDBLOCKCODE_H_
#define MELDBLOCKCODE_H_

#include "meldInterpretVM.h"
#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"

class MeldBlockCode : public BlockCode {
private:
	bool hasWork, polling;
	MeldInterpret::MeldInterpretVM *vm;
	uint64_t currentLocalDate; // fastest mode

public:
	MeldBlockCode(BaseSimulator::BuildingBlock *host);
	~MeldBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void setCurrentLocalDate(uint64_t t) {currentLocalDate = t;}
	void handleDeterministicMode(/*MeldProcess::VMCommand &command*/);
	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* MELDBLOCKCODE_H_ */
