#ifndef MELDBLOCKCODE_H_
#define MELDBLOCKCODE_H_

#include "meldInterpretVM.h"
#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"

class MeldBlockCode : public BlockCode {
private:
	bool hasWork, polling;
	MeldInterpret::MeldInterpretVM *vm;
	Time currentLocalDate; // fastest mode
public:
	MeldBlockCode(BaseSimulator::BuildingBlock *host);
	~MeldBlockCode();

    static ModuleType moduleType;
    
	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void setCurrentLocalDate(Time t) {currentLocalDate = t;}
	void handleDeterministicMode(/*MeldProcess::VMCommand &command*/);
	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* MELDBLOCKCODE_H_ */
