#ifndef MRMELDBLOCKCODE_H_
#define MRMELDBLOCKCODE_H_

#include "multiRobotsBlock.h"
#include "meldInterpretVM.h"
#include "multiRobotsBlockCode.h"
#include "multiRobotsSimulator.h"

class MRMeldBlockCode : public MultiRobots::MultiRobotsBlockCode {
private:
	bool hasWork, polling;
	MeldInterpret::MeldInterpretVM *vm;
	MultiRobots::MultiRobotsBlock *bb;
	Time currentLocalDate; // fastest mode
public:
	MRMeldBlockCode(MultiRobots::MultiRobotsBlock *host);
	~MRMeldBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void setCurrentLocalDate(Time t) {currentLocalDate = t;}
	void handleDeterministicMode(/*MeldProcess::VMCommand &command*/);
	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* MRMELDBLOCKCODE_H_ */
