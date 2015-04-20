#ifndef BLINKYMELDBLOCKCODE_H_
#define BLINKYMELDBLOCKCODE_H_

#include "blinkyBlocksBlock.h"
#include "meldInterpretVM.h"
#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"
#include "meldInterpretVM.h"
#include <boost/random.hpp>

class BlinkyMeldBlockCode : public BlinkyBlocks::BlinkyBlocksBlockCode {
private:
      bool hasWork, polling;
	MeldInterpret::MeldInterpretVM *vm;
	BlinkyBlocks::BlinkyBlocksBlock *bb;
	uint64_t currentLocalDate; // fastest mode


public:
	BlinkyMeldBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
	~BlinkyMeldBlockCode();

      void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void setCurrentLocalDate(uint64_t t) {currentLocalDate = t;}
	void handleDeterministicMode(/*MeldProcess::VMCommand &command*/);
	static BlinkyBlocks::BlinkyBlocksBlockCode *buildNewBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
};

#endif /* BLINKYMELDBLOCKCODE_H_ */
