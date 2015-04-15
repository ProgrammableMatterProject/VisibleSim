#ifndef BLINKYMELDBLOCKCODE_H_
#define BLINKYMELDBLOCKCODE_H_

#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"
#include "meldInterpretVM.h"
#include <boost/random.hpp>

class BlinkyMeldBlockCode : public BlinkyBlocks::BlinkyBlocksBlockCode {
private:
	MeldInterpretVM *vm;
	BlinkyBlocksBlock *bb;

public:
	BlinkyMeldBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
	~BlinkyMeldBlockCode();

      void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void handleDeterministicMode(/*MeldProcess::VMCommand &command*/);
	static BlinkyBlocks::BlinkyBlocksBlockCode *buildNewBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
};

#endif /* BLINKYMELDBLOCKCODE_H_ */
