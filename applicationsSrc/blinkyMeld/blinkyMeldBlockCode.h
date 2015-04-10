#ifndef BLINKYMELDBLOCKCODE_H_
#define BLINKYMELDBLOCKCODE_H_

#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"
#include "meldInterpretVM.h"
#include <boost/random.hpp>

class BlinkyMeldBlockCode : public BlinkyBlocks::BlinkyBlocksBlockCode {
private:
	MeldInterpretVM *vm;

public:
	BlinkyMeldBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
	~BlinkyMeldBlockCode();

	void startup();
	void init();
	static BlinkyBlocks::BlinkyBlocksBlockCode *buildNewBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
};

#endif /* BLINKYMELDBLOCKCODE_H_ */
