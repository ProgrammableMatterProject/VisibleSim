#ifndef C2DMELDBLOCKCODE_H_
#define C2DMELDBLOCKCODE_H_

#include <boost/random.hpp>

#include "catoms2DBlock.h"
#include "meldInterpretVM.h"
#include "catoms2DBlockCode.h"
#include "catoms2DSimulator.h"

class C2DMeldBlockCode : public Catoms2D::Catoms2DBlockCode {
private:
	bool hasWork, polling;
	MeldInterpret::MeldInterpretVM *vm;
	Catoms2D::Catoms2DBlock *bb;
	uint64_t currentLocalDate; // fastest mode

public:
	C2DMeldBlockCode(Catoms2D::Catoms2DBlock *host);
	~C2DMeldBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void setCurrentLocalDate(uint64_t t) {currentLocalDate = t;}
	void handleDeterministicMode(/*MeldProcess::VMCommand &command*/);
	static Catoms2D::Catoms2DBlockCode *buildNewBlockCode(Catoms2D::Catoms2DBlock *host);
};

#endif /* C2DMELDBLOCKCODE_H_ */
