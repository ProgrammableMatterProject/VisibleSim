#ifndef C3DMELDBLOCKCODE_H_
#define C3DMELDBLOCKCODE_H_



#include "catoms3DBlock.h"
#include "meldInterpretVM.h"
#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"

class C3DMeldBlockCode : public Catoms3D::Catoms3DBlockCode {
private:
	bool hasWork, polling;
	MeldInterpret::MeldInterpretVM *vm;
	Catoms3D::Catoms3DBlock *bb;
	uint64_t currentLocalDate; // fastest mode

public:
	C3DMeldBlockCode(Catoms3D::Catoms3DBlock *host);
	~C3DMeldBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	void setCurrentLocalDate(uint64_t t) {currentLocalDate = t;}
	void handleDeterministicMode(/*MeldProcess::VMCommand &command*/);
	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* C3DMELDBLOCKCODE_H_ */
