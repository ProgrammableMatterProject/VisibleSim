#ifndef MELDBLOCKCODE_H_
#define MELDBLOCKCODE_H_

#include "meldInterpretVM.h"
#include "slidingCubesBlockCode.h"
#include "slidingCubesSimulator.h"

class MeldBlockCode : public BlockCode {
private:
    bool hasWork, polling;
    MeldInterpret::MeldInterpretVM *vm;
    Time currentLocalDate; // fastest mode
public:
    MeldBlockCode(BaseSimulator::BuildingBlock *host);
    ~MeldBlockCode();

    static ModuleType moduleType;

    void startup() override;
    void init() override;
    void processLocalEvent(EventPtr pev) override;
    void setCurrentLocalDate(Time t) {currentLocalDate = t;}
    void handleDeterministicMode(/*MeldProcess::VMCommand &command*/);
    static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* MELDBLOCKCODE_H_ */
