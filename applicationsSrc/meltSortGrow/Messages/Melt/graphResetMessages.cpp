
#include "graphResetMessages.hpp"

void ResetGraphMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    if (!bc->resetFather) 
        bc->resetFather = destinationInterface;

    if (bc->resetFather == destinationInterface)
        bc->propagateGraphResetBFS();
    else {
        bc->sendMessage(new ResetGraphNACKMessage(),
                        destinationInterface, MSG_DELAY, 0);
    }
}

void handleResetGraphResponse(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    bc->resetDFSFlags();
    bc->meltFather = NULL;

    if (!--bc->resetChildrenDecount) {
        if (bc->resetFather) {
            bc->sendMessage(new ResetGraphDoneMessage(),
                            bc->resetFather, MSG_DELAY, 0);
            bc->resetDFSForLabelling();
        } else { // isSource
            bc->resetDFSForLabelling();
            bc->meltOneModule();
        }                
    }

}

void ResetGraphDoneMessage::handle(BaseSimulator::BlockCode* bsbc) {
    handleResetGraphResponse(bsbc);
}

void ResetGraphNACKMessage::handle(BaseSimulator::BlockCode* bsbc) {
    handleResetGraphResponse(bsbc);
}
