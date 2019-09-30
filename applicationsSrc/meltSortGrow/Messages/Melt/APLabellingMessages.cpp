/*
 * APLabellingMessages.cpp
 *
 *  Created on: 12/02/2018
 *      Author: pthalamy
 */

#include "APLabellingMessages.hpp"
#include "../../meltSortGrowBlockCode.hpp"

void APLabellingTokenMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    bc->flag[destinationInterface] = true;

    if (bc->state == APLState::Inactive) {
        bc->state = APLState::Active;
        bc->father = destinationInterface;
        bc->lDfn = bc->dfn = bc->dfnCnt = fatherDfnCnt;

        bc->APLabellingSearch();
                
        // send VISITED(DFN(i)) to all nodes in (Neighbors(i) -Sons(i)-Father(i))
        for (auto const& module : bc->neighbors) {
            if (!bc->sons.count(module) && module != bc->father)
                bc->sendMessage(new APLabellingVisitedMessage(bc->lDfn),
                                module, MSG_DELAY, 0);
        }

    } else {
        bool senderIsChild = bc->sons.count(destinationInterface);
        if (senderIsChild) {
            bc->sons.erase(destinationInterface);
            bc->APLabellingSearch();
        }
    }        
}


void APLabellingEchoMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    bool senderIsChild = bc->sons.count(destinationInterface);
    if (senderIsChild) {
        bc->flag[destinationInterface] = true;
        bc->lDfn = std::min(bc->lDfn, lDfn); /* updates L(i) to reflect its son's smaller L */
        bc->dfnCnt = std::max(bc->dfnCnt, dfnCnt);

        if (lDfn >= bc->dfn && !bc->source) {
            bc->articulationPoint = true;
            bc->catom->setColor(PINK); // #ToRemove
        }

        bc->APLabellingSearch();
    }
}


void APLabellingVisitedMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    bc->flag[destinationInterface] = true;
            
    if (receivedDfn < bc->minDfn) {
        bc->minDfn = receivedDfn;
        bc->minSdr = destinationInterface;
    }
            
    bool senderIsChild = bc->sons.count(destinationInterface);
    if (senderIsChild) {
        bc->sons.erase(destinationInterface);
        bc->APLabellingSearch();
    }
}
