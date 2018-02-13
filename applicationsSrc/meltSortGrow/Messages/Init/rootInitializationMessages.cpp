/*
 * rootInitializationMessages.cpp
 *
 *  Created on: 12/02/2018
 *      Author: pthalamy
 */

#include "rootInitializationMessages.hpp"
#include "../../api.hpp"

void RootUpdateMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
        
    if (bc->challengeRootFitness(candidateRootPosition)) {
        bc->currentRootPosition = candidateRootPosition;
            
        // Broadcast new root candidate to every neighbor except sender
        bc->parent = destinationInterface;
        bc->sendMessageToAllNeighbors("RootUpdate",
                                      new RootUpdateMessage(bc->currentRootPosition),
                                      0, 100, 1, bc->parent);
        bc->expectedConfirms = bc->catom->getNbNeighbors() - 1; // Ignore parent

        if (!bc->expectedConfirms) {
            bc->sendMessage(new RootConfirmationMessage(bc->currentRootPosition),
                            bc->parent, 0, 100);
        }    
    } else {
        // Received best root twice, send a NACK
        bc->sendMessage(new RootRefusalMessage(candidateRootPosition),
                        destinationInterface, 0, 100);
    }
}

void handleRootUpdateAnswer(BaseSimulator::BlockCode* bsbc,
                            Cell3DPosition& rootPos) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    
    if (rootPos == bc->currentRootPosition)
        --bc->expectedConfirms;
            
    if (!bc->expectedConfirms) {
        if (bc->parent)
            bc->sendMessage(new RootConfirmationMessage(bc->currentRootPosition),
                        bc->parent, 0, 100);
        else {
            if (bc->catom->position == bc->currentRootPosition) { // Election Complete, module is root
                bc->relPos = new Cell3DPosition(0,0,0);
                bc->rtg->setOrigin(bc->catom->position);
                cout << "Absolute Target Positions" << endl; 
                for (auto x : bc->rtg->getTargetCellsAsc())
                    cout << x << endl;                        
                bc->rtg->targetCellsAsc->pop_front(); // this is the root's target position, in which it already is
                        
                // Proceed to next stage
                bc->meltOneModule();
            }
        }
    }    
}


void RootConfirmationMessage::handle(BaseSimulator::BlockCode* bsbc) {
    handleRootUpdateAnswer(bsbc, confirmationRootPosition);
}

void RootRefusalMessage::handle(BaseSimulator::BlockCode* bsbc) {
    handleRootUpdateAnswer(bsbc, refusedRootPosition);
}
