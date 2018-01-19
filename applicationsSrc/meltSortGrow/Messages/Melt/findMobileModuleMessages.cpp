/*
 * findMobileModuleMessages.cpp
 *
 *  Created on: 11/01/2018
 *      Author: pthalamy
 */

#include "findMobileModuleMessages.hpp"
#include "../../api.hpp"

FindMobileModuleMessage::FindMobileModuleMessage(short _senderOriCode,
                                                 set<short> _pathCons) {    
    senderOriCode = _senderOriCode;
    pathCons = _pathCons;
}

void FindMobileModuleMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    sender = static_cast<Catoms3DBlock*>(sourceInterface->hostBlock);

    if (!bc->meltFather) {
        bc->meltFather = sourceInterface;
        bc->resetDFSFlags();
    
        if (!bc->articulationPoint && !bc->melted) {
            vector<Catoms3DMotionRulesLink*> mrl;
            API::getAllLinks(this->sender, mrl);
            
            // Mobile Module:
            // Check if module can move to any of the path connectors of pivot
            list<Catoms3DMotionRulesLink*> rotations;
            API::findConnectorsPath(mrl,
                                    (short)this->sender->getDirection(bc->meltFather),
                                    this->pathCons,
                                    rotations);

            if (!rotations.empty()) {
                Catoms3DMotionRulesLink *nextRotation = rotations.front();
                rotations.pop_front();
                nextRotation->sendRotationEvent(bc->catom, this->sender, 100);
                return;
            }
        }

        // If module not mobile or a movement path could not be found,
        //  then propagate search to children
        set<short> myAdjacentPathConnectors;
        API::findAdjacentConnectors(this->pathCons,
                                    this->senderOriCode,
                                    bc->catom->orientationCode,
                                    myAdjacentPathConnectors);
        if (!myAdjacentPathConnectors.empty()) {
            API::findPathConnectors(bc->catom,
                                    myAdjacentPathConnectors,
                                    bc->pathConnectors);
            bc->findMobileModule();
            return;
        }
            
    }

    // Module already in DFS tree, or path to pathConnectors is blocked
    bc->sendMessage("FindMobileModuleNotFound",
                    new FindMobileModuleNotFoundMessage(),
                    bc->meltFather, 100, 0);
}

void FindMobileModuleIgnoreMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    sender = static_cast<Catoms3DBlock*>(sourceInterface->hostBlock);

    bc->flag[sourceInterface] = true;
    
    // Just proceed with next DFS child        
    bc->findMobileModule();
}


void FindMobileModuleNotFoundMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    sender = static_cast<Catoms3DBlock*>(sourceInterface->hostBlock);

    bc->flag[sourceInterface] = true;

    if (bc->meltFather)  {
        bc->sendMessage("FindMobileModuleNotFound",
                    new FindMobileModuleNotFoundMessage(),
                    bc->meltFather, 100, 0);
        bc->meltFather = NULL;
        bc->resetDFSFlags();
    } else {
        // Melt is done
        // bc->catom->setColor(RED);
        // bc->grow();
        throw exception();
    }
    
}

void FindMobileModuleFoundMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    sender = static_cast<Catoms3DBlock*>(sourceInterface->hostBlock);

    bc->flag[sourceInterface] = true;
    
    if (!bc->source) {
        bc->sendMessage("FindMobileModuleFound",
                        new FindMobileModuleFoundMessage(),
                        bc->meltFather, 100, 0);
    } else {
        bc->catom->setColor(ORANGE);
    }

    // Prepare data structures for the mobile module search DFS
    bc->resetDFSForSearching();
}
