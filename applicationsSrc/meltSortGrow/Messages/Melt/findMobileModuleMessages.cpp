/*
 * findMobileModuleMessages.cpp
 *
 *  Created on: 11/01/2018
 *      Author: pthalamy
 */

#include "findMobileModuleMessages.hpp"
#include "../../api.hpp"

FindMobileModuleMessage::FindMobileModuleMessage(list<PathHop> _path)
    : path(_path) {}

void FindMobileModuleMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    sender = static_cast<Catoms3DBlock*>(sourceInterface->hostBlock);
    short pivotConId = (short)sender->getDirection(sourceInterface);
    
    if (!bc->meltFather) {
        bc->meltFather = destinationInterface;
        bc->resetDFSFlags();

        PathHop lastHop = path.back();
        vector<short> inputConnectors;
        lastHop.getConnectorsByIncreasingDistance(inputConnectors);

        if (!bc->articulationPoint && !bc->melted) {
            vector<Catoms3DMotionRulesLink*> mrl;
            API::getAllLinks(this->sender, mrl);
            
            // Mobile Module:
            // Check if module can move to any of the path connectors of pivot.
            // Input connector set is ordered by distance to target, so the search should stop
            // as soon as a solution has been found
            list<Catoms3DMotionRulesLink*> rotations;
            API::findConnectorsPath(mrl,
                                    pivotConId,
                                    inputConnectors,
                                    rotations); ///...           

            // This will only lead the module so far, as it will simply move it to a position where it is connected to the next hop.
            // It might be preferable to compute the entire list of rotations to reach the tail at once, and only start moving afterwards
            bool motionIsPossible =
                API::buildRotationSequenceToTarget(bc->catom, pivotConId,
                                                   path, bc->meltRotationsPlan);
            
            if (!bc->meltRotationsPlan.empty()) {
                Catoms3DMotionRulesLink *nextRotation = bc->meltRotationsPlan.front();
                bc->meltRotationsPlan.pop_front();
                nextRotation->sendRotationEvent(bc->catom, this->sender, 100);
                return;
            }
        }

        // If module not mobile or a movement path could not be found,
        //  then propagate search to children
        bc->path = this->path;
        bool moduleCanBeHop = API::addModuleToPath(bc->catom, bc->path);
        if (moduleCanBeHop) {
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

    bc->flag[destinationInterface] = true;
    
    // Just proceed with next DFS child        
    bc->findMobileModule();
}


void FindMobileModuleNotFoundMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    sender = static_cast<Catoms3DBlock*>(sourceInterface->hostBlock);

    bc->flag[destinationInterface] = true;

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

    bc->flag[destinationInterface] = true;
    
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
