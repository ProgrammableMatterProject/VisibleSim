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
    short catomDockingConnector = bc->catom->getDirection(destinationInterface);
    short pivotDockingConnector = static_cast<Catoms3DBlock*>
        (sourceInterface->hostBlock)->getDirection(sourceInterface); // so-so
    
    if (!bc->meltFather) {        
        bc->meltFather = destinationInterface;
        bc->resetDFSFlags();
        bc->path = this->path;
        
        if (!bc->articulationPoint && !bc->melted) {
            bool rotationIsPossible = bc->computeNextRotation(path);
            if (!rotationIsPossible) 
            {
                cout << "Could not compute feasible rotation plan to parent,"
                     << " add myself to path" << endl;
            } else return;
        } else {
            cout << "I am an articulation point, I cannot move" << endl;
        }

        // If module not mobile or a movement path could not be found,
        //  then propagate search to children        
        PathHop& lastHop = path.back();

        std::vector<short> adjacentPathConnectors;
        std::map<short, short> mirrorConnector;
        cout << endl << "Adding module " << bc->catom->blockId << " to path" << endl;
        API::findAdjacentConnectors(bc->catom, lastHop,
                                    pivotDockingConnector, catomDockingConnector,
                                    adjacentPathConnectors, mirrorConnector);

        bool moduleCanBeHop = API::addModuleToPath(bc->catom, bc->path,
                                                   pivotDockingConnector,
                                                   catomDockingConnector);
        if (moduleCanBeHop) {
            bc->findMobileModule();
        } else {
            // Path to pathConnectors is blocked
            bc->sendMessage(new FindMobileModuleNotFoundMessage(),
                            bc->meltFather, 100, 0);
            // Prepare data structures for the mobile module search DFS
            bc->resetDFSFlags();
            bc->meltFather = NULL;
        }
    } else {
        // Module already in DFS tree
        bc->sendMessage(new FindMobileModuleIgnoreMessage(),
                        destinationInterface, 100, 0);
    }
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
    bc->findMobileModule();
}

void FindMobileModuleFoundMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    sender = static_cast<Catoms3DBlock*>(sourceInterface->hostBlock);

    bc->flag[destinationInterface] = true;
    
    if (!bc->source) {
        bc->sendMessage(new FindMobileModuleFoundMessage(),
                        bc->meltFather, 100, 0);
    } else {
        bc->catom->setColor(ORANGE);
    }
}

void FindMobileModuleBlockedMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    sender = static_cast<Catoms3DBlock*>(sourceInterface->hostBlock);
    
    if (!bc->source) {
        bc->sendMessage(new FindMobileModuleBlockedMessage(),
                        bc->meltFather, 100, 0);
    } else {
        bc->propagateGraphResetBFS();
    }
}
