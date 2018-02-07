/*
 * findMobileModuleMessages.cpp
 *
 *  Created on: 11/01/2018
 *      Author: pthalamy
 */

#include "findMobileModuleMessages.hpp"
#include "../../api.hpp"

FindMobileModuleMessage::FindMobileModuleMessage(vector<PathHop> _path)
    : path(_path) {}

void FindMobileModuleMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    sender = static_cast<Catoms3DBlock*>(sourceInterface->hostBlock);
    short catomDockingConnector = bc->catom->getDirection(destinationInterface);
    short pivotDockingConnector = sender->getDirection(sourceInterface); // so-so
    
    if (!bc->meltFather) {
        bc->meltFather = destinationInterface;
        bc->resetDFSFlags();

        // List all connectors that could be filled in order to connect
        //  a neighbor module to the last hop or that would help reach parent's path connectors
        PathHop& lastHop = path.back();

        std::vector<short> adjacentPathConnectors;
        std::map<short, short> mirrorConnector;
        cout << endl << "Adding module " << bc->catom->blockId << " to path" << endl;
        API::findAdjacentConnectors(bc->catom, lastHop,
                                    pivotDockingConnector, catomDockingConnector,
                                    adjacentPathConnectors, mirrorConnector);
        
        if (!bc->articulationPoint && !bc->melted) {
            vector<Catoms3DMotionRulesLink*> mrl;
            API::getMotionRulesFromConnector(bc->catom, catomDockingConnector, mrl);
            
            // Mobile Module:
            // Check if module can move to any of the adjacent path connectors of pivot.
            // Input connector set is ordered by distance to target, so the search should stop
            // as soon as a solution has been found
            bc->meltRotationsPlan.clear();
            API::findConnectorsPath(mrl, catomDockingConnector, adjacentPathConnectors,
                                    bc->meltRotationsPlan);
            
            if (!bc->meltRotationsPlan.empty()) {
                short dirTo = bc->meltRotationsPlan.back()->getConToID();
                lastHop.prune(mirrorConnector[dirTo]);
                // lastHop.removeConnectorOnPosition(bc->catom->position); 

                // Notify father we are moving
                // Path to pathConnectors is blocked
                bc->sendMessage("FindMobileModuleFound",
                                new FindMobileModuleFoundMessage(),
                                bc->meltFather, 100, 0);

                
                Catoms3DMotionRulesLink *nextRotation = bc->meltRotationsPlan.front();
                bc->pivotLinkConId = nextRotation->getConToID();
                bc->meltRotationsPlan.pop_front();
                nextRotation->sendRotationEvent(bc->catom,
                                                bc->catom->getNeighborOnCell(lastHop.getPosition()),
                                                getScheduler()->now() + 100);
                bc->path = this->path;
                return;
            } else {
                cout << "Could not compute feasible rotation plan to parent, add myself to path" << endl;
            }
        } else {
            cout << "I am an articulation point, I cannot move" << endl;
        }

        // If module not mobile or a movement path could not be found,
        //  then propagate search to children
        bc->path = this->path;
        bool moduleCanBeHop = API::addModuleToPath(bc->catom, bc->path,
                                                   pivotDockingConnector,
                                                   catomDockingConnector);
        if (moduleCanBeHop) {
            bc->findMobileModule();
        } else {
            // Path to pathConnectors is blocked
            bc->sendMessage("FindMobileModuleNotFound",
                            new FindMobileModuleNotFoundMessage(),
                            bc->meltFather, 100, 0);
            // Prepare data structures for the mobile module search DFS
            bc->resetDFSFlags();
            bc->meltFather = NULL;
        }
    } else {
        // Module already in DFS tree
        bc->sendMessage("FindMobileModuleIgnore",
                        new FindMobileModuleIgnoreMessage(),
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
        bc->sendMessage("FindMobileModuleFound",
                        new FindMobileModuleFoundMessage(),
                        bc->meltFather, 100, 0);
    } else {
        bc->catom->setColor(ORANGE);
    }

    // Prepare data structures for the mobile module search DFS
    bc->resetDFSFlags();
    bc->meltFather = NULL;
}
