
#include "findNextTCellMessages.hpp"

#include "../../api.hpp"

// #define DEBUG_FNTC

void FindNextTCellMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    short catomDockingConnector = bc->catom->getDirection(destinationInterface);
    short pivotDockingConnector = static_cast<Catoms3DBlock*>
        (sourceInterface->hostBlock)->getDirection(sourceInterface); // so-so

    if (!bc->growthVisited) { // Module has not been considered for path planning yet
        bc->growthParent = destinationInterface;

        bc->growthVisited = true;
        bc->flag[destinationInterface] = true;

        bc->path = this->path;
        P2PNetworkInterface* lastHopItf =
            bc->catom->getInterface(bc->path.back().position);
        assert(lastHopItf == destinationInterface);

        // If module not mobile or a movement path could not be found,
        //  then propagate search to children        
        PathHop& lastHop = bc->path.back();

        std::vector<short> adjacentPathConnectors;
        std::map<short, short> mirrorConnector;
#ifdef DEBUG_FNCT
        cout << endl << "Adding module " << bc->catom->blockId << " to path" << endl;
#endif
        API::findAdjacentConnectors(bc->catom, lastHop,
                                    pivotDockingConnector, catomDockingConnector,
                                    adjacentPathConnectors, mirrorConnector);

        bool moduleCanBeHopAndAddedToPath = API::addModuleToPath(bc->catom, bc->path,
                                                                 pivotDockingConnector,
                                                                 catomDockingConnector);
        PathHop& newHop = bc->path.back();
        bc->goalPosition = goalPosition;

#ifdef DEBUG_FNCT
        cout << bc->catom->blockId << " " << bc->catom->position
             <<"Searching for nearby target position "
             << goalPosition << endl;
#endif
#ifdef DEBUG_FNCT
        cout << "Reminder: " << endl << bc->rtg;
        cout << "Reminder2: " << endl << newHop << endl;
#endif

        
        for (const auto& pair : newHop.conDistanceMap) {
            Cell3DPosition gp; bc->catom->getNeighborPos(pair.first, gp);
#ifdef DEBUG_FNCT
            cout << "con " << pair.first
                 << " ==> " << gp << endl;
#endif
        }
        
        short targetCon = bc->catom->getConnectorId(bc->goalPosition);
#ifdef DEBUG_FNCT        
        if (targetCon != -1)
            cout << "found on " << targetCon << endl;
#endif
        targetCon = newHop.getDistance(targetCon) != -1 ? targetCon : -1;

        // bc->lattice->highlightCell(bc->catom->position, PINK);
        
        if (moduleCanBeHopAndAddedToPath) {
            if (targetCon != -1) {
                // Check if adjacent to target and Visit sub-tree otherwise
                bc->console << "target " << bc->goalPosition << " is on connector "
                            << targetCon << "\n";
                bc->console << "Path to " << bc->goalPosition << " found" << "\n";
                OUTPUT << "FROM PATH: " << bc->path;

                // Find next hop interface
                bc->path.pop_back(); // Retrieve previous hop by popping self
                OUTPUT << "AFTER POP PATH: " << bc->path;
                P2PNetworkInterface* nextHopItf = bc->catom->getInterface(bc->path.back().position);
                if (!nextHopItf) {
                    OUTPUT << "error: Need to send message to " << bc->path.back().position
                           << " but I can't find that guy" << endl;
                    bc->catom->setColor(WHITE);        
                    awaitKeyPressed();
                    assert(nextHopItf);
                }

            
                // Reset Path and build path back
                bc->path.clear();
                
                /// Add self as the first hop of the path
                API::addModuleToPath(bc->catom, bc->path, targetCon, targetCon);
            
                bc->sendMessage(new FindNextTCellSuccessMessage(bc->path),
                                nextHopItf, 100, 0);
                bc->catom->setColor(BLUE);

                return;
            } else {
                P2PNetworkInterface *unprocessedNeighbor =
                    bc->getNextUnprocessedInterface();

                if (unprocessedNeighbor) {
                    bc->sendMessage(new FindNextTCellMessage(bc->path, bc->goalPosition),
                                    unprocessedNeighbor, 100, 0);                
                    return;
                } 
            }
        }

        bc->sendMessage(new FindNextTCellFailureMessage(),
                        bc->growthParent, 100, 0);
        bc->catom->setColor(ORANGE);
        // bc->lattice->unhighlightCell(bc->catom->position);
            
        bc->growthParent = NULL;
        bc->growthVisited = false;
        bc->resetDFSFlags();
        bc->path.clear();
    } else { // Module has already been considered in this phase
        bc->sendMessage(new FindNextTCellIgnoreMessage(),
                        destinationInterface, 100, 0);
    }

}

void handleFindNextTCellResponse(BaseSimulator::BlockCode* bsbc,
                            P2PNetworkInterface* sender) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    bc->flag[sender] = true;
    
    P2PNetworkInterface *unprocessedNeighbor =
        bc->getNextUnprocessedInterface();

    if (unprocessedNeighbor) {
        bc->sendMessage(new FindNextTCellMessage(bc->path, bc->goalPosition),
                        unprocessedNeighbor, 100, 0);
    } else {
        if (bc->growthParent) {
            // bc->lattice->unhighlightCell(bc->catom->position);
            bc->sendMessage(new FindNextTCellFailureMessage(),
                            bc->growthParent, 100, 0);
            bc->growthVisited = false;
        } else {
            bc->catom->setColor(WHITE); //todo probleeeeem

            cout << "!!! growth failed: no path to target "
                 << bc->goalPosition << " found !!!" << endl;
            cout << bc->path;
            
            awaitKeyPressed();
            assert(false);
        } 
    }
}

void FindNextTCellIgnoreMessage::handle(BaseSimulator::BlockCode* bsbc) {
    handleFindNextTCellResponse(bsbc, destinationInterface);
}

void FindNextTCellFailureMessage::handle(BaseSimulator::BlockCode* bsbc) {
    handleFindNextTCellResponse(bsbc, destinationInterface);
}

void FindNextTCellSuccessMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    short catomDockingConnector = bc->catom->getDirection(destinationInterface);
    short pivotDockingConnector = static_cast<Catoms3DBlock*>
        (sourceInterface->hostBlock)->getDirection(sourceInterface); // so-so

    bc->flag[destinationInterface] = true;

    bc->console << bc->catom->position << " received path: " << path;
    bc->console << bc->catom->position << " my path: " << bc->path;
    // bc->lattice->unhighlightCell(bc->catom->position);
    
    if (bc->growthParent) {
        bc->path.pop_back(); // Retrieve previous hop by popping self
        P2PNetworkInterface* nextHopItf = bc->catom->getInterface(bc->path.back().position);
        if (!nextHopItf) {
            OUTPUT << "error: Need to send message to " << bc->path.back().position
                   << " but I can't find that guy" << endl;
            bc->catom->setColor(WHITE);        
            awaitKeyPressed();
            assert(nextHopItf);
        }

        // Add self as the next hop of the path
        bc->path = path;
        if (!API::addModuleToPath(bc->catom, bc->path,
                                  pivotDockingConnector, catomDockingConnector))
        {
            awaitKeyPressed();
            assert(false);
        }

        bc->sendMessage(new FindNextTCellSuccessMessage(bc->path),
                        nextHopItf, 100, 0);
        
        bc->growthVisited = false;
        bc->catom->setColor(GREEN); // todo
    } else { // isTail, moving module
        bc->path = this->path;
        if (!bc->computeNextRotation(bc->path)) {
#ifdef DEBUG_FNCT
            cout << "Could not compute feasible rotation plan to parent,"
                 << " add myself to path" << endl;
#endif
            bc->catom->setColor(RED);

            awaitKeyPressed();
            assert(false);
        } else {
            bc->growing = true;
            bc->growthVisited = true;
        }
    } 
}
