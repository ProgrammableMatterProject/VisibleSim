
#include "buildPathMessages.hpp"

#include "../../api.hpp"

void BuildPathMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);
    short catomDockingConnector = bc->catom->getDirection(destinationInterface);
    short pivotDockingConnector = static_cast<Catoms3DBlock*>
        (sourceInterface->hostBlock)->getDirection(sourceInterface); // so-so

    if (!bc->growthVisited) { // Module has not been considered for path planning yet
        if (!bc->growthParent) bc->growthParent = destinationInterface;

        bc->growthVisited = true;
        // bc->goalPosition = goalPosition;                
        bc->flag[destinationInterface] = true;        

        bc->path = this->path;

        // If module not mobile or a movement path could not be found,
        //  then propagate search to children        
        PathHop& lastHop = bc->path.back();

        std::vector<short> adjacentPathConnectors;
        std::map<short, short> mirrorConnector;
        cout << endl << "Adding module " << bc->catom->blockId << " to path" << endl;
        API::findAdjacentConnectors(bc->catom, lastHop,
                                    pivotDockingConnector, catomDockingConnector,
                                    adjacentPathConnectors, mirrorConnector);

        bool moduleCanBeHopAndAddedToPath = API::addModuleToPath(bc->catom, bc->path,
                                                                 pivotDockingConnector,
                                                                 catomDockingConnector);
        PathHop& newHop = bc->path.back();

        cout << "Searching for nearby target position "
             << bc->catom->position << endl;
        cout << "Reminder: " << endl << bc->rtg;
            
        short targetCon = -1; Cell3DPosition goalPosition;
        std::vector<short> hopCons;
        newHop.getConnectorsByIncreasingDistance(hopCons);        
        for (short con : hopCons) {
            bc->catom->getNeighborPos(con, goalPosition);
            cout << "candidate (" << con << ") " << goalPosition << endl;
            if (bc->rtg->isInTarget(goalPosition)) {
                targetCon = con;
                break;
            }
        }
        
        if (moduleCanBeHopAndAddedToPath && targetCon != -1) {
            // Check if adjacent to target and Visit sub-tree otherwise
            bc->console << "target " << goalPosition << " is on connector "
                        << targetCon << "\n";
            bc->lattice->highlightCell(goalPosition, BLUE);
            
            // Reset Path and build path back
            bc->path.clear();
                
            /// Add self as the first hop of the path
            API::addModuleToPath(bc->catom, bc->path, targetCon, targetCon);
            
            bc->sendMessage(new BuildPathSuccessMessage(bc->path, goalPosition),
                            bc->growthParent, MSG_DELAY, 0);

            bc->console << "Path to " << goalPosition << " found" << "\n";
            return;
        } else {
            P2PNetworkInterface *unprocessedNeighbor =
                bc->getNextUnprocessedInterface();

            if (unprocessedNeighbor) {
                bc->sendMessage(new BuildPathMessage(bc->path),
                                unprocessedNeighbor, MSG_DELAY, 0);
                return;
            } 
        }

        bc->sendMessage(new BuildPathFailureMessage(),
                        bc->growthParent, MSG_DELAY, 0);
        bc->growthParent = NULL;
        bc->resetDFSFlags();
    } else { // Module has already been considered in this phase
        bc->sendMessage(new BuildPathIgnoreMessage(),
                        destinationInterface, MSG_DELAY, 0);
    }

}

void handleBuildPathResponse(BaseSimulator::BlockCode* bsbc,
                            P2PNetworkInterface* sender) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    P2PNetworkInterface *unprocessedNeighbor =
        bc->getNextUnprocessedInterface();

    bc->flag[sender] = true;

    if (unprocessedNeighbor) {
        bc->sendMessage(new BuildPathMessage(bc->path),
                        unprocessedNeighbor, MSG_DELAY, 0);
    } else {
        if (bc->growthParent) {
            bc->sendMessage(new BuildPathFailureMessage(),
                            bc->growthParent, MSG_DELAY, 0);
            bc->growthVisited = false;
        } else {
            bc->catom->setColor(WHITE); //todo probleeeeem

            cerr << "growth failed: no path to target "
                 << bc->goalPosition << " found" << endl;
            awaitKeyPressed();
            assert(false);
        } 
    }
}

void BuildPathIgnoreMessage::handle(BaseSimulator::BlockCode* bsbc) {
    handleBuildPathResponse(bsbc, destinationInterface);
}

void BuildPathFailureMessage::handle(BaseSimulator::BlockCode* bsbc) {
    handleBuildPathResponse(bsbc, destinationInterface);
}

void BuildPathSuccessMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    short catomDockingConnector = bc->catom->getDirection(destinationInterface);
    short pivotDockingConnector = static_cast<Catoms3DBlock*>
        (sourceInterface->hostBlock)->getDirection(sourceInterface); // so-so

    bc->flag[destinationInterface] = true;

    bc->console << bc->catom->position << " received path: " << "\n";
    for (auto p : path) OUTPUT << p << endl;
    
    if (bc->growthParent) {
        // Add self as the next hop of the path
        bc->path = path;
        if (!API::addModuleToPath(bc->catom, bc->path,
                                  pivotDockingConnector, catomDockingConnector))
        {
            awaitKeyPressed();
            assert(false);
        }
        
        bc->sendMessage(new BuildPathSuccessMessage(bc->path, goalPosition),
                        bc->growthParent, MSG_DELAY, 0);
        
        bc->growthVisited = false;
        bc->catom->setColor(GREEN); // todo
    } else { // isTail, moving module
        bc->goalPosition = goalPosition;
        bc->path = this->path;
        if (!bc->computeNextRotation(bc->path)) {
            cout << "Could not compute feasible rotation plan to parent,"
                 << " add myself to path" << endl;
            bc->catom->setColor(RED);

            awaitKeyPressed();
            assert(false);
        } else {
            bc->growing = true;
            bc->growthVisited = true;
        }
    } 
}
