
#include "findPathMessages.hpp"

#include "teleportationEvents.h"

#include "../../api.hpp"

void FindPathMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    if (!bc->growthVisited) { // Module has not been considered for path planning yet
        if (!bc->growthParent) bc->growthParent = destinationInterface;

        bc->growthVisited = true;
        bc->goalPosition = goalPosition;
                
        bc->flag[destinationInterface] = true;        
        
        // targetCells.pop_front(); todo done globally for now
                
        // Check if adjacent to target and Visit sub-tree otherwise
        if (bc->lattice->cellsAreAdjacent(bc->catom->position, goalPosition)) {
            short targetCon = bc->catom->getConnectorId(goalPosition);
            bc->console << "target " << goalPosition << " is on connector "
                        << targetCon << "\n";
            
            
            // Add self as the next hop of the path
            bc->path.clear();
            API::addModuleToPath(bc->catom, bc->path, targetCon, targetCon);            
            
            bc->sendMessage(new FindPathFoundMessage(bc->path),
                            bc->growthParent, 100, 0);

            bc->console << "Path to " << goalPosition << " found" << "\n";
        } else {
            P2PNetworkInterface *unprocessedNeighbor =
                bc->getNextUnprocessedInterface();

            if (unprocessedNeighbor) {
                bc->sendMessage(new FindPathMessage(goalPosition),
                                unprocessedNeighbor, 100, 0);
            } else {
                bc->sendMessage(new FindPathNotFoundMessage(),
                                bc->growthParent, 100, 0);
            }

        }
                     
    } else { // Module has already been considered in this phase
        bc->sendMessage(new FindPathIgnoreMessage(),
                        destinationInterface, 100, 0);
    }

}

void FindPathFoundMessage::handle(BaseSimulator::BlockCode* bsbc) {
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
        assert(API::addModuleToPath(bc->catom, bc->path,
                                    pivotDockingConnector, catomDockingConnector));
        bc->sendMessage(new FindPathFoundMessage(bc->path),
                        bc->growthParent, 100, 0);
        
        bc->growthVisited = false;
        bc->catom->setColor(GREEN); // todo
    } else { // isTail, moving module        
        PathHop& lastHop = path.back();

        std::vector<short> adjacentPathConnectors;
        std::map<short, short> mirrorConnector;
        
        API::findAdjacentConnectors(bc->catom, lastHop,
                                    pivotDockingConnector, catomDockingConnector,
                                    adjacentPathConnectors, mirrorConnector);

        vector<Catoms3DMotionRulesLink*> mrl;
        API::getMotionRulesFromConnector(bc->catom, catomDockingConnector, mrl);
            
        bc->growthRotationsPlan.clear();
        API::findConnectorsPath(mrl, catomDockingConnector, adjacentPathConnectors,
                                bc->growthRotationsPlan);
            
        if (!bc->growthRotationsPlan.empty()) {
            bc->growing = true;
            bc->growthVisited = true;
            bc->console << " (" << bc->catom->position << ") "
                        << "Moving to " << bc->goalPosition << "!" << "\n";

            short dirTo = bc->growthRotationsPlan.back()->getConToID();
            lastHop.prune(mirrorConnector[dirTo]);
            // lastHop.removeConnectorOnPosition(bc->catom->position); 
                
            Catoms3DMotionRulesLink *nextRotation = bc->growthRotationsPlan.front();
            bc->pivotLinkConId = nextRotation->getConToID();
            bc->growthRotationsPlan.pop_front();

            Catoms3DBlock *pivot = bc->catom->getNeighborOnCell(lastHop.getPosition());
            assert(pivot);
            nextRotation->sendRotationEvent(bc->catom,
                                            pivot,
                                            getScheduler()->now() + 100);
            bc->path = this->path;
            return;
        } else {
            cout << "Could not compute feasible rotation plan to parent,"
                 << " add myself to path" << endl;
            bc->catom->setColor(RED);
            assert(false);
        }

        // bc->scheduler->schedule(new TeleportationStartEvent(bc->scheduler->now() + 150,
        //                                                     bc->catom, bc->goalPosition));
    } 
}

void handleFindPathResponse(BaseSimulator::BlockCode* bsbc,
                            P2PNetworkInterface* sender) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    P2PNetworkInterface *unprocessedNeighbor =
        bc->getNextUnprocessedInterface();

    bc->flag[sender] = true;

    if (unprocessedNeighbor) {
        bc->sendMessage(new FindPathMessage(bc->goalPosition),
                        unprocessedNeighbor, 100, 0);
    } else {
        if (bc->growthParent) {
            bc->sendMessage(new FindPathNotFoundMessage(),
                            bc->growthParent, 100, 0);
            bc->growthVisited = false;
        } else {
            bc->catom->setColor(WHITE); //todo probleeeeem
            cerr << "growth failed" << endl;
            assert(false);
        } 
    }
}

void FindPathIgnoreMessage::handle(BaseSimulator::BlockCode* bsbc) {
    handleFindPathResponse(bsbc, destinationInterface);
}

void FindPathNotFoundMessage::handle(BaseSimulator::BlockCode* bsbc) {
    handleFindPathResponse(bsbc, destinationInterface);
}

void GrowNextModuleMessage::handle(BaseSimulator::BlockCode* bsbc) {
    MeltSortGrowBlockCode *bc = static_cast<MeltSortGrowBlockCode*>(bsbc);

    if (bc->growthParent) {
        bc->resetDFSFlags();
                
        if (bc->growthParent->isConnected()) {
            bc->sendMessage(new GrowNextModuleMessage(),
                            bc->growthParent, 100, 0);
            bc->growthVisited = false;
        } else { // Current module should be new tail
            bc->moveToGoal();
        }
    } else {
        bc->console << "I'm very much sorry sir, but I do not have a growth parent." << "\n";
    }
}
