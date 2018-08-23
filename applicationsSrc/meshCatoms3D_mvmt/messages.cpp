/**
 * @file   messages.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 14:13:13 2018
 * 
 * @brief  
 * 
 * 
 */

#include <iostream>
#include <sstream>

#include "utils.h"
#include "messages.hpp"

#include "meshCatoms3DBlockCode.hpp" //FIXME:

void Catoms3DMotionEngineMessage::handle(BaseSimulator::BlockCode* bc) {
    handle(*static_cast<MeshCatoms3DBlockCode*>(bc)->engine);
}

// END FIXME:

static bool COLORIZE_PATH_SEARCH = false;
const Color visitedColor = ORANGE;

void FindPathMessage::handle(Catoms3DMotionEngine& engine) {   
    if (!engine.visited) { // Module has not been considered for path planning yet
        if (!engine.searchParent) engine.searchParent = destinationInterface;

        engine.visited = true;
        engine.goalPosition = destination;
        engine.flag[destinationInterface] = true;        

        if (COLORIZE_PATH_SEARCH)
            engine.catom.setColor(visitedColor);
        
        // Check if adjacent to target and Visit sub-tree otherwise
        if (BaseSimulator::getWorld()->lattice->
            cellsAreAdjacent(engine.catom.position, destination)) {
            short targetCon = engine.catom.getConnectorId(destination);
            // engine.console << "target " << destination << " is on connector "
            //             << targetCon << "\n";
            
            
            // Add self as the next hop of the path
            engine.path.clear();
            engine.addModuleToPath(engine.catom, engine.path, targetCon, targetCon);
            
            engine.bc.sendMessage(new FindPathFoundMessage(engine.path),
                                  engine.searchParent, MSG_DELAY, 0);

            // engine.console << "Path to " << destination << " found" << "\n";
        } else {
            P2PNetworkInterface *unprocessedNeighbor =
                engine.getNextUnprocessedInterface();

            if (unprocessedNeighbor) {
                engine.bc.sendMessage(new FindPathMessage(destination),
                                      unprocessedNeighbor, MSG_DELAY, 0);
            } else {
                engine.bc.sendMessage(new FindPathNotFoundMessage(),
                                      engine.searchParent, MSG_DELAY, 0);
            }

        }
                     
    } else { // Module has already been considered in this phase
        engine.bc.sendMessage(new FindPathIgnoreMessage(),
                              destinationInterface, MSG_DELAY, 0);
    }

}

void FindPathFoundMessage::handle(Catoms3DMotionEngine& engine) {
    short catomDockingConnector =
        engine.catom.getDirection(destinationInterface);
    short pivotDockingConnector = static_cast<Catoms3DBlock*>
        (sourceInterface->hostBlock)->getDirection(sourceInterface); // so-so

    engine.flag[destinationInterface] = true;

    // engine.console << engine.catom.position << " received path: " << "\n";
    // for (auto p : path) OUTPUT << p << endl;
    
    if (engine.searchParent) {
        // Add self as the next hop of the path
        engine.path = path;
        if (!engine.addModuleToPath(engine.catom, engine.path,
                                    pivotDockingConnector, catomDockingConnector))
        {
            for (const auto& hop : path)
                cout << hop << endl;
            engine.catom.setColor(PINK);
            VS_ASSERT_MSG(false, "test");
        }
        
        engine.bc.sendMessage(new FindPathFoundMessage(engine.path),
                              engine.searchParent, MSG_DELAY, 0);
        
        engine.visited = false;
        engine.catom.setColor(GREEN); // todo
    } else { // isTail, moving module        
        engine.path = this->path;
        if (!engine.computeNextRotation(engine.path)) {
            cout << "Could not compute feasible rotation plan to parent,"
                 << " add myself to path" << endl;
            engine.catom.setColor(RED);

            awaitKeyPressed();
            assert(false);
        } else {
            // engine.growing = true;
            engine.visited = true;
        }
    } 
}

void handleFindPathResponse(Catoms3DMotionEngine& engine,
                            P2PNetworkInterface* sender) {
    P2PNetworkInterface *unprocessedNeighbor =
        engine.getNextUnprocessedInterface();

    engine.flag[sender] = true;

    if (unprocessedNeighbor) {
        engine.bc.sendMessage(new FindPathMessage(engine.goalPosition),
                              unprocessedNeighbor, MSG_DELAY, 0);
    } else {
        if (engine.searchParent) {
            engine.bc.sendMessage(new FindPathNotFoundMessage(),
                                  engine.searchParent, MSG_DELAY, 0);
            engine.visited = false;
        } else {
            engine.catom.setColor(WHITE); //todo probleeeeem

            cerr << "search failed" << endl;
            awaitKeyPressed();
            assert(false);
        } 
    }
}

void FindPathIgnoreMessage::handle(Catoms3DMotionEngine& engine) {
    handleFindPathResponse(engine, destinationInterface);
}

void FindPathNotFoundMessage::handle(Catoms3DMotionEngine& engine) {
    handleFindPathResponse(engine, destinationInterface);
}


AbstractMeshSpanningTreeMessage*
DisassemblyTriggerMessage::buildNewMeshSpanningTreeMessage(BaseSimulator::BlockCode& bc,
                                                           const bool isAck) {
    MeshCatoms3DBlockCode& mcbc = static_cast<MeshCatoms3DBlockCode&>(bc);
    return new DisassemblyTriggerMessage(*mcbc.ruleMatcher, isAck);
}

void DisassemblyTriggerMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshCatoms3DBlockCode& mcbc = *static_cast<MeshCatoms3DBlockCode*>(bc);    
    
    if (not --mcbc.numberExpectedAcksFromSubTree) {
        mcbc.catom->setColor(GREEN);
        const Cell3DPosition& parentPos =
            ruleMatcher.getTreeParentPosition(mcbc.catom->position);

        if (parentPos != mcbc.catom->position) {
            if (not mcbc.target->isInTarget(mcbc.catom->position)) {
                // mcbc.catom->setColor(WHITE);
                mcbc.catom->setVisible(false);
                // mcbc.world->deleteBlock(mcbc.catom);
            } else {
                mcbc.catom->setColor(mcbc.target->getTargetColor(mcbc.catom->position));
            }
            
            P2PNetworkInterface* parentItf = mcbc.catom->getInterface(parentPos);
            assert(parentItf->isConnected());
            acknowledgeToParent(mcbc, parentItf);
        } else {
            BaseSimulator::getWorld()->lattice->highlightCell(mcbc.catom->position, BLUE);
            cout << "-- Shape disassembly into CSG object done." << endl;
        }
    }        
}

AbstractMeshSpanningTreeMessage* SubTreeScaffoldConstructionDoneMessage::
buildNewMeshSpanningTreeMessage(BaseSimulator::BlockCode& bc, const bool isAck) {
    return new SubTreeScaffoldConstructionDoneMessage(ruleMatcher, isAck);
}

void SubTreeScaffoldConstructionDoneMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshCatoms3DBlockCode& mcbc = *static_cast<MeshCatoms3DBlockCode*>(bc);    
    
    // const Cell3DPosition &debugCell = Cell3DPosition(6,0,6);
    // if (mcbc.catom->position == debugCell) {
    //   mcbc.world->lattice->highlightCell(debugCell, RED);
    //   ruleMatcher.printDebugInfo(debugCell);
    //   awaitKeyPressed();
    // }
    
    if (not --mcbc.numberExpectedAcksFromSubTree) {        
        mcbc.catom->setColor(RED);

        const Cell3DPosition& parentPos =
            ruleMatcher.getTreeParentPosition(mcbc.catom->position);

        if (parentPos != mcbc.catom->position) {
            P2PNetworkInterface* parentItf = mcbc.catom->getInterface(parentPos);
            assert(parentItf->isConnected());
            acknowledgeToParent(mcbc, parentItf);
        } else {
            BaseSimulator::getWorld()->lattice->highlightCell(mcbc.catom->position, BLUE);
			cout << "-- Scaffold construction done." << endl;
            mcbc.triggerMeshTraversalProcess();
        }
    }
}

