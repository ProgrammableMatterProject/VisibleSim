/**
 * @file   messages.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 14:13:13 2018
 * 
 * @brief  
 * 
 * 
 */

#include "messages.hpp"

#include "meshCatoms3DBlockCode_mvmt.hpp" //FIXME:

void Catoms3DMotionEngineMessage::handle(BaseSimulator::BlockCode* bc) {
    handle(*static_cast<MeshCatoms3DBlockCode*>(bc)->engine);
}

// END FIXME:

void FindPathMessage::handle(Catoms3DMotionEngine& engine) {   
    if (!engine.visited) { // Module has not been considered for path planning yet
        if (!engine.searchParent) engine.searchParent = destinationInterface;

        engine.visited = true;
        engine.posToLocate = destination;
        engine.flag[destinationInterface] = true;        
                        
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
            awaitKeyPressed();
            assert(false);
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
        engine.bc.sendMessage(new FindPathMessage(engine.posToLocate),
                               unprocessedNeighbor, MSG_DELAY, 0);
    } else {
        if (engine.searchParent) {
            engine.bc.sendMessage(new FindPathNotFoundMessage(),
                                   engine.searchParent, MSG_DELAY, 0);
            engine.visited = false;
        } else {
            engine.catom.setColor(WHITE); //todo probleeeeem

            cerr << "growth failed" << endl;
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

bool isMeshRoot(const Cell3DPosition& pos, const uint B) {
    return pos.pt[0] % B == 0 and pos.pt[1] % B == 0 and pos.pt[2] % B == 0;
}

bool upwardBranchRulesApply(const Cell3DPosition& own,
                            const Cell3DPosition& other,
                            const uint B) {
    const int zCoeff = other.pt[2] / B;
    // Module is on branch if z is NOT a multiple of B
    return not isMeshRoot(own, B) and own.pt[2] % B != 0
        and (
            // In that case, only allow upward transmission
            own.pt[2] < other.pt[2]
            // Unless neighbor is not on branch
            and (other.pt[2] % B != 0
                 // Transmitting to the next root only along z axis from (0,0,0)
                 or (isMeshRoot(other, B)
                     and (
                         // (IS_EVEN(zCoeff)
                         //// If o.z / B even, then we need to go up/forward
                         //  and other.pt[0] == (int)(1-zCoeff * B)
                         //  and other.pt[1] == (int)(1-zCoeff * B)
                         //  and other - own == Cell3DPosition(0, 0, 1))
                         (IS_ODD(zCoeff)
                          // if o.z / B odd, then we need to go up/backward
                          and other.pt[0] == (int)(-(zCoeff / 2 * B))
                          and other.pt[0] == other.pt[1]
                          and other - own == Cell3DPosition(-1, -1, 1))
                         )
                     )
                )
            );            
}

bool planarBranchRulesApply(const Cell3DPosition& own,
                            const Cell3DPosition& other,
                            const uint B) {
    // Module is on plan if z is a multiple of B
    return own.pt[2] % B == 0 and own.pt[2] == other.pt[2]
        and (
            // In that case, only allow transmission to increasing x...
            // (except if other is a root as transmission to roots occur along y axis,
            //   and we are not on lower border)
            (own.pt[0] < other.pt[0] and
             (not isMeshRoot(other, B) or own.pt[1] / B == 0))
            // ... or increasing y.
            or own.pt[1] < other.pt[1]
            );
}

bool meshRootBranchRulesApply(const Cell3DPosition& own,
                              const Cell3DPosition& other,
                              const uint B) {
    // Mesh root is responsible for upward propagation
    return isMeshRoot(own, B)
        and (
            // In that case, only allow transmission to increasing z, in all directions
            own.pt[2] < other.pt[2]
            );
}

void SpanningTreeAMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshCatoms3DBlockCode& mcbc = *static_cast<MeshCatoms3DBlockCode*>(bc);    
    
    if (!mcbc.stParent) {
        mcbc.stParent = destinationInterface;
        mcbc.catom->setColor(BLUE);
    } else {
        cout << mcbc.catom->blockId << endl;
        awaitKeyPressed();
        assert(!mcbc.stParent);
    }

    for (const Cell3DPosition& pos :
             mcbc.lattice->getActiveNeighborCells(mcbc.catom->position)) {
        
        P2PNetworkInterface* itf = mcbc.catom->getInterface(pos);
        assert (itf != NULL);


        const Cell3DPosition& myPos = mcbc.catom->position;
        if (mcbc.moduleInSpanningTree(pos)
            and (
                planarBranchRulesApply(myPos, pos, mcbc.B)
                or meshRootBranchRulesApply(myPos, pos, mcbc.B)
                or upwardBranchRulesApply(myPos, pos, mcbc.B)
                )
// ( (myPos.pt[2] < pos.pt[2]
            //    and (mcbc.isMeshRoot(myPos)
            //         or (myPos.pt[2] % mcbc.B != 0
            //             and (pos.pt[2] % mcbc.B != 0 or mcbc.isMeshRoot(pos)) )))
            //   or (myPos.pt[2] == pos.pt[2] and (myPos.pt[1] < pos.pt[1]
            //                                     or myPos.pt[0] < pos.pt[0]))
            // )
            
            and itf != mcbc.stParent) {
            mcbc.sendMessage("Spanning Tree A",
                             new SpanningTreeAMessage(),
                             itf, MSG_DELAY_MC, 0);
            mcbc.expectedConfirms++;
        }
    }

    if (not mcbc.expectedConfirms)
        mcbc.sendMessage("Spanning Tree B",
                    new SpanningTreeBMessage(),
                    mcbc.stParent, MSG_DELAY_MC, 0);        
}

void SpanningTreeBMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshCatoms3DBlockCode& mcbc = *static_cast<MeshCatoms3DBlockCode*>(bc);

    if (not --mcbc.expectedConfirms) {
        mcbc.catom->setColor(PINK);

        if (mcbc.stParent) {
            mcbc.sendMessage("Spanning Tree B",
                    new SpanningTreeBMessage(),
                        mcbc.stParent, MSG_DELAY_MC, 0);
        }
    }
}
