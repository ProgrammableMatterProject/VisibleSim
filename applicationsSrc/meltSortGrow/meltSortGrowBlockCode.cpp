/*
 * meltSortGrowBlockCode.cpp
 *
 *  Created on: 06/11/17
 *      Author: pthalamy
 */

#include <iostream>
#include <sstream>
#include <memory>
#include <algorithm>
#include <unordered_set>

#include "catoms3DWorld.h"
#include "scheduler.h"
#include "events.h"
#include "trace.h"
#include "teleportationEvents.h"

#include "meltSortGrowBlockCode.hpp"
#include "Messages/Init/rootInitializationMessages.hpp"
#include "Messages/Melt/APLabellingMessages.hpp"
#include "Messages/Melt/graphResetMessages.hpp"
#include "Messages/Melt/findMobileModuleMessages.hpp"
#include "Messages/Growth/findPathMessages.hpp"
#include "Messages/Growth/buildPathMessages.hpp"
#include "api.hpp"

using namespace Catoms3D;

MeltSortGrowBlockCode::MeltSortGrowBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    scheduler = getScheduler();
    catom = (Catoms3DBlock*)hostBlock;
}

MeltSortGrowBlockCode::~MeltSortGrowBlockCode() {}

void MeltSortGrowBlockCode::startup() {
    stringstream info;

    info << "Starting ";

    rtg = (RelativeTargetGrid*)target;	
	
    determineRoot();
    APLabellingInitialization();
}

void MeltSortGrowBlockCode::resetDFSForLabelling() {
    neighbors.clear();
    flag.clear();
    sons.clear();
    
    for (P2PNetworkInterface *itf : catom->getP2PNetworkInterfaces()) {
        if (itf->isConnected()) {
            flag[itf] = false;
            neighbors.push_back(itf);
        }
    }

    dfn = 0;
    lDfn = 0;
    dfnCnt = 0;
    minDfn = INT_MAX;
    bridge = false;
    articulationPoint = false;
    father = NULL;
    minSdr = NULL;
    source = false;
    state = APLState::Inactive;
    
    // Next APL Reset
    resetFather = NULL;
    meltFather = NULL;
}

void MeltSortGrowBlockCode::resetDFSForSearching() {
    flag.clear();
    
    for (P2PNetworkInterface *itf : sons) {
        flag[itf] = false; // Reset DFS markers
    }
}

void MeltSortGrowBlockCode::resetDFSFlags() {
    flag.clear();
    neighbors.clear();
    
    for (P2PNetworkInterface *itf : catom->getP2PNetworkInterfaces()) {
        if (itf->isConnected() && itf != growthParent && itf != meltFather) {
            flag[itf] = false;
            neighbors.push_back(itf);
        }
    }
}

void MeltSortGrowBlockCode::APLabellingInitialization() {
    resetDFSForLabelling();    
}

void MeltSortGrowBlockCode::APLabellingStart() {
    source = true;
    catom->setColor(RED);
        
    if (state == APLState::Inactive) {
        state = APLState::Active;
        lDfn = dfn = dfnCnt = 1;
        APLabellingSearch();
                
        // send VISITED(DFN(i)) to all nodes in (Neighbors(i) - Sons(i))
        for (auto const& module : neighbors) {
            if (!sons.count(module))
                sendMessage(new APLabellingVisitedMessage(dfn),
                            module, 100, 0);
        }
    }
}


void MeltSortGrowBlockCode::APLabellingSearch() {
    P2PNetworkInterface *unprocessedNeighbor = getNextUnprocessedInterface();
    
    if (unprocessedNeighbor) {
        // Send TOKEN(DFNcnt(i) + 1) to k
        sendMessage(new APLabellingTokenMessage(dfnCnt + 1),
                    unprocessedNeighbor, 100, 0);
        sons.insert(unprocessedNeighbor);
    } else if (source) { /* root checks for articulation point and terminates */
        if (sons.size() >= 2) articulationPoint = true;
        
        initializeMeltPath();
       
        // Prepare data structures for the mobile module search DFS
        resetDFSForSearching();

        findMobileModule();        
    } else { /* a normal node has finished the visits to its subtree */
        if (minDfn < lDfn && minSdr != father)
            lDfn = minDfn; /* sets to the smaller min. DFN from ancestors */            

        if (lDfn == dfn)
            bridge = true;

        // send ECHO(L(i), DFNcnt(i)) to Father(i)
        sendMessage(new APLabellingEchoMessage(lDfn, dfnCnt),
                    father, 100, 0);
    }
    
}

void MeltSortGrowBlockCode::initializeMeltPath() {    
    // Set next position to be filled
    Cell3DPosition tailPosition = catom->position - Cell3DPosition(1, 0, 0);
    assert(lattice->isInGrid(tailPosition)); // If not, increase grid size in XML
    
    short tailConId = catom->getConnectorId(tailPosition);
    cout << "Tail is " << tailConId << endl;
    lattice->highlightCell(tailPosition, RED);

    // Add self as the next hop of the path
    API::addModuleToPath(catom, path, tailConId, tailConId);
}

void MeltSortGrowBlockCode::findMobileModule() {    
    P2PNetworkInterface *unprocessedNeighbor = getNextUnprocessedInterface();
    
    if (unprocessedNeighbor) {
        sendMessage(new FindMobileModuleMessage(path),
                    unprocessedNeighbor, 100, 0);
    } else if (source) {
        // No more module paths to explore
        lattice->unhighlightCell(catom->position - Cell3DPosition(1, 0, 0));
        catom->setColor(BLUE); // #TOREMOVE todo
        grow();
    } else {
        sendMessage(new FindMobileModuleNotFoundMessage(),
                    meltFather, 100, 0);
        resetDFSFlags();
    }
}

void MeltSortGrowBlockCode::propagateGraphResetBFS() {
    resetChildrenDecount = 0;
    for (P2PNetworkInterface *nghbr : catom->getP2PNetworkInterfaces()) {
        if (nghbr->connectedInterface && nghbr != resetFather) {
            sendMessage(new ResetGraphMessage(),
                        nghbr, 100, 0);
            resetChildrenDecount++;
        }
    }

    if (!resetChildrenDecount && resetFather) { // No children, return 
        sendMessage(new ResetGraphDoneMessage(),
                    resetFather, 100, 0);
        resetDFSForLabelling();
    }
}

void MeltSortGrowBlockCode::moveToGoal() {
    growthParent = NULL;

    // std::list<Cell3DPosition>* targetCellsInConstructionOrder =
    //     rtg->getTargetCellsInConstructionOrder();    
        
    // if (!targetCellsInConstructionOrder->empty()) {
    //     cout << " New growth target position: " << goalPosition << endl;

    if (!rtg->reconfigurationIsComplete()) {
        // Here we would normally have to DFS-send FINDPATH until we reach
        //  goalPosition through path positions (surface movements)
        // For now, teleporting there right away is sufficient.

        resetDFSFlags();

        P2PNetworkInterface *unprocessedNeighbor = getNextUnprocessedInterface();
    
        if (unprocessedNeighbor) {
            // sendMessage(new FindPathMessage(goalPosition),
            //             unprocessedNeighbor, 100, 0);
            
            sendMessage(new BuildPathMessage(path),
                        unprocessedNeighbor, 100, 0);
        } else {
            console << "Houston we have a problem." << "\n";
            catom->setColor(BLACK); // todo
            assert(false);
        }
    }else {        
        // Growth is over. Yee-pee
        catom->setColor(BLACK);
    }              
}

void MeltSortGrowBlockCode::processReceivedMessage(MessagePtr msg,
                                                   P2PNetworkInterface *sender) {
    stringstream info;

    switch (msg->type) {
        // ALL MOVED TO HANDLEABLE MESSAGES
        default:
            cout << "Unknown Generic Message Type" << endl;
            assert(false);
            break;
    }

}

void MeltSortGrowBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;
	
    switch (pev->eventType) {
        case EVENT_RECEIVE_MESSAGE: {
            message =
                (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;

            if (message->isMessageHandleable()) {
                (std::static_pointer_cast<HandleableMessage>(message))->handle(this);
            } else {
                P2PNetworkInterface * recv_interface = message->destinationInterface;
            
                // Handover to global message handler
                processReceivedMessage(message, recv_interface);
            }
        } break;

        case EVENT_ROTATION3D_END: {            
            assert(!nextRotation);
            assert(!path.empty());

            if (!growing) {
                // Case 1: This is not the final hop
                // Case 1.1: Catom can skip some hops
                trimPath(path);
                bool meltIsOver = tryNextMeltRotation(path);

                if (meltIsOver && path.empty()) {
                    cout << "Final hop is empty, melt should be done" << endl;

                    melted = true;
                    lattice->unhighlightCell(catom->position);
                    
                    propagateGraphResetBFS();
                } else if (meltIsOver && !path.empty()) {
                    cout << "MELT: MODULE IS STUCK!" << endl;

                    PathHop& lastHop = path.back(); 
                    P2PNetworkInterface* pathHopInterface =
                        catom->getInterface(lastHop.getPosition());
                    assert(pathHopInterface);
                    
                    sendMessage(new FindMobileModuleBlockedMessage(),
                                pathHopInterface, 100, 0);        
                }
            } else {
                if (catom->position == goalPosition) {
                    growing = false;
                    growthVisited = false;
                    meltFather = NULL;
                    resetDFSFlags();

                    cout << catom->blockId << " has reached target position "
                         << goalPosition << endl;
                    lattice->unhighlightCell(goalPosition);
                    rtg->removeTargetCell(goalPosition);

                    // Send to single neigbor which will follow the message
                    //  route back to the new tail
                    assert(neighbors.size());

                    PathHop& finalHop = path.back(); 
                    P2PNetworkInterface* growthParent =
                        catom->getInterface(finalHop.getPosition());
                    assert(growthParent != NULL);
                    // growthParent = neighbors.front();
                    sendMessage(new GrowNextModuleMessage(),
                                growthParent, 100, 0);
                } else {
                    // Perform next move towards current growth target
                    // Case 1: This is not the final hop
                    // Case 1.1: Catom can skip some hops
                    // TODO: REFACTOR
                    trimPath(path);
                    assert(!tryNextGrowthRotation(path));
                }
            }
        } break;
            
        case EVENT_TAP: {
            for (const auto &pair : flag)
                cout << pair.first->connectedInterface->hostBlock->blockId << endl;
        } break;
    }
}

void MeltSortGrowBlockCode::trimPath(list<PathHop>& path) {
    for (auto it = path.begin(); it != path.end(); it = std::next(it)) {
        PathHop& someHop = *(it);
 
        // Next next hop is within reach, clear current hop
        if (someHop.isInVicinityOf(catom->position)
            || someHop.catomIsAlreadyOnBestConnector(catom->position)) {
            OUTPUT << "Skipping until: " << *it << endl;
            path.erase(++it, path.end());

            OUTPUT << "Updated path: " << endl;
            for (auto hop:path) OUTPUT << hop << endl;
                            
            break;
        } 
    }
}

/**
 * @brief Compares the fitness of the candidate root in argument and returns whether 
 it is fitter than current root
 * @param candidateRoot the position of the root to consider as new root
 * @return true if candidateRoot is fitter than current root, false otherwise */
bool MeltSortGrowBlockCode::challengeRootFitness(Cell3DPosition& candidateRoot) {
    // return candidateRoot < currentRootPosition;

    return Cell3DPosition::compare_ZYX(candidateRoot, currentRootPosition);
}

// Locate the root of the algorithm
// i.e., find the left-most module in the whole ensemble (perhaps extend to lowest-leftmost for 3D)
void MeltSortGrowBlockCode::determineRoot() {
    // Every node keeps its currently known candidate root in a variable
    //  and updates it when it finds a better potential root for (min(z) & min(y) & min(x) in this order)
    //  when the root is updated, the block broadcasts the new root, and then expects an ack from all of its neighbors
    currentRootPosition = catom->position;

    parent = NULL;
    expectedConfirms = catom->getNbNeighbors();
    
    // Send a message to every neighbor that includes the block's own location, as a potential root
    sendMessageToAllNeighbors("RootUpdate",
                              new RootUpdateMessage(currentRootPosition),
                              0, 100, 0);
}

// Initiates the Melt phase of the algorithm by the root node
//  i.e., decomposition into an intermediate shape
void MeltSortGrowBlockCode::meltOneModule() {
    melted = true;
    APLabellingStart();
}

// Initiates the Sort phase of the algorithm
void MeltSortGrowBlockCode::sort() {
    // Nothing to be done when using homogeneous modules
    return;
}

// Initiates the Grow phase of the algorithm
//  i.e., grow the goal shape from the intermediate configuration
void MeltSortGrowBlockCode::grow() {
    moveToGoal();
}

P2PNetworkInterface *MeltSortGrowBlockCode::getNextUnprocessedInterface() {
    for (const auto& element : flag) {
        if (!element.second) {
            return element.first;
        }
    }

    return NULL;
}

bool MeltSortGrowBlockCode::computeNextRotation(list<PathHop>& path) 
{
    PathHop &lastHop = path.back();

    short catomDockingConnector = catom->getConnectorId(lastHop.getPosition());
    short pivotDockingConnector =
        lastHop.getHopConnectorAtPosition(catom->position); // so-so    

    cout << "computeNextRotation: " << "module is on " << pivotDockingConnector
         << " -> " << catomDockingConnector << endl
         << "input path: " << lastHop << endl;

    std::vector<short> adjacentPathConnectors;
    std::map<short, short> mirrorConnector;
    API::findAdjacentConnectors(catom, lastHop,
                                pivotDockingConnector, catomDockingConnector,
                                adjacentPathConnectors, mirrorConnector);
        
    vector<Catoms3DMotionRulesLink*> mrl;
    API::getMotionRulesFromConnector(catom, catomDockingConnector, mrl);
            
    // Mobile Module:
    // Check if module can move to any of the adjacent path connectors of pivot.
    // Input connector set is ordered by distance to target, so the search should stop
    // as soon as a solution has been found
    nextRotation =
        API::findConnectorsPath(mrl, catomDockingConnector, adjacentPathConnectors, catom);
            
    if (nextRotation) {
        short dirTo = nextRotation->getConToID();
        lastHop.prune(mirrorConnector[dirTo]);
        
        cout << "Moving : " << *nextRotation << endl << endl;

        nextRotation->sendRotationEvent(catom,
                                        catom->getNeighborOnCell(lastHop.getPosition()),
                                        getScheduler()->now() + 100);
        nextRotation = NULL;

        return true;        
    }

    return false;    
}


bool MeltSortGrowBlockCode::tryNextMeltRotation(list<PathHop>& path) {
    if (path.empty()) {
        cout << "catom has reached target location" << endl;                    
        return true;
    }
        
    // List all connectors that could be filled in order to connect
    //  a neighbor module to the last hop or that would help reach parent's path connectors
    PathHop &lastHop = path.back();

    if (lastHop.catomIsAlreadyOnBestConnector(catom->position)) {
        path.pop_back();
        if (path.empty()) return true;
        else lastHop = path.back();
    }

    this->path = path;
    bool rotationIsPossible = computeNextRotation(this->path);
    if (!rotationIsPossible) 
    {
        // Stay in place an restart mobileModuleSearch
        cout << "tryNextMeltRotation: Could not compute feasible rotation plan to parent"
             << endl;

        return true;
    }

    return false;
}

bool MeltSortGrowBlockCode::tryNextGrowthRotation(list<PathHop>& path) {
    assert(!path.empty());
        
    // List all connectors that could be filled in order to connect
    //  a neighbor module to the last hop or that would help reach parent's path connectors
    PathHop &lastHop = path.back();

    if (lastHop.catomIsAlreadyOnBestConnector(catom->position)) {
        path.pop_back();
        if (path.empty()) return true;
        else lastHop = path.back();
    }
    
    bool rotationIsPossible = computeNextRotation(this->path);
    if (!rotationIsPossible) 
    {
        cout << "tryNextGrowthRotation: Could not compute feasible rotation plan to parent"
             << endl;
        
        OUTPUT << rtg << endl; 
        
        awaitKeyPressed();        
        
        assert(false);
        return true; // growth is over
    }
    
    return false;
}
