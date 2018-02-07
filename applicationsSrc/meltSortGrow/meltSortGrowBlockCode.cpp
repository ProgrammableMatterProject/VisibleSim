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
#include "meltSortGrowUtils.hpp"
#include "APLTypes.hpp"
#include "Messages/Melt/findMobileModuleMessages.hpp"
#include "api.hpp"

using namespace Catoms3D;

MeltSortGrowBlockCode::MeltSortGrowBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    cout << "MeltSortGrowBlockCode constructor" << endl;
    scheduler = getScheduler();
    catom = (Catoms3DBlock*)hostBlock;
}

MeltSortGrowBlockCode::~MeltSortGrowBlockCode() {
    cout << "MeltSortGrowBlockCode destructor" << endl;
}

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
                sendMessage("visited",
                            new MessageOf<int>(MSG_MELT_APL_VISITED, dfn),
                            module, 100, 0);
        }
    }
}


void MeltSortGrowBlockCode::APLabellingSearch() {
    P2PNetworkInterface *unprocessedNeighbor = getNextUnprocessedInterface();
    
    if (unprocessedNeighbor) {
        // Send TOKEN(DFNcnt(i) + 1) to k
        sendMessage("Token",
                    new MessageOf<int>(MSG_MELT_APL_TOKEN, dfnCnt + 1),
                    unprocessedNeighbor, 100, 0);
        sons.insert(unprocessedNeighbor);
    } else if (source) { /* root checks for articulation point and terminates */
        if (sons.size() >= 2) articulationPoint = true;
        
        bool meltStarted = initializeMeltPath();

        if (meltStarted) {
            // Prepare data structures for the mobile module search DFS
            resetDFSForSearching();

            findMobileModule();
        }
    } else { /* a normal node has finished the visits to its subtree */
        if (minDfn < lDfn && minSdr != father)
            lDfn = minDfn; /* sets to the smaller min. DFN from ancestors */            

        if (lDfn == dfn)
            bridge = true;

        // send ECHO(L(i), DFNcnt(i)) to Father(i)
        sendMessage("Echo",
                    new MessageOf<EchoPayload>(MSG_MELT_APL_ECHO,
                                               EchoPayload(lDfn, dfnCnt)),
                    father, 100, 0);
    }
    
}

bool MeltSortGrowBlockCode::initializeMeltPath() {    
    // Set next position to be filled
    Cell3DPosition tailPosition = catom->position - Cell3DPosition(1, 0, 0);

    if (lattice->isInGrid(tailPosition)) {
        short tailConId = catom->getConnectorId(tailPosition);
        cout << "Tail is " << tailConId << endl;

        // Initially only the target connector needs be in the path 
        // path.push_back(PathHop(catom->position, catom->orientationCode,
        //                        { {tailConId, 0} }) // Inline map init
        //     );

        // Add self as the next hop of the path
        API::addModuleToPath(catom, path, tailConId, tailConId);

        return true;
    } else {
        catom->setColor(RED);
        grow();

        return false;
    }
}

void MeltSortGrowBlockCode::findMobileModule() {    
    P2PNetworkInterface *unprocessedNeighbor = getNextUnprocessedInterface();
    
    if (unprocessedNeighbor) {
        sendMessage("FindMobileModule",
                    new FindMobileModuleMessage(path),
                    unprocessedNeighbor, 100, 0);
    } else if (source) {
        // No more module paths to explore
        catom->setColor(BLUE); // #TOREMOVE todo
        grow();
    } else {
        sendMessage("FindMobileModuleNotFound",
                    new FindMobileModuleNotFoundMessage(),
                    meltFather, 100, 0);
        resetDFSFlags();
    }
}

void MeltSortGrowBlockCode::propagateGraphResetBFS() {
    resetChildrenDecount = 0;
    for (P2PNetworkInterface *nghbr : catom->getP2PNetworkInterfaces()) {
        if (nghbr->connectedInterface && nghbr != resetFather) {
            sendMessage("GraphReset",
                        new Message(MSG_MELT_RESET_GRAPH),
                        nghbr, 100, 0);
            resetChildrenDecount++;
        }
    }

    if (!resetChildrenDecount && resetFather) { // No children, return 
        sendMessage("GraphResetDone",
                    new Message(MSG_MELT_RESET_GRAPH_DONE),
                    resetFather, 100, 0);
        resetDFSForLabelling();
    }
}

void MeltSortGrowBlockCode::moveToGoal() {
    growthParent = NULL;
    
    if (!rtg->getTargetCellsAsc().empty()) {
        goalPosition = rtg->getTargetCellsAsc().front();
        rtg->targetCellsAsc->pop_front();

        cout << " New growth target position: " << goalPosition << endl;
        
        // Here we would normally have to DFS-send FINDPATH until we reach
        //  goalPosition through path positions (surface movements)
        // For now, teleporting there right away is sufficient.

        resetDFSFlags();

        P2PNetworkInterface *unprocessedNeighbor = getNextUnprocessedInterface();
    
        if (unprocessedNeighbor) {
            sendMessage("FindPath",
                        new MessageOf<Cell3DPosition>(MSG_GROW_FINDPATH, goalPosition),
                        unprocessedNeighbor, 100, 0);
        } else {
            console << "Houston we have a problem." << "\n";
            catom->setColor(BLACK); // todo
        }

        // Once it's done, broadcast an announcement saying this position is filled,
        //  and trigger next individual growth
    } else {
        // Growth is over. Yee-pee
        catom->setColor(BLACK);
    }
              
}

void MeltSortGrowBlockCode::processReceivedMessage(MessagePtr msg,
                                                   P2PNetworkInterface *sender) {
    stringstream info;

    switch (msg->type) {

        case MSG_ROOT_UPDATE: {
            Cell3DPosition candidateRoot =
                *(std::static_pointer_cast<MessageOf<Cell3DPosition>>(msg)->getData());
        
            if (challengeRootFitness(candidateRoot)) {
                currentRootPosition = candidateRoot;
            
                // Broadcast new root candidate to every neighbor except sender
                sendMessageToAllNeighbors("RootUpdate",
                                          new MessageOf<Cell3DPosition>(MSG_ROOT_UPDATE,
                                                                        currentRootPosition),
                                          0, 100, 1, sender);
                expectedConfirms = catom->getNbNeighbors() - 1; // Ignore parent
                parent = sender;

                if (!expectedConfirms) {
                    sendMessage("RootConfirmation",
                                new MessageOf<Cell3DPosition>(MSG_ROOT_CONFIRM,
                                                              currentRootPosition),
                                parent, 0, 100);
                }    
            } else {
                // Received best root twice, send a NACK
                sendMessage("RootConfirmation",
                            new MessageOf<Cell3DPosition>(MSG_ROOT_NCONFIRM,
                                                          candidateRoot),
                            sender, 0, 100);
            }
        } break;

        case MSG_ROOT_CONFIRM:
            // If confirmation, first add to children and then proceed as in NCONFIRM
            // children.push_back(sender);
        case MSG_ROOT_NCONFIRM:{
            Cell3DPosition confirmedRoot =
                *(std::static_pointer_cast<MessageOf<Cell3DPosition>>(msg)->getData());

            if (confirmedRoot == currentRootPosition)
                --expectedConfirms;
            
            if (!expectedConfirms) {
                if (parent)
                    sendMessage("RootConfirmation",
                                new MessageOf<Cell3DPosition>(MSG_ROOT_CONFIRM,
                                                              currentRootPosition),
                                parent, 0, 100);
                else {
                    if (catom->position == currentRootPosition) { // Election Complete, module is root
                        relPos = new Cell3DPosition(0,0,0);
                        rtg->setOrigin(catom->position);
                        cout << "Absolute Target Positions" << endl; 
                        for (auto x : rtg->getTargetCellsAsc())
                            cout << x << endl;                        
                        rtg->targetCellsAsc->pop_front(); // this is the root's target position, in which it already is
                        
                        // Proceed to next stage
                        meltOneModule();
                    }
                }
            }    
        } break;
        
        case MSG_MELT_APL_START: {
            if (state == APLState::Inactive) {
                state = APLState::Active;;
                lDfn = dfn = dfnCnt = 1;
                APLabellingSearch();
                
                // send VISITED(DFN(i)) to all nodes in (Neighbors(i) - Sons(i))
                for (auto const& module : neighbors) {
                    if (!sons.count(module))
                        sendMessage("visited",
                                    new MessageOf<int>(MSG_MELT_APL_VISITED, dfn),
                                    module, 100, 0);
                }
            }
            
        } break;

        case MSG_MELT_APL_TOKEN: {
            int fatherDfnCnt =
                *(std::static_pointer_cast<MessageOf<int>>(msg)->getData());

            flag[sender] = true;

            if (state == APLState::Inactive) {
                state = APLState::Active;
                father = sender;
                lDfn = dfn = dfnCnt = fatherDfnCnt;

                APLabellingSearch();
                
                // send VISITED(DFN(i)) to all nodes in (Neighbors(i) -Sons(i)-Father(i))
                for (auto const& module : neighbors) {
                    if (!sons.count(module) && module != father)
                        sendMessage("visited",
                                    new MessageOf<int>(MSG_MELT_APL_VISITED, lDfn),
                                    module, 100, 0);
                }

            } else {
                bool senderIsChild = sons.count(sender);
                if (senderIsChild) {
                    sons.erase(sender);
                    APLabellingSearch();
                }
            }
            
        } break;

        case MSG_MELT_APL_ECHO: {
            EchoPayload *echoPayload =
                std::static_pointer_cast<MessageOf<EchoPayload>>(msg)->getData();

            bool senderIsChild = sons.count(sender);
            if (senderIsChild) {
                flag[sender] = true;
                lDfn = std::min(lDfn, echoPayload->lDfn); /* updates L(i) to reflect its son's smaller L */
                dfnCnt = std::max(dfnCnt, echoPayload->dfnCnt);

                if (echoPayload->lDfn >= dfn && !source) {
                    articulationPoint = true;
                    catom->setColor(PINK); // #ToRemove
                }

                APLabellingSearch();
            }
        } break;

        case MSG_MELT_APL_VISITED: {
            int receivedDfn =
                *(std::static_pointer_cast<MessageOf<int>>(msg)->getData());            

            flag[sender] = true;
            
            if (receivedDfn < minDfn) {
                minDfn = receivedDfn;
                minSdr = sender;
            }
            
            bool senderIsChild = sons.count(sender);
            if (senderIsChild) {
                sons.erase(sender);
                APLabellingSearch();
            }
        } break;

        case MSG_MELT_RESET_GRAPH: {
            if (!resetFather) 
                resetFather = sender;

            if (resetFather == sender)
                propagateGraphResetBFS();
            else {
                sendMessage("MeltResetGraphNack",
                            new Message(MSG_MELT_RESET_GRAPH_NACK),
                            sender, 100, 0);
            }

        } break;

        case MSG_MELT_RESET_GRAPH_DONE:
        case MSG_MELT_RESET_GRAPH_NACK: {            
            if (!--resetChildrenDecount) {
                if (resetFather) {
                    sendMessage("GraphResetDone",
                                new Message(MSG_MELT_RESET_GRAPH_DONE),
                                resetFather, 100, 0);
                    resetDFSForLabelling();
                } else { // isSource
                    resetDFSForLabelling();
                    meltOneModule();
                }                
            }
        } break;

        case MSG_GROW_FINDPATH: {
            goalPosition =
                *(std::static_pointer_cast<MessageOf<Cell3DPosition>>(msg)->getData());
            
            if (!growthVisited) { // Module has not been considered for path planning yet
                if (!growthParent) growthParent = sender;

                growthVisited = true;
                
                flag[sender] = true;

                // targetCells.pop_front(); todo done globally for now
                
                // Check if adjacent to target and Visit sub-tree otherwise
                if (lattice->cellsAreAdjacent(catom->position, goalPosition)) {
                    list<int> pathIds; pathIds.push_back(catom->blockId);
                    sendMessage("FindPathFound",
                                new MessageOf<list<int>>(MSG_GROW_FINDPATH_FOUND,
                                                         pathIds),
                                growthParent, 100, 0);
                    catom->setColor(BLUE); // todo
                    console << "Path to " << goalPosition << " found" << "\n";
                } else {
                    P2PNetworkInterface *unprocessedNeighbor =
                        getNextUnprocessedInterface();

                    if (unprocessedNeighbor) {
                        sendMessage("FindPath",
                                    new MessageOf<Cell3DPosition>(MSG_GROW_FINDPATH,
                                                                  goalPosition),
                                    unprocessedNeighbor, 100, 0);
                    } else {
                        sendMessage("FindPathNotFound",
                                    new Message(MSG_GROW_FINDPATH_NOTFOUND),
                                    growthParent, 100, 0);
                    }

                }
                     
            } else { // Module has already been considered in this phase
                sendMessage("FindPathIgnore",
                            new Message(MSG_GROW_FINDPATH_IGNORE),
                            sender, 100, 0);
            }
            
        } break;

            
        case MSG_GROW_FINDPATH_FOUND: {
            list<int> pathIds =
                *(std::static_pointer_cast<MessageOf<list<int>>>(msg)->getData());

            flag[sender] = true;

            if (growthParent) {            
                pathIds.push_back(catom->blockId);
                sendMessage("FindPathFound",
                            new MessageOf<list<int>>(MSG_GROW_FINDPATH_FOUND,
                                                     pathIds),
                            growthParent, 100, 0);
                growthVisited = false;
                catom->setColor(GREEN); // todo
            } else { // isTail, moving module
                catom->setColor(RED);
                growing = true;
                growthVisited = true;
                console << "Moving to " << goalPosition << "!" << "\n";
                scheduler->schedule(new TeleportationStartEvent(scheduler->now() + 150,
                                                                catom, goalPosition));
            } 
        } break;

        case MSG_GROW_FINDPATH_NOTFOUND:
        case MSG_GROW_FINDPATH_IGNORE: {
            P2PNetworkInterface *unprocessedNeighbor =
                getNextUnprocessedInterface();

            flag[sender] = true;

            if (unprocessedNeighbor) {
                sendMessage("FindPath",
                            new MessageOf<Cell3DPosition>(MSG_GROW_FINDPATH,
                                                          goalPosition),
                            unprocessedNeighbor, 100, 0);
            } else {
                if (growthParent) {
                    sendMessage("FindPathNotFound",
                                new Message(MSG_GROW_FINDPATH_NOTFOUND),
                                growthParent, 100, 0);
                    growthVisited = false;
                } else {
                    catom->setColor(WHITE); //todo probleeeeem
                } 
            }
        } break;
            
        case MSG_GROW_NEXTMODULE: {
            if (growthParent) {
                resetDFSFlags();
                
                if (growthParent->isConnected()) {
                    sendMessage("NextModule",
                                new Message(MSG_GROW_NEXTMODULE),
                                growthParent, 100, 0);
                    growthVisited = false;
                } else { // Current module should be new tail
                    moveToGoal();
                }
            } else {
                console << "I'm very much sorry sir, but I do not have a growth parent." << "\n";
            }
            
        } break;
            
        default: cout << "wut?" << endl; break;
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
                console << " received " << message->type << " from "
                        << recv_interface->connectedInterface->hostBlock->blockId << "\n";
            
                // Handover to global message handler
                processReceivedMessage(message, recv_interface);
            }
        } break;

        case EVENT_TELEPORTATION_END: {
            if (!growing) { // MELT
                propagateGraphResetBFS();
            } else { // GROW 
                // do stuff...
                if (catom->position == goalPosition) {
                    growing = false;
                    growthVisited = false;
                    resetDFSFlags();

                    // Send to single neigbor which will follow the message
                    //  route back to the new tail
                    assert(neighbors.size());
                    growthParent = neighbors.front();
                    sendMessage("NextModule",
                                new Message(MSG_GROW_NEXTMODULE),
                                growthParent, 100, 0);
                } else {
                    // Perform next move towards current growth target
                }
                
            }
        } break;

        case EVENT_ROTATION3D_END: {
            if (!meltRotationsPlan.empty()) {                    
                Catoms3DMotionRulesLink *nextRotation = meltRotationsPlan.front();
                pivotLinkConId = nextRotation->getConToID();
                meltRotationsPlan.pop_front();
                nextRotation->sendRotationEvent(catom, rotationPlanPivot,
                                                getScheduler()->now() + 100);
            } else {
                if (!path.empty()) { // Not in place yet
                    // Check if next path hop has been reached
                    auto lastHop = path.back();

                    // Case 1: This is not the final hop
                    if (path.size() > 1) {
                        PathHop& nextHop = *(std::prev(std::prev(path.end())));

                        // Next hop is within reach, clear current hop
                        if (nextHop.isInVicinityOf(catom->position))
                            path.pop_back();
                    } else {
                        if (lastHop.finalTargetReached()) {
                            path.pop_back();
                            cout << "Final hop is empty, we should be done 2" << endl;

                            melted = true;
                            propagateGraphResetBFS();
                            
                            return;
                        }
                        
                    }
                    
                    tryNextMeltRotation(path);
                } else {
                    cout << "Final hop is empty, we should be done" << endl;
                }
            }
        } break;
            
        case EVENT_TAP: {
            for (const auto &pair : flag)
                cout << pair.first->connectedInterface->hostBlock->blockId << endl;
        } break;
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
    //  and updates it when it finds a better potential root for (min(x) & min(y) & min(z) in this order)
    //  when the root is updated, the block broadcasts the new root, and then expects an ack from all of its neighbors
    currentRootPosition = catom->position;

    parent = NULL;
    expectedConfirms = catom->getNbNeighbors();
    
    // Send a message to every neighbor that includes the block's own location, as a potential root
    sendMessageToAllNeighbors("RootUpdate",
                              new MessageOf<Cell3DPosition>(MSG_ROOT_UPDATE,
                                                            currentRootPosition),
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


bool MeltSortGrowBlockCode::tryNextMeltRotation(vector<PathHop>& path) {
    if (path.empty()) {
        cout << "catom has reach target location" << endl;
        melted = true;
        return false;
    }
        
    // List all connectors that could be filled in order to connect
    //  a neighbor module to the last hop or that would help reach parent's path connectors
    PathHop &lastHop = path.back();
    cout << "tryNextMeltRotation: " << "input path: " << lastHop << endl;

    short catomDockingConnector = catom->getConnectorId(lastHop.getPosition());
    short pivotDockingConnector =
        lastHop.getHopConnectorAtPosition(catom->position); // so-so

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
    meltRotationsPlan.clear();
    API::findConnectorsPath(mrl, catomDockingConnector, adjacentPathConnectors,
                            meltRotationsPlan);
            
    if (!meltRotationsPlan.empty()) {
        short dirTo = meltRotationsPlan.back()->getConToID();
        lastHop.prune(mirrorConnector[dirTo]);
        // lastHop.removeConnectorOnPosition(catom->position);
        
        Catoms3DMotionRulesLink *nextRotation = meltRotationsPlan.front();

        cout << "Moving : " << *nextRotation << endl << endl;
        
        meltRotationsPlan.pop_front();
        nextRotation->sendRotationEvent(catom,
                                        catom->getNeighborOnCell(lastHop.getPosition()),
                                        getScheduler()->now() + 100);
        return true;
    } else {
        cout << "tryNextMeltRotation: Could not compute feasible rotation plan to parent" << endl;
    }

    return false;
}
