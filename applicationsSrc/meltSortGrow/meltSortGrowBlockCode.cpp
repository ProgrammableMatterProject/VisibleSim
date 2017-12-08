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
#include "meltSortGrowBlockCode.h"
#include "scheduler.h"
#include "events.h"
#include "trace.h"
#include "teleportationEvents.h"
#include "meltSortGrowUtils.h"
#include "APLTypes.h"

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

    
    isTail = false;
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
}

void MeltSortGrowBlockCode::resetDFSForSearching() {
    flag.clear();
    
    for (P2PNetworkInterface *itf : sons) {
        flag[itf] = false; // Reset DFS markers
    }
}

void MeltSortGrowBlockCode::APLabellingInitialization() {
    resetDFSForLabelling();    
}

void MeltSortGrowBlockCode::APLabellingStart() {
    source = true;
    catom->setColor(RED);
        
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
            
        // For now, re-use tree built during APL phase
        findMobileModule();
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

void MeltSortGrowBlockCode::findMobileModule() {
    Cell3DPosition tailPosition = catom->position - Cell3DPosition(1, 0, 0);

    // Prepare data structures for the mobile module search DFS
    resetDFSForSearching();
    
    P2PNetworkInterface *unprocessedNeighbor = getNextUnprocessedInterface();
    
    if (unprocessedNeighbor) {
        sendMessage("FindMobileModule",
                    new MessageOf<Cell3DPosition>(MSG_MELT_FIND_MOBILE_MODULE,
                                                  tailPosition),
                    unprocessedNeighbor, 100, 0);        
    } else if (source) {
        // No more module paths to explore,
        catom->setColor(BLUE); // #TOREMOVE
    } else {
        sendMessage("FindMobileModuleNope",
                    new MessageOf<Cell3DPosition>(MSG_MELT_FIND_MOBILE_MODULE_NOPE,
                                                  tailPosition),
                    father, 100, 0);
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
                        // Proceed to next stage
                        isTail = true;
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
                                    new MessageOf<int>(MSG_MELT_APL_VISITED, dfn),
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

                if (lDfn >= dfn && !source) {
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

        case MSG_MELT_FIND_MOBILE_MODULE: {
            Cell3DPosition tailPosition =
                *(std::static_pointer_cast<MessageOf<Cell3DPosition>>(msg)->getData());

            // Prepare data structures for the mobile module search DFS
            resetDFSForSearching();
            
            if (articulationPoint) {
                P2PNetworkInterface *unprocessedNeighbor = getNextUnprocessedInterface();
                if (unprocessedNeighbor) {
                    sendMessage("FindMobileModule",
                                new MessageOf<Cell3DPosition>(MSG_MELT_FIND_MOBILE_MODULE,
                                                              tailPosition),
                                unprocessedNeighbor, 100, 0);        
                } else {
                    // There are no mobile modules on this branch
                    sendMessage("FindMobileModuleNope",
                                new MessageOf<Cell3DPosition>(MSG_MELT_FIND_MOBILE_MODULE_NOPE,
                                                              tailPosition),
                                father, 100, 0);
                }
            } else {
                // Module is mobile, notify father and jump to tail for now
                sendMessage("FindMobileModuleYup",
                            new MessageOf<Cell3DPosition>(MSG_MELT_FIND_MOBILE_MODULE_YUP,
                                                          tailPosition),
                            father, 100, 0);

                catom->setColor(WHITE);
                // Teleport to tail for now
                scheduler->schedule(
                    new TeleportationStartEvent(scheduler->now() + 150,
                                                catom, tailPosition));
            }
        } break;

        case MSG_MELT_FIND_MOBILE_MODULE_YUP: {
            Cell3DPosition tailPosition =
                *(std::static_pointer_cast<MessageOf<Cell3DPosition>>(msg)->getData());

            if (!source) {
                sendMessage("FindMobileModuleYup",
                            new MessageOf<Cell3DPosition>(MSG_MELT_FIND_MOBILE_MODULE_YUP,
                                                          tailPosition),
                            father, 100, 0);
            } else {
                catom->setColor(ORANGE);
            }

            // resetDFSForLabelling();
        } break;

        case MSG_MELT_FIND_MOBILE_MODULE_NOPE: {
            Cell3DPosition tailPosition =
                *(std::static_pointer_cast<MessageOf<Cell3DPosition>>(msg)->getData());

            flag[sender] = true;
            
            P2PNetworkInterface *unprocessedNeighbor = getNextUnprocessedInterface();
            if (unprocessedNeighbor) {
                sendMessage("FindMobileModule",
                            new MessageOf<Cell3DPosition>(MSG_MELT_FIND_MOBILE_MODULE,
                                                          tailPosition),
                            unprocessedNeighbor, 100, 0);
            } else {
                sendMessage("FindMobileModuleNope",
                            new MessageOf<Cell3DPosition>(MSG_MELT_FIND_MOBILE_MODULE_NOPE,
                                                          tailPosition),
                            father, 100, 0);
            }

            // resetDFSForLabelling();
        } break;

        case MSG_MELT_RESET_GRAPH: {
            if (!resetFather) 
                resetFather = sender;

            if (resetFather == sender)
                propagateGraphResetBFS();
            else {
                sendMessage("No Sir, I do not want anything to do with you",
                            new Message(MSG_MELT_RESET_GRAPH_NOSIRIDONOTWANTANYTHINGTODOWITHYOU),
                            sender, 100, 0);
            }

        } break;

        case MSG_MELT_RESET_GRAPH_DONE:
        case MSG_MELT_RESET_GRAPH_NOSIRIDONOTWANTANYTHINGTODOWITHYOU: {            
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
            P2PNetworkInterface * recv_interface = message->destinationInterface;
            console << " received " << message->type << " from "
                    << recv_interface->connectedInterface->hostBlock->blockId << "\n";
            
            // Handover to global message handler
            processReceivedMessage(message, recv_interface);
        } break;

        case EVENT_TELEPORTATION_END: {
            console << "I have reached where you wanted me to go sir." << "\n";
            // World::getWorld()->connectBlock(catom);
            propagateGraphResetBFS();
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
    return candidateRoot < currentRootPosition;
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
    return;
}

struct C3DPos_hash {
    std::size_t operator()(const Cell3DPosition& _pos) const {
        std::ostringstream oss; oss << _pos; 
        return std::hash<std::string>()(oss.str());
    }
};
void MeltSortGrowBlockCode::initializePathPositions() {
    // Add target tail position at front of list
    myPathPositions.clear();
    myPathPositions.push_back(catom->position + Cell3DPosition(-1, 0, 0));

    std::vector<Cell3DPosition> freeNeighborCells =
        lattice->getFreeNeighborCells(catom->position);

    // Ensures position are only added once to the list (ToImprove)
    std::unordered_set<Cell3DPosition, C3DPos_hash> addedPos;
    
    for (Cell3DPosition pathPos : freeNeighborCells) {
        if (!addedPos.count(pathPos)) {
            addedPos.insert(pathPos);
            myPathPositions.push_back(pathPos);
        }
    }
}

// Returns true if there is a path position around this module that can reach
//  at least one of the path positions of the module's parent
// Returns false otherwise
bool MeltSortGrowBlockCode::computePathPositions(list<Cell3DPosition> *parentPath,
                                                 Cell3DPosition parentPos) {
    // Clear previous computation in case neighborhood has changed
    myPathPositions.clear();
    
    // Find path position around us
    // Assumption: Module is able to detect whether or not one of its connectors is
    //  connected, and deduce the cell's position from its own and its connector's position
    std::vector<Cell3DPosition> freeNeighborCells =
        lattice->getFreeNeighborCells(catom->position);

    // Ensures position are only added once to the list (ToImprove)
    std::unordered_set<Cell3DPosition, C3DPos_hash> addedPos;

    // For every free position around module, check if it allows a direct motion
    //  to a cell of the parent's path
    for (Cell3DPosition pathPos : freeNeighborCells) {
        for (Cell3DPosition parentPathPos : *parentPath) {
            if (MeltSortGrowUtils::motionIsPossible(lattice,
                                                    pathPos,
                                                    parentPathPos,
                                                    parentPos)) {                
                if (!addedPos.count(pathPos)) {
                    addedPos.insert(pathPos);
                    myPathPositions.push_back(pathPos);
                }
            }
        }
    }

    return !myPathPositions.empty();        
}


P2PNetworkInterface *MeltSortGrowBlockCode::getNextUnprocessedInterface() {
    for (const auto& element : flag) {
        if (!element.second) {
            return element.first;
        }
    }

    return NULL;
}
