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

#include "catoms3DWorld.h"
#include "meltSortGrowBlockCode.h"
#include "scheduler.h"
#include "events.h"
#include "teleportationEvents.h"

#define verbose 1

using namespace std;
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

    // addMessageEventFunc(MSG_ROOT_UPDATE, _processReceivedMessage);
    
    info << "Starting ";
    isTail = false;
    determineRoot();
}

// static void _processReceivedMessage(BlockCode *bc, MessagePtr msg, P2PNetworkInterface *sender) {
//   cout << "coucou" << endl;
//   static_cast<MeltSortGrowBlockCode*>(bc)->processReceivedMessage(msg, sender);
// }

void MeltSortGrowBlockCode::processReceivedMessage(MessagePtr msg, P2PNetworkInterface *sender) {
    stringstream info;

    switch (msg->type) {

        case MSG_ROOT_UPDATE: {
            Cell3DPosition candidateRoot =
                *(std::static_pointer_cast<MessageOf<Cell3DPosition>>(msg)->getData());
        
            if (challengeRootFitness(candidateRoot)) {
                children.clear();
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

            catom->setColor(currentRootPosition == catom->position
                            && !electionClosed ?
                            RED : GREY);
        } break;

        case MSG_ROOT_CONFIRM:
            // If confirmation, first add to children and then proceed as in NCONFIRM
            children.push_back(sender);

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
                        catom->setColor(GREEN);
                        electionClosed = true;
                        isTail = true;
                        meltOneModule();
                    }
                }
            }    

            cout << "#" << catom->blockId << " is expecting "
                 << expectedConfirms << " confirms" << endl;
        } break;
            
        case MSG_MELT_LABEL_AP: {            
            // Send a message to every neighbor DFS style to show Spanning Tree
            // int colorCounter = *(std::static_pointer_cast<MessageOf<int>>(msg)
            //                      ->getData());
            if (!children.empty()) {
                amIArticulationPoint = true;
                catom->setColor(3);

                resetDFSQueue();
                sendMessage("SpanningTreeColoring",
                            new MessageOf<int>(MSG_MELT_LABEL_AP, 0),
                            dfsQueue.front(), 0, 100);
                dfsQueue.pop_front();
            } else {                
                // Color leaves in black
                catom->setColor(BLACK);
                amIArticulationPoint = false;

                sendMessage("SpanningTreeColoringDone",
                            new MessageOf<int>(MSG_MELT_LABEL_AP_DONE, 0),
                            parent, 0, 100);
            }
        } break;
            
        case MSG_MELT_LABEL_AP_DONE: { // Notify parent we finished labelling
            if (!dfsQueue.empty()) { // Send MSG_MELT_LABEL_AP to next neighbor
                sendMessage("SpanningTreeColoring",
                            new MessageOf<int>(MSG_MELT_LABEL_AP, 3),
                            dfsQueue.front(), 0, 100);
                dfsQueue.pop_front();
            } else { // Notify parent we're done here
                if (parent)
                    sendMessage("SpanningTreeColoringDone",
                                new MessageOf<int>(MSG_MELT_LABEL_AP_DONE, 0),
                                parent, 0, 100);
                else { // We're root, proceed to next step 
                    catom->setColor(RED);

                    // Start FIND_MODULE_POSITION DFS
                    resetDFSQueue();
                    sendMessage("FindMobileModule",
                                new MessageOf<std::set<Cell3DPosition, posCmp>*>(
                                    MSG_MELT_FIND_MOBILE_MODULE,
                                    computePathPositions(NULL)),
                                dfsQueue.front(), 0, 100);
                    dfsQueue.pop_front();
                }
            }
        } break;

        case MSG_MELT_FIND_MOBILE_MODULE: {
            std::list<Cell3DPosition, posCmp> *path =
                *(std::static_pointer_cast<MessageOf<std::set<Cell3DPosition, posCmp>*>>(msg)->getData());

            resetDFSQueue();
            if (amIArticulationPoint) {
                assert(!children.empty());
                
                // Add the module's path positions to current path
                currentPath = computePathPositions(path);
                
                // DFS-send to children until a mobile module is found
                sendMessage("FindMobileModule",
                            new MessageOf<std::set<Cell3DPosition, posCmp>*>(
                                MSG_MELT_FIND_MOBILE_MODULE,
                                currentPath),
                            dfsQueue.front(), 0, 100);
                dfsQueue.pop_front();                                         
            } else { // I am a mobile module, let's get going boyyyys!
                // But first, let me take a selfie
                currentPath = path;
                
                sendMessage("FindMobileModuleYup",
                            new MessageOf<bool>(
                                MSG_MELT_FIND_MOBILE_MODULE_YUP,
                                true),
                            parent, 0, 100);
                catom->setColor(PINK);

                // MOVEMOVEMOVE
                // Follow path positions 
                Catoms3DBlock *pivot = static_cast<Catoms3DBlock*>(
                    parent->connectedInterface->hostBlock);
                Time t = scheduler->now();
                Cell3DPosition teleportationTarget = computeNextMove(currentPath);
                scheduler->schedule(
                    new TeleportationStartEvent(t, catom, teleportationTarget));
                parent = NULL;
            }
        } break;
            
        case MSG_MELT_FIND_MOBILE_MODULE_YUP: {
            bool senderIsMovingModule =
                *(std::static_pointer_cast<MessageOf<bool*>>(msg)->getData());

            if (senderIsMovingModule)
                children.remove(sender);

            // Simply notify parent that a catom is on the move to open tail position
            if (parent) {
                sendMessage("FindMobileModuleYup",
                            new MessageOf<bool>(MSG_MELT_FIND_MOBILE_MODULE_YUP,
                                                false),
                            parent, 0, 100);
                catom->setColor(BLUE);
            }
        } break;
            
        case MSG_MELT_FIND_MOBILE_MODULE_NOPE: {
            Cell3DPosition openPosition =
                *(std::static_pointer_cast<MessageOf<Cell3DPosition>>(msg)->getData());
            
            // Notify parent
            if (parent) {
                sendMessage("FindMobileModuleYup",
                            new MessageOf<Cell3DPosition>(MSG_MELT_FIND_MOBILE_MODULE_YUP,
                                                          openPosition),
                            parent, 0, 100);
                catom->setColor(BLUE);
            } else { // Algorithm has converged (should be "isRoot" test perhaps)
                sort(); // homogenous robots, does nothing
                grow();
            }

        } break;
            
        case MSG_MELT_UPDATE_TAIL: {
            Cell3DPosition newTailPosition =
                *(std::static_pointer_cast<MessageOf<Cell3DPosition>>(msg)->getData());

            if (!parent) {
                parent = sender;
                isTail = false;
                amIArticulationPoint = true;
                sendMessage("UpdateTailAck",
                            new MessageOf<Cell3DPosition>(MSG_MELT_UPDATE_TAIL_ACK,
                                                          newTailPosition),
                            parent, 0, 100);
            }
        } break;
            
        case MSG_MELT_UPDATE_TAIL_ACK: {
            Cell3DPosition myTailPosition =
                *(std::static_pointer_cast<MessageOf<Cell3DPosition>>(msg)->getData());

            if (myTailPosition == catom->position) {
                children.push_back(sender);

                // Neighborhood updated, can now proceed to next Melt
                catom->setColor(RED);

                // Start FIND_MODULE_POSITION DFS
                resetDFSQueue();
                sendMessage("FindMobileModule",
                            new MessageOf<std::set<Cell3DPosition, posCmp>*>(
                                MSG_MELT_FIND_MOBILE_MODULE,
                                computePathPositions(NULL)),
                            dfsQueue.front(), 0, 100);
                dfsQueue.pop_front();
                
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
            message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
            P2PNetworkInterface * recv_interface = message->destinationInterface;

            // Handover to global message handler
            processReceivedMessage(message, recv_interface);
        } break;

        case EVENT_TAP: {
            static int mod = 0;
            if (mod == 0) catom->setColor(RED);
            else if (mod == 1) catom->setColor(GREEN);
            else catom->setColor(BLUE);

            mod = (mod + 1) % 3;
        } break;

        case EVENT_TELEPORTATION_END: {
            Time t = scheduler->now();
            Cell3DPosition teleportationTarget = computeNextMove(currentPath);

            cout << "Moving toooooooo " << teleportationTarget << endl;
            
            if (teleportationTarget != catom->position)
                scheduler->schedule(
                    new TeleportationStartEvent(t, catom, teleportationTarget));
            else {
                isTail = true;                
                sendMessageToAllNeighbors("UpdateTail",
                            new MessageOf<Cell3DPosition>(MSG_MELT_UPDATE_TAIL,
                                                          catom->position),
                            0, 100, 0);
            }
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
    //  when root is updated, the block broadcast the new root, and then expects an ack from all its neighbors
    currentRootPosition = catom->position;

    electionClosed = false;
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
    // While modules remain in initial configuration
    //  Find articulation point among modules
    //  (i.e., spanning tree leaves)
    int colorCounter = 0;
    amIArticulationPoint = false;
    
    // Send a message to every neighbor DFS style to show Spanning Tree
    // for (P2PNetworkInterface *connector : children) {
    resetDFSQueue();
    if (!dfsQueue.empty()) {
        sendMessage("SpanningTreeColoring",
                    new MessageOf<int>(MSG_MELT_LABEL_AP, colorCounter++),
                    dfsQueue.front(), 0, 100);
        dfsQueue.pop_front();
    }    
   
    // }
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

void MeltSortGrowBlockCode::resetDFSQueue() {
    dfsQueue.clear();
    for (P2PNetworkInterface *interface : children) {
        dfsQueue.push_back(interface);
    }
}

std::set<Cell3DPosition, MeltSortGrowBlockCode::posCmp>*
MeltSortGrowBlockCode::computePathPositions(std::set<Cell3DPosition, MeltSortGrowBlockCode::posCmp> *path) {
    // Path does not exist yet, initialize it 
    if (!path)
        path = new std::set<Cell3DPosition, MeltSortGrowBlockCode::posCmp>();
    
    // Find path position around us
    // Assumption: Module is able to detect whether or not one of its connectors is connected, and deduce the cell's position from its own and its connector's position
    std::vector<Cell3DPosition> freeNeighborCells = lattice->getFreeNeighborCells(catom->position);
    for (Cell3DPosition pathPos : freeNeighborCells) {
        path->insert(pathPos);
    }

    currentPath = path;
    
    return path;
}

const Cell3DPosition
MeltSortGrowBlockCode::computeNextMove(std::set<Cell3DPosition, MeltSortGrowBlockCode::posCmp> *path) {
    // Search among neighbor cells a cell that is in path, and that is closest to destination
    std::vector<Cell3DPosition> neighborCells = lattice->getFreeNeighborCells(catom->position);    
    std::set<Cell3DPosition, MeltSortGrowBlockCode::posCmp> moveCandidates;
    std::set<Cell3DPosition, MeltSortGrowBlockCode::posCmp>* newPath = new std::set<Cell3DPosition, MeltSortGrowBlockCode::posCmp>();
    
    // std::set_intersection(path->begin(), neighborCellsSet.begin(),
    //                       path->end(), neighborCellsSet.end(),
    //                       std::inserter(moveCandidates, moveCandidates->begin()));
    for (Cell3DPosition neighborPos : neighborCells) {
        if (path->count(neighborPos)) {
            moveCandidates.insert(neighborPos);
            path->erase(neighborPos);
        } 
    }
        
    if (moveCandidates.empty()) { // hu-ho
        // No possible move
        catom->setColor(ORANGE);
        return catom->position;
    } else { // Should be minimum reachable position, or target
        return *moveCandidates.begin();
    }
}
