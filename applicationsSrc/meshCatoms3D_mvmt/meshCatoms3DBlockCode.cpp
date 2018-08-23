/**
 * @file   meshCatoms3DBlockCode_mvmt.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 13:47:56 2018
 * 
 * @brief  
 * 
 * 
 */

#include <iostream>
#include <set>

#include "catoms3DWorld.h"
#include "scheduler.h"
#include "events.h"
#include "trace.h"
#include "tDefs.h"

#include "meshCatoms3DBlockCode.hpp"

// C3D Motion Engine
#include "messages.hpp"
#include "catoms3DMotionEngine.hpp"

using namespace Catoms3D;
using namespace MeshSpanningTree;

uint MeshCatoms3DBlockCode::X_MAX;
uint MeshCatoms3DBlockCode::Y_MAX;
bool MeshCatoms3DBlockCode::skipMeshInit = false;
bID id = 1;

#define IT_MODE_TILE_INSERTION 1

MeshCatoms3DBlockCode::MeshCatoms3DBlockCode(Catoms3DBlock *host):
    Catoms3DBlockCode(host) {
    scheduler = getScheduler();
    world = BaseSimulator::getWorld();
    lattice = world->lattice;   
    catom = host;
    engine = new Catoms3DMotionEngine(*this, *host);
    
    const Cell3DPosition& ub = lattice->getGridUpperBounds();
    X_MAX = ub[0];
    Y_MAX = ub[1];

    ruleMatcher = new MeshSpanningTreeRuleMatcher(X_MAX, Y_MAX, B);
}

MeshCatoms3DBlockCode::~MeshCatoms3DBlockCode() {
    delete engine;
}


bool MeshCatoms3DBlockCode::moduleInSpanningTree(const Cell3DPosition& pos) {
    return target->isInTarget(pos) and lattice->isInGrid(pos);
}

bool MeshCatoms3DBlockCode::isMeshRoot(const Cell3DPosition& pos) {
    return pos.pt[0] % B == 0 and pos.pt[1] % B == 0 and pos.pt[2] % B == 0;
}

static std::set<Cell3DPosition> placedBorderCatoms;
void MeshCatoms3DBlockCode::startup() {
    stringstream info;
    info << "Starting ";

    if (not skipMeshInit) {
        bool isLeaf = true;
        static const bool ASSEMBLE_SCAFFOLD = true;
        if (ASSEMBLE_SCAFFOLD) {        
            for (auto const& nPos : world->lattice->getNeighborhood(catom->position)) {            
                if (ruleMatcher->shouldSendToNeighbor(catom->position, nPos)
                    and ruleMatcher->isInMesh(nPos)) {

                    if (ruleMatcher->isTileRoot(nPos)) {
                        if (checkOrthogonalIncidentBranchCompletion(catom->position)
                            or ruleMatcher->isOnXBorder(catom->position)
                            or ruleMatcher->isOnYBorder(catom->position))
                            world->addBlock(++id, buildNewBlockCode, nPos, GREEN);
                        else {
                            posTileAwaitingPlacement = nPos;
                            getScheduler()->schedule(
                                new InterruptionEvent(getScheduler()->now() + 200, catom,
                                                      IT_MODE_TILE_INSERTION));
                        }
                    } else {
                        world->addBlock(++id, buildNewBlockCode, nPos, ORANGE);
                    }

                    awaitKeyPressed();
                    isLeaf = false;
                }
            }    
        }

        static const bool NOTIFY_SCAFFOLD_COMPLETION = true;
        static const bool DISASSEMBLE_INTO_OBJECT = not NOTIFY_SCAFFOLD_COMPLETION;
        if (isLeaf) {
            // Notify ST parent that scaffold construction is complete
            const Cell3DPosition& parentPos =
                ruleMatcher->getTreeParentPosition(catom->position);

            if (parentPos != catom->position) {
                // cout << "myPos: " << catom->position << " -- parentPos: " << parentPos << endl;
            
                P2PNetworkInterface* parentItf = catom->getInterface(parentPos);
                assert(parentItf);
                assert(parentItf->isConnected());

                if (DISASSEMBLE_INTO_OBJECT) {
                    sendMessage(new DisassemblyTriggerMessage(*ruleMatcher, true),
                                parentItf, MSG_DELAY_MC, 0);

                    if (not target->isInTarget(catom->position)) catom->setVisible(false);
                    else catom->setColor(target->getTargetColor(catom->position));
                } else {
                    sendMessage(new SubTreeScaffoldConstructionDoneMessage(*ruleMatcher, true),
                                parentItf, MSG_DELAY_MC, 0);
                    catom->setColor(BLUE);
                }
            
            } // else module is mesh root, and therefore the only module is the configuration?!
        } else {
            numberExpectedAcksFromSubTree =
                ruleMatcher->getNumberOfExpectedSubTreeConfirms(catom->position);
        }

        engine->resetDFS();
    } else {
        // Any module added after skipMeshInit has been set to true will run this code
        //  and therefore explore the mesh
        goalPosition = Cell3DPosition(1,-3,6);
        engine->attemptMovingTo(goalPosition);
    }
}    

// const Cell3DPosition& MeshCatoms3DBlockCode::chooseNextHop() {
//     const Cell3DPosition &diff = dest - catom->position;
//     if (diff[2] < )
// }

void MeshCatoms3DBlockCode::triggerMeshTraversalProcess() {
    const Cell3DPosition& startingPos = Cell3DPosition(-1,-1,2);
    static const bool PERFORM_SCAFFOLD_TRAVERSAL = true;
    if (PERFORM_SCAFFOLD_TRAVERSAL) {
        skipMeshInit = true;
        world->addBlock(++id, buildNewBlockCode, startingPos, GREEN);
    }
}

void MeshCatoms3DBlockCode::processReceivedMessage(MessagePtr msg,
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

void MeshCatoms3DBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;
	
    switch (pev->eventType) {
        case EVENT_RECEIVE_MESSAGE: {
            message =
                (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;

            if (message->isMessageHandleable()) {
                (std::static_pointer_cast<Catoms3DMotionEngineMessage>(message))->
                    handle(this);
            } else {
                P2PNetworkInterface * recv_interface = message->destinationInterface;
            
                // Handover to global message handler
                processReceivedMessage(message, recv_interface);
            }
        } break;

        case EVENT_ROTATION3D_END: {
            engine->handleRotationEnd();
        } break;            
            
        case EVENT_TAP: {
            // ?
        } break;

        case EVENT_INTERRUPTION: {
            std::shared_ptr<InterruptionEvent> itev =
                std::static_pointer_cast<InterruptionEvent>(pev);
            switch(itev->mode) {
                case IT_MODE_TILE_INSERTION:
                    if (checkOrthogonalIncidentBranchCompletion(catom->position))
                        world->addBlock(++id, buildNewBlockCode,
                                        posTileAwaitingPlacement, GREEN);
                    else
                        getScheduler()->schedule(
                            new InterruptionEvent(getScheduler()->now() + 200, catom,
                                                  IT_MODE_TILE_INSERTION));                    
                    break;
            }
        }
    }
}

bool MeshCatoms3DBlockCode::
checkOrthogonalIncidentBranchCompletion(const Cell3DPosition& pos) {
    VS_ASSERT(ruleMatcher->isInMesh(pos));
    
    if (ruleMatcher->isOnXBranch(pos))
        return lattice->cellHasBlock(pos + Cell3DPosition(1,-1,0));
    else if (ruleMatcher->isOnYBranch(pos))
        return lattice->cellHasBlock(pos + Cell3DPosition(-1,1,0));
    else return true; // FIXME:
}
