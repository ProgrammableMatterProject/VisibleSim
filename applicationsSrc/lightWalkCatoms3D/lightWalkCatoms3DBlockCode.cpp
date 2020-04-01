/**
 * @file   lightWalkCatoms3DBlockCode.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Mon Dec 10 15:27:27 2018
 *
 * @brief
 *
 *
 */

#include <iostream>
#include <set>

#include "robots/catoms3D/catoms3DWorld.h"
#include "events/scheduler.h"
#include "events/events.h"
#include "utils/trace.h"
#include "utils/tDefs.h"

#include "motion/teleportationEvents.h"
#include "robots/catoms3D/catoms3DRotationEvents.h"
#include "robots/catoms3D/catoms3DMotionEngine.h"

#include "lightWalkCatoms3DBlockCode.hpp"

const int LightWalkCatoms3DBlockCode::ZLINE = 2;

LightWalkCatoms3DBlockCode::LightWalkCatoms3DBlockCode(Catoms3DBlock *host):
    Catoms3DBlockCode(host) {
    scheduler = getScheduler();
    world = BaseSimulator::getWorld();
    lattice = world->lattice;
    catom = host;
}


LightWalkCatoms3DBlockCode::~LightWalkCatoms3DBlockCode() {
}

void LightWalkCatoms3DBlockCode::onBlockSelected() {
    // cout << "greenLightIsOn: " << greenLightIsOn << endl;
}

void LightWalkCatoms3DBlockCode::startup() {
    stringstream info;
    info << "Starting ";

    relocated = false;
    if (not hasLeftNeighbor()) {
        // Catom has to get moving UPWARD then X-WARD
        /// Determine new anchor point at end of next motion.
        /// NO!!! Better : Propagate target position and let pivots decide.
        /// Therefore: inquire about pivot status
        targetPos = catom->position + Cell3DPosition(0, 0, 1);

        cout << *catom << " " << catom->position << endl;
        cout << "targetPos: " << targetPos << endl;
        catom->setColor(YELLOW);
        Catoms3DBlock *pivot = Catoms3DMotionEngine::findMotionPivot(catom, targetPos);
        VS_ASSERT(pivot);
        P2PNetworkInterface* itf = catom->getInterface(pivot->position);
        VS_ASSERT(itf);

        sendMessage(new ProbePivotLightStateMessage(catom->position, targetPos),
                    itf, MSG_DELAY_MC, 0);
    } else {
        catom->setColor(GREEN);

        // nothing to be done but wait for motion requests
    }

    initialized = true;
}

void LightWalkCatoms3DBlockCode::processReceivedMessage(MessagePtr msg,
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

void LightWalkCatoms3DBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;

    switch (pev->eventType) {
        case EVENT_RECEIVE_MESSAGE: {
            message =
                (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;

            if (message->isMessageHandleable()) {
                std::shared_ptr<HandleableMessage> hMsg =
                    (std::static_pointer_cast<HandleableMessage>(message));

                console << " received " << hMsg->getName() << " from "
                        << message->sourceInterface->hostBlock->blockId
                        << " at " << getScheduler()->now() << "\n";
                hMsg->handle(this);
            } else {
                P2PNetworkInterface * recv_interface = message->destinationInterface;

                // Handover to global message handler
                processReceivedMessage(message, recv_interface);
            }
        } break;

        case EVENT_ADD_NEIGHBOR: {
            if (catom->position[2] == ZLINE
                and initialized and awaitingModulePos != Cell3DPosition(-1, -1, -1)
                and not rotating) {
                uint64_t face = Catoms3DWorld::getWorld()->lattice->getOppositeDirection((std::static_pointer_cast<AddNeighborEvent>(pev))->face);
                const Cell3DPosition& pos = catom->getNeighborBlock(face)->position;

                if (pos[2] != ZLINE) {
                    // Neighbor is module moving on the line
                    greenLightIsOn = false;
                    catom->setColor(RED);
                } else {
                    // Module has taken the line tail position and is now pivot
                    setGreenLightAndResumeFlow();
                }
            }

            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            if (not rotating) {
                uint64_t face = Catoms3DWorld::getWorld()->lattice->getOppositeDirection((std::static_pointer_cast<RemoveNeighborEvent>(pev))->face);

                Cell3DPosition pos;
                if (catom->getNeighborPos(face, pos)
                    and ((pos[2] > ZLINE
                          and catom->getState() == BuildingBlock::State::ALIVE)
                         // Motion is not blocking for a module coming in
                         or (catom->getState() == BuildingBlock::State::ACTUATING
                             and actuationTargetPos[2] == ZLINE)) ) {
                    setGreenLightAndResumeFlow();
                }
            }

            break;
        }

        case EVENT_PIVOT_ACTUATION_START: {
            std::shared_ptr<PivotActuationStartEvent> pase = std::static_pointer_cast
                <PivotActuationStartEvent>(pev);

            if (greenLightIsOn) greenLightIsOn = false;

            catom->getNeighborPos(pase->toConP, actuationTargetPos);

            console << " started actuating for module #" << pase->mobile->blockId << "\n";
        } break;

        case EVENT_PIVOT_ACTUATION_END: {
            std::shared_ptr<PivotActuationEndEvent> paee = std::static_pointer_cast
                <PivotActuationEndEvent>(pev);

            console << " finished actuating for module #" << paee->mobile->blockId << "\n";
        } break;

        case EVENT_ROTATION3D_END: {
            rotating = false;

            if (catom->position[2] != ZLINE) {
                targetPos = catom->position +
                    (hasReachedLineTail() ?
                     Cell3DPosition(1, 0, -1) : Cell3DPosition(1, 0, 0));

                cout << targetPos << " " << hasReachedLineTail() << endl;

                Catoms3DBlock *pivot = Catoms3DMotionEngine::findMotionPivot(catom, targetPos);
                VS_ASSERT(pivot);

                sendMessage(new ProbePivotLightStateMessage(catom->position, targetPos),
                            catom->getInterface(pivot->position), MSG_DELAY_MC, 0);
            } else {
                greenLightIsOn = true;
                moduleAwaitingGo = false;
                relocated = true;
            }
        } break;

        case EVENT_TAP: {
        } break;

        case EVENT_INTERRUPTION: {
            std::shared_ptr<InterruptionEvent> itev =
                std::static_pointer_cast<InterruptionEvent>(pev);

            switch(itev->mode) {
                default: break;
            }
        } break;
    }
}

void LightWalkCatoms3DBlockCode::setGreenLightAndResumeFlow() {
    greenLightIsOn = true;
    catom->setColor(GREEN);

    if (moduleAwaitingGo) {
        bool nextToModule = isAdjacentToPosition(awaitingModulePos);
        P2PNetworkInterface* itf = nextToModule ?
            catom->getInterface(awaitingModulePos) :
            catom->getInterface(catom->position
                                + Cell3DPosition(-1, 0, 0));

        sendMessage(new GreenLightIsOnMessage(catom->position, awaitingModulePos),
                    itf, MSG_DELAY_MC, 0);
        moduleAwaitingGo = false;
    }

    if (not hasLeftNeighbor() and catom->getNbNeighbors() == 1
        and not relocated) {
        targetPos = catom->position + Cell3DPosition(0, 0, 1);

        cout << "pivots for " << catom->position
             << " moving to " << targetPos << endl;
        Catoms3DBlock *pivot =
            Catoms3DMotionEngine::findMotionPivot(catom, targetPos);
        VS_ASSERT(pivot);

        sendMessage(new ProbePivotLightStateMessage(catom->position,targetPos),
                    catom->getInterface(pivot->position), MSG_DELAY_MC, 0);
    }

}

bool LightWalkCatoms3DBlockCode::hasLeftNeighbor() const {
    return catom->getNeighborOnCell(catom->position + Cell3DPosition(-1, 0, 0)) != NULL;
}

bool LightWalkCatoms3DBlockCode::hasReachedLineTail() const {
    return catom->getNeighborOnCell(catom->position + Cell3DPosition(1, 0, -1)) == NULL;
}

bool LightWalkCatoms3DBlockCode::isLineTip() const {
    return catom->getNeighborOnCell(catom->position + Cell3DPosition(1, 0, 0)) == NULL;
}

bool LightWalkCatoms3DBlockCode::isAdjacentToPosition(const Cell3DPosition& pos) const {
    return lattice->cellsAreAdjacent(catom->position, pos);
}

Catoms3DBlock* LightWalkCatoms3DBlockCode::
findTargetLightAmongNeighbors(const Cell3DPosition& targetPos) const {
    for (const auto& cell : lattice->getActiveNeighborCells(catom->position)) {
        if (lattice->cellsAreAdjacent(cell, targetPos)
            and cell[2] == ZLINE and cell[0] > catom->position[0])
            return static_cast<Catoms3DBlock*>(lattice->getBlock(cell));
    }

    return NULL;
}
