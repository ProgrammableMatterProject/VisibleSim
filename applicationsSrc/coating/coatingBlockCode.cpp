/**
 * @file   coatingBlockCode.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Oct  9 16:57:13 2019
 *
 * @brief
 *
 *
 */

#include <iostream>
#include <set>
#include <limits>
#include <algorithm>

#include "catoms3DWorld.h"
#include "scheduler.h"
#include "events.h"
#include "trace.h"
#include "tDefs.h"
#include "configExporter.h"

#include "teleportationEvents.h"
#include "rotation3DEvents.h"
#include "catoms3DMotionEngine.h"
#include "color.h"

#include "coatingBlockCode.hpp"

using namespace Catoms3D;
using namespace MeshCoating;

CoatingBlockCode::CoatingBlockCode(Catoms3DBlock *host):
    Catoms3DBlockCode(host) {

    if (host) {
        scheduler = getScheduler();
        world = BaseSimulator::getWorld();
        lattice = world->lattice;
        catom = host;
    }
}

CoatingBlockCode::~CoatingBlockCode() {
}

bool CoatingBlockCode::parseUserCommandLineArgument(int argc, char *argv[]) {
    /* Reading the command line */
    if ((argc > 0) && (argv[0][0] == '-')) {
        switch(argv[0][1]) {
            // case 'b':   {
            //     BUILDING_MODE = true;

            //     argc--;
            //     argv++;

            //     return true;
            // } break;
            case '-': {
                string varg = string(argv[0] + 2); // argv[0] without "--"

                if (varg == string("highlight")) HIGHLIGHT_SCAFFOLD = true;
                if (varg == string("csg")) HIGHLIGHT_CSG = true;
                else return false;

                argc--;
                argv++;

                return true;
            }
        }
    }

    return false;
}

void CoatingBlockCode::onAssertTriggered() {
    onBlockSelected();
    catom->setColor(BLACK);
}

void CoatingBlockCode::onBlockSelected() {

    // Debug:
    cout << endl << "--- PRINT MODULE " << *catom << "---" << endl;
}

void CoatingBlockCode::startup() {
    stringstream info;
    info << "Starting ";

    // Progress Display
    static unsigned int lastNbModules = 0;
    lastNbModules = max(lastNbModules, lattice->nbModules);
    int ts = round(getScheduler()->now() / getRoundDuration());
    if (ts > 0)
        cout << TermColor::BWhite << "\rCurrent Timestep:\t"
             << TermColor::BMagenta << ts
             << TermColor::BWhite << "\t---\tCurrent #Modules:\t"
             << TermColor::BMagenta << lastNbModules
             << TermColor::Reset << std::flush;
}

void CoatingBlockCode::processReceivedMessage(MessagePtr msg,
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

void CoatingBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;

    if (BUILDING_MODE
        and scheduler->getState() == Scheduler::State::PAUSED)
        return; // Disable any module or blockcode alteration

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
            getScheduler()->trace(" ADD_NEIGHBOR ", catom->blockId, MAGENTA);
            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            break;
        }

        case EVENT_PIVOT_ACTUATION_START: {
        } break;

        case EVENT_PIVOT_ACTUATION_END: {
        } break;

        case EVENT_ROTATION3D_START: {
        } break;

        case EVENT_ROTATION3D_END: {
            getScheduler()->trace(" ROTATION3D_END ", catom->blockId, MAGENTA);
        } break;

        case EVENT_TAP: {
        } break;

        case EVENT_INTERRUPTION: {
            std::shared_ptr<InterruptionEvent> itev =
                std::static_pointer_cast<InterruptionEvent>(pev);

            switch(itev->mode) {

                case IT_MODE_TILEROOT_ACTIVATION: {
                } break;

                case IT_MODE_ALGORITHM_START: {
                    break;
                }
            }
        }
    }
}


const Cell3DPosition
CoatingBlockCode::norm(const Cell3DPosition& pos) {
    return pos - sbSeedPos;
}

const Cell3DPosition
CoatingBlockCode::denorm(const Cell3DPosition& pos) {
    return pos + sbSeedPos;
}

bool CoatingBlockCode::isInsideCSGFn(const Cell3DPosition& pos) const {
    return target->isInTarget(denorm(pos));
}

void CoatingBlockCode::initializeSandbox() {
    highlightCSGScaffold();

    const Cell3DPosition& ulb = world->lattice->getGridUpperBounds();
    for (int x = sbSeedPos[0]; x < ulb[0]; x+=B) {
        for (int y = sbSeedPos[1]; y < ulb[1]; y+=B) {
            const Cell3DPosition& trPos = Cell3DPosition(x, y, sbSeedPos[2]);

            for (int i = 0; i < XBranch; i++) {
                Cell3DPosition pos = trPos;
                for (int j = 0; j < 3; j++) {
                    pos += ruleMatcher->getIncidentTipRelativePos((BranchIndex)i);

                    if (lattice->isInGrid(pos)) {
                        world->addBlock(0, buildNewBlockCode, pos, GREY);
                        nbSandboxCatoms++;
                    }
                }
            }

            if (trPos != sbSeedPos) { // or i != ZBranch)
                Cell3DPosition futureTRPos = trPos
                    + ruleMatcher->getEntryPointRelativePos(Z_EPL);

                if (lattice->isInGrid(futureTRPos) and ruleMatcher->isInCSG(norm(trPos)))
                    world->addBlock(0, buildNewBlockCode, futureTRPos, YELLOW);
            }
        }
    }
}


void CoatingBlockCode::highlightCSGScaffold(bool debug) {
    // target->highlight();

    // Initialize Target Object Preview
    const Cell3DPosition& gs = world->lattice->gridSize;
    Cell3DPosition pos;
    for (short iz = 0; iz < gs[2]; iz++) {
        for (short iy = - iz / 2; iy < gs[1] - iz / 2; iy++) {
            for (short ix = - iz / 2; ix < gs[0] - iz / 2; ix++) {
                pos.set(ix, iy, iz);

                if (debug) {
                    if (target->isInTarget(pos))
                        lattice->highlightCell(pos, RED);
                } else {

                    if (not ruleMatcher->isInCSG(norm(pos))
                        // or (ruleMatcher->isInCSG(norm(pos)) and not ruleMatcher->isInCSG
                        //     (ruleMatcher->getTileRootPositionForMeshPosition(norm(pos)))))
                        )
                        continue;

                    if (HIGHLIGHT_CSG and target->isInTarget(pos))
                        lattice->highlightCell(pos, RED);

                    if (HIGHLIGHT_SCAFFOLD and ruleMatcher->isInCSG(norm(pos)))
                        lattice->highlightCell(pos, WHITE);
                }
            }
        }
    }

}
