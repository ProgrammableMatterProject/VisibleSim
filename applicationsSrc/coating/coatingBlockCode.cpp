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

    if (scaffoldSeedPos == Cell3DPosition(-1,-1,-1)) {
        scaffoldSeedPos = determineScaffoldSeedPosition();
        // cerr << "scaffoldSeedPos: " << scaffoldSeedPos << endl;
        if (scaffoldSeedPos == Cell3DPosition(-1,-1,-1)) {
            highlightCSGScaffold(true);
            VS_ASSERT_MSG(scaffoldSeedPos != Cell3DPosition(-1,-1,-1), "Cannot find where to place scaffold seed tile. Please check CSG placement.");
        }
    }

    VS_ASSERT_MSG(target, "Target is null, aborting...");


    // Initialize scaffold bounds
    if (X_MAX == numeric_limits<int>::min()) {
        // Initialize Scaffold bounds
        const Cell3DPosition& gs = world->lattice->gridSize;

        Cell3DPosition pos;
        for (short iz = 0; iz < gs[2]; iz++) {
            for (short iy = - iz / 2; iy < gs[1] - iz / 2; iy++) {
                for (short ix = - iz / 2; ix < gs[0] - iz / 2; ix++) {
                    pos.set(ix, iy, iz);

                    if (isInCSG(pos)) {
                        if (pos[0] > X_MAX) X_MAX = pos[0];
                        else if (pos[0] < X_MIN) X_MIN = pos[0];

                        if (pos[1] > Y_MAX) Y_MAX = pos[1];
                        else if (pos[1] < Y_MIN) Y_MIN = pos[1];

                        if (pos[2] > Z_MAX) Z_MAX = pos[2];
                        else if (pos[2] < Z_MIN) Z_MIN = pos[2];
                    }
                }
            }
        }
    }

    rm = new CoatingRuleMatcher(X_MAX - sbSeedPos[0],
                                         Y_MAX - sbSeedPos[1],
                                         Z_MAX - sbSeedPos[2],
                                         X_MIN - sbSeedPos[0],
                                         Y_MIN - sbSeedPos[1],
                                         Z_MIN - sbSeedPos[2],
                                         B,
                                         [this](const Cell3DPosition& pos) {
                                             return isInsideCSGFn(pos);
                                         });

    initialized = true;
    // if (not sandboxInitialized)
    //     initializeSandbox();

    coordinatorPos =
        denorm(rm->getNearestTileRootPosition(norm(catom->position)));


    // // Will not be used, set green and forget about it
    // // FIXME:
    // if (not rm->isInGrid(norm(coordinatorPos))) {

    //     if (rm->isInMesh(norm(catom->position)))
    //         SET_GREEN_LIGHT(true);

    //     catom->setColor(GREY);

    //     return;
    // }


//     // need to initialize target light for sandbox modules at algorithm start
//     if (rm->isInSandbox(norm(catom->position))) {
//         // All other modules should be green (set by default)
//         SET_GREEN_LIGHT(true);

// }

    if (catom->position == scaffoldSeedPos) {
        spawnLoc = catom->position - Cell3DPosition(1,1,1);
        lattice->highlightCell(spawnLoc, MAGENTA);

        scheduler->schedule(new InterruptionEvent(getScheduler()->now() + getRoundDuration(),
                                                  catom, IT_MODULE_INSERTION));
    } else if (catom->position == spawnLoc) {
        scheduleRotationTo(catom->position + Cell3DPosition(1,0,0));
    }
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

        case EVENT_REMOVE_NEIGHBOR:
        case EVENT_PIVOT_ACTUATION_START:
        case EVENT_PIVOT_ACTUATION_END:
        case EVENT_ROTATION3D_START:
        case EVENT_TAP:
            break;

        case EVENT_ROTATION3D_END: {
            getScheduler()->trace(" ROTATION3D_END ", catom->blockId, MAGENTA);

            if (catom->position[2] < scaffoldSeedPos[2]) {
                if (lattice->isFree(catom->position + Cell3DPosition(0,0,1))) {
                    scheduleRotationTo(catom->position + Cell3DPosition(0,0,1));
                } else {
                    scheduleRotationTo(catom->position + Cell3DPosition(-1,-1,2));
                }
            } else {
                if (isInCoatingLayer(0)) {
                    catom->setColor(ORANGE);
                } else {
                    // Move around border until encountering a coating position
                    const Cell3DPosition& nextCoatingPos =
                        catom->position + Cell3DPosition(1, 1, -1);

                    // if (not isInCSG(nextCoatingPos)
                        // and isInCSG(nextCoatingPos + )) {}

                    if (lattice->isFree(nextCoatingPos))
                        scheduleRotationTo(nextCoatingPos);
                    else scheduleRotationTo(catom->position + Cell3DPosition(1, 0, 0));
                }
            }
        } break;

        case EVENT_INTERRUPTION: {
            std::shared_ptr<InterruptionEvent> itev =
                std::static_pointer_cast<InterruptionEvent>(pev);

            switch(itev->mode) {

                case IT_MODULE_INSERTION: {
                    if (lattice->isFree(spawnLoc))
                        world->addBlock(0, buildNewBlockCode, spawnLoc, YELLOW);

                    getScheduler()->schedule(
                        new InterruptionEvent(getScheduler()->now() +
                                              3 * (getRoundDuration()),
                                              catom, IT_MODULE_INSERTION));
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
    return isInCSG(denorm(pos));
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
                    pos += rm->getIncidentTipRelativePos((BranchIndex)i);

                    if (lattice->isInGrid(pos)) {
                        world->addBlock(0, buildNewBlockCode, pos, GREY);
                    }
                }
            }

            if (trPos != sbSeedPos) { // or i != ZBranch)
                Cell3DPosition futureTRPos = trPos
                    + rm->getEntryPointRelativePos(Z_EPL);

                if (lattice->isInGrid(futureTRPos) and rm->isInCSGScaffold(norm(trPos)))
                    world->addBlock(0, buildNewBlockCode, futureTRPos, YELLOW);
            }
        }
    }
}

Cell3DPosition CoatingBlockCode::determineScaffoldSeedPosition() {
    // const Cell3DPosition& glb = world->lattice->getGridLowerBounds();
    const Cell3DPosition& ulb = world->lattice->getGridUpperBounds();

    Cell3DPosition pos;
    const Cell3DPosition &sctPos = sbSeedPos;

    // Scan base for lowest XY
    for (short ix = sctPos[0]; ix < ulb[0]; ix+=B) {
        for (short iy = sctPos[1]; iy < ulb[1]; iy+=B) {
            // MAYBE: Only diagonal needs to be considered
            if (ix == iy) {
                pos.set(ix, iy, sctPos[2]); // 3 is grid base over scaffold
                // lattice->highlightCell(pos, ORANGE);
                if (isInCSG(pos)) {
                    // lattice->highlightCell(pos, MAGENTA);
                    return pos;
                }
            }
        }
    }

    return Cell3DPosition(-1, -1, -1);
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
                    if (isInCSG(pos))
                        lattice->highlightCell(pos, RED);
                } else {

                    if (not rm->isInCSGScaffold(norm(pos))
                        // or (rm->isInCSGScaffold(norm(pos)) and not rm->isInCSGScaffold
                        //     (rm->getTileRootPositionForMeshPosition(norm(pos)))))
                        )
                        continue;

                    if (HIGHLIGHT_CSG and isInCSG(pos))
                        lattice->highlightCell(pos, RED);

                    if (HIGHLIGHT_SCAFFOLD and rm->isInCSGScaffold(norm(pos)))
                        lattice->highlightCell(pos, WHITE);
                }
            }
        }
    }
}

void
CoatingBlockCode::scheduleRotationTo(const Cell3DPosition& pos, Catoms3DBlock* pivot) {
    try {
        if (not pivot) pivot = Catoms3DMotionEngine::findMotionPivot(catom, pos);

        // OUTPUT << "mvmt: " << round((scheduler->now()) / getRoundDuration()) << "\t" << endl;
        // cout << "[t-" << scheduler->now() << "] rotation scheduled" << endl;
        scheduler->schedule(new Rotation3DStartEvent(getScheduler()->now(),
                                                     catom, pivot, pos,
                                                     RotationLinkType::HexaFace, false));
#ifdef INTERACTIVE_MODE
        awaitKeyPressed();
#endif
    } catch (const NoAvailableRotationPivotException& e_piv) {
        cerr << e_piv.what();
        cerr << "target position: " << pos << endl;
        catom->setColor(BLUE);
    } catch (std::exception const& e) {
        cerr << "exception: " << e.what() << endl;
    }

}

bool CoatingBlockCode::isInCoatingLayer(const int layer) const {
    if (catom->position[2] != scaffoldSeedPos[2] + layer)
        return false;

    for (const Cell3DPosition& p : lattice->getNeighborhood(catom->position)) {
        if (p[2] == catom->position[2] and isInCSG(p)) return true;
    }

    for (const Cell3DPosition& pRel : diagNeighbors) {
        if (isInCSG(pRel + catom->position)) return true;
    }

    return false;
}

const Cell3DPosition getNextOpenCoatingSlot() const {

}
