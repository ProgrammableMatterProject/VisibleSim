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
#include <unistd.h>

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

bool CoatingBlockCode::parseUserCommandLineArgument(int &argc, char **argv[]) {
    /* Reading the command line */
    if ((argc > 0) && ((*argv)[0][0] == '-')) {
        switch((*argv)[0][1]) {

            // case 'b':   {
            //     BUILDING_MODE = true;
            //     return true;
            // } break;

            case '-': {
                string varg = string((*argv)[0] + 2); // argv[0] without "--"
                if (varg == string("highlight")) HIGHLIGHT_SCAFFOLD = true;
                else if (varg == string("csg")) HIGHLIGHT_CSG = true;
                else if (varg == string("coating")) {
                    try {
                        HIGHLIGHT_layer = stoi((*argv)[1]);
                        argc--;
                        (*argv)++;
                    } catch(std::logic_error&) {}

                    HIGHLIGHT_COATING = true;
                } else if (varg == string("resources")) HIGHLIGHT_RES = true;
                // else if (varg == string("pyramid")) PYRAMID_MODE = true;
                else return false;

                return true;
            }

            default:
                cerr << "Unrecognized command line argument: " << (*argv)[0] << endl;
        }
    }

    return false;
}

void CoatingBlockCode::onAssertTriggered() {
    // onBlockSelected();
    catom->setColor(BLACK);
}

void CoatingBlockCode::onBlockSelected() {

    // Debug:
    cerr << endl << "--- PRINT MODULE " << *catom << "---" << endl;

    if (catom->position == spawnPivot) {
        cerr << "currentLayer: " << currentLayer << endl;
        cerr << "topCoatingLayer: " << topCoatingLayer << endl;
        cerr << "spawnCount: " << spawnCount << " / "
             << getResourcesForCoatingLayer(currentLayer) << endl;
    }

    // cerr << "getResourcesForCoatingLayer(" << currentLayer << "): "
    //      << getResourcesForCoatingLayer(currentLayer) << endl;

    // cerr << "getAllReachablePositions: " << endl;
    // const vector<Cell3DPosition>& reachablePositions = Catoms3DMotionEngine::getAllReachablePositions(catom);
    // for (const Cell3DPosition& p : reachablePositions) {
    //     cerr << p << endl;
    //     if (not highlightedReachablePos)
    //         lattice->highlightCell(p, PINK);
    //     else lattice->unhighlightCell(p);
    // }
    // highlightedReachablePos = not highlightedReachablePos;

    // nextRotationTowards(trainStart);
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

        // useExternalCoatingOnOddLayers = shouldUseExternalCoatingOnOddLayers();
        // highlightCSGScaffold();
        coatingSeed = determineCoatingSeedPosition();

        spawnLoc = cornerTilePos + Cell3DPosition(-1, -1, -1);
        spawnPivot = cornerTilePos + Cell3DPosition(0, 0, -2);
        spawnBTip = cornerTilePos + Cell3DPosition(0, 0, -1);

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

    topCoatingLayer = determineTopCoatingHeight();

    if (catom->position == spawnLoc) {
        P2PNetworkInterface* spawnPivotItf = catom->getInterface(spawnPivot);

        sendMessage(new CoaTrainRequest(), spawnPivotItf, MSG_DELAY_MC, 0);
    } else if (catom->position == scaffoldSeedPos) {
        scheduler->schedule(new InterruptionEvent(getScheduler()->now() + getRoundDuration(),
                                                  catom, IT_MODULE_INSERTION));
    } else if (catom->position == spawnPivot) {
        catom->setColor(GREEN);

        useExternalCoatingOnOddLayers = shouldUseExternalCoatingOnOddLayers();
        cout << "useExternalCoatingOnOddLayers: " << useExternalCoatingOnOddLayers << endl;
        highlightCSGScaffold();
        initializeClosingCornerAndFBPLocations(closingCorner, firstBorderPos);

        // trainStart = isInCSG(cornerTilePos) ?
        //     closingCorner[0] + Cell3DPosition(1, -1, 1)
        //     : closingCorner[0] + Cell3DPosition(1, -2, 1);

        trainStart = isInCSG(cornerTilePos) ?
            firstBorderPos[0] + Cell3DPosition(0, -1, 1)
            : firstBorderPos[0] + Cell3DPosition(0, 0, 1);

        lattice->highlightCell(coatingSeed, LIGHTBLUE);
        lattice->highlightCell(spawnLoc, GREY);
        lattice->highlightCell(trainStart, GREEN);
        // lattice->highlightCell(spawnPivot, YELLOW);

        if (not isInCSG(cornerTilePos)) {
            // an additional module might be needed along the daigonal of the seed tile,
            //  for modules to reach the first navigable border position. Call in this module
            world->addBlock(0, buildNewBlockCode, ZHelperPos, LIGHTBLUE);
        }
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
            // getScheduler()->trace(" ADD_NEIGHBOR ", catom->blockId, MAGENTA);
            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            if (pendingPlanning) {
                catom->setColor(YELLOW);
                pendingPlanning = false;
                scheduleRotationTo(nextRotationTowards(pendingPlanningDest,
                                                       pendingPlanningAllowsDirectMotion));
            }

            break;
        }

        case EVENT_PIVOT_ACTUATION_START:
        case EVENT_PIVOT_ACTUATION_END:
        case EVENT_ROTATION3D_START:
            lastPos = catom->position;
        case EVENT_TAP:
            break;

        case EVENT_ROTATION3D_END: {
            step++;
            // getScheduler()->trace(" ROTATION3D_END ", catom->blockId, MAGENTA);

            if (calledInToSupportLocation) {
                if (catom->position != supportLocation)
                    scheduleRotationTo(nextRotationTowards(supportLocation));
                else catom->setColor(LIGHTBLUE);

                return;
            }

            if (catom->position == trainStart
                // or (not isInCSG(cornerTilePos)
                //     and catom->position == closingCorner[currentLayer]))
                ) {
                info << " got onboard the coatrain!";
                scheduler->trace(info.str(),catom->blockId, PINK);

                isOnTheCoatrain = true;
            } else if (not isInCSG(cornerTilePos)
                       and IS_EVEN(currentLayer)
                       and not passedThroughCC
                       and catom->position == closingCorner[currentLayer]) {
                passedThroughCC = true;
                info << " passed through the closing corner";
                scheduler->trace(info.str(),catom->blockId, PINK);

                // Claim first border position if next to it
                Cell3DPosition potentialOpenSlot;
                if (hasOpenCoatingSlotNeighbor(currentLayer, potentialOpenSlot)) {
                    stringstream info;
                    info << " moving to open coating slot at " << potentialOpenSlot;
                    scheduler->trace(info.str(),catom->blockId,CYAN);

                    scheduleRotationTo(potentialOpenSlot);
                    return;
                }
            }


            if (catom->position[2] < scaffoldSeedPos[2]
                or currentLayer < topCoatingLayer // or PYRAMID_MODE
                or isOnTheCoatrain) {

                bool isClosingCorner = catom->position == closingCorner[currentLayer];
                if (isOnTheCoatrain) {

                    if (isInCoatingLayer(catom->position, currentLayer)
                        and (not isClosingCorner or
                             (isClosingCorner
                              and closingCornerInsertionReady(closingCorner[currentLayer])))) {

                        if (isClosingCorner) {
                            handleClosingCornerInsertion();
                        } else {
                            catom->setColor(ORANGE);
                        }

                    } else {
                        scheduleNextBorderMotion();
                    }

                } else {

                    if (isInCoatingLayer(catom->position, currentLayer)
                        and coatingSlotInsertionReady(catom->position)
                        and not isClosingCorner)  {
                        catom->setColor(ORANGE);

                    } else {
                        if (getCoatingLayer(closingCorner[currentLayer]) < topCoatingLayer
                            or verticalLayerShouldOffset(currentLayer)) {
                            // const Cell3DPosition& firstBorderPos = trainStart
                            //     + Cell3DPosition(0,1,-1);
                            if (Catoms3DMotionEngine::canMoveTo(catom, firstBorderPos[0])
                                and lattice->isFree(firstBorderPos[0])) {
                                scheduleRotationTo(firstBorderPos[0]);
                            } else if (getResourcesForCoatingLayer(currentLayer) == 1) {
                                scheduleRotationTo(nextRotationTowards(
                                                       closingCorner[currentLayer]));
                            } else if (isInCSG(cornerTilePos)
                                       or (not isInCSG(cornerTilePos) and
                                           ((IS_ODD(currentLayer)) or
                                            (IS_EVEN(currentLayer) and passedThroughCC)))) {
                                scheduleRotationTo(nextRotationTowards(trainStart));
                            } else {
                                scheduleRotationTo(nextRotationTowards(
                                                       closingCorner[currentLayer]));
                            }

                        } else {
                            bool allowDirectMotion =
                                isAdjacentToPosition(closingCorner[currentLayer])
                                and lattice->getBlock(closingCorner[currentLayer]
                                                      + Cell3DPosition(1, 0, 0));
                            scheduleRotationTo(nextRotationTowards(closingCorner[currentLayer],
                                                                   allowDirectMotion));
                        }
                    }
                }

            } else {

                if (catom->position[1] == closingCorner[currentLayer][1]
                    and catom->position[2] == closingCorner[currentLayer][2]
                    and catom->position[0] != closingCorner[currentLayer][0]) {
                    const Cell3DPosition& rPos = catom->position + Cell3DPosition(1, 0, 0);
                    if (not isInCoatingLayer(rPos, topCoatingLayer)
                        or lattice->getBlock(rPos))
                        // Claim current coating position
                        catom->setColor(BLUE);
                    else
                        scheduleRotationTo(rPos);

                } else if (catom->position == closingCorner[currentLayer]) {
                    if (lattice->getBlock(closingCorner[currentLayer]+Cell3DPosition(1,0,0))) {
                        handleClosingCornerInsertion();
                    } else {
                        const Cell3DPosition& rPos = catom->position + Cell3DPosition(1, 0, 0);
                        scheduleRotationTo(rPos);
                    }

                } else {
                    scheduleRotationTo(
                        nextRotationTowards(closingCorner[currentLayer]));
                }
            }
        } break;

        case EVENT_INTERRUPTION: {
            std::shared_ptr<InterruptionEvent> itev =
                std::static_pointer_cast<InterruptionEvent>(pev);

            switch(itev->mode) {

                case IT_MODULE_INSERTION: {
                    if (not passNextSpawnRound
                        and lattice->isFree(spawnLoc)
                        and (lastSpawnedModuleBc == NULL or lastSpawnedModuleBc->step >= 2)) {
                        world->addBlock(0, buildNewBlockCode, spawnLoc, YELLOW);

                        lastSpawnedModuleBc = static_cast<CoatingBlockCode*>(
                            lattice->getBlock(spawnLoc)->blockCode);
                    }

                    if (passNextSpawnRound) passNextSpawnRound = false;

                    if (not coatingIsOver)
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

Cell3DPosition CoatingBlockCode::determineCoatingSeedPosition() const {
    // Assume for now that the CSG object and scaffold are always placed so that
    //  the border is adjacent to the first tile along the XY diagonal of the sandbox
    // We seek here to locate the seed closing corner.

    // WARNING: This will not work for some CSG objects with differences as the border will
    //  not be there.
    const Cell3DPosition& seedTr = cornerTilePos;

    // Move to the first border position from seedTR
    // If the tile is in the object, that would mean that the border is further out
    //  otherwise, it would be further in
    Cell3DPosition curPos = seedTr;
    if (isInCSG(seedTr)) {
        curPos += Cell3DPosition(-1, -1, 0);
    } else {
        do {
            curPos += Cell3DPosition(1, 1, 0);
            lattice->highlightCell(curPos);
        } while (lattice->isInGrid(curPos) and not isInCoatingLayer(curPos, 0));
    }

    // Assumption: This should be a corner
    VS_ASSERT(isCoatingCorner(curPos));

    return curPos;
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

void
CoatingBlockCode::scheduleRotationTo(const Cell3DPosition& pos, Catoms3DBlock* pivot) {
    if (pos == Cell3DPosition(-1, -1, -1)) // ERR pos
        return;

    try {
        if (not pivot) pivot = Catoms3DMotionEngine::findMotionPivot(catom, pos);

        // OUTPUT << "mvmt: " << round((scheduler->now()) / getRoundDuration()) << "\t" <<endl;
        // cout << "[t-" << scheduler->now() << "] rotation scheduled" << endl;

        if (lattice->isFree(pos))
            scheduler->schedule(new Rotation3DStartEvent(getScheduler()->now(),
                                                         catom, pivot, pos,
                                                         RotationLinkType::HexaFace, false));
        else catom->setColor(RED);

#ifdef INTERACTIVE_MODE
        awaitKeyPressed();
#endif
    } catch (const NoAvailableRotationPivotException& e_piv) {
        // cerr << e_piv.what();
        // cerr << "target position: " << pos << endl;
        cerr << "Couldn't find pivot for moving module from " << catom->position
             << " to " << pos << endl;
        catom->setColor(GREY);
    } catch (std::exception const& e) {
        cerr << "exception: " << e.what() << endl;
    }

}

unsigned int CoatingBlockCode::getCoatingLayer(const Cell3DPosition& pos) const {
    return pos[2] - scaffoldSeedPos[2];
}

bool CoatingBlockCode::hasOpenCoatingSlotNeighbor(const unsigned int layer,
                                                  Cell3DPosition &openSlot) const {
    for (const Cell3DPosition& p : Catoms3DMotionEngine::getAllReachablePositions(catom)) {
        stringstream info;
        info << " checking openCoatingSlotNeighbor: " << p;
        info << " - l: " << layer;
        info << " - cl: " << getCoatingLayer(p);
        info << " - isICL: " << isInCoatingLayer(p, layer);
        info << " - u: " << useExternalCoatingOnOddLayers;
        if (isInCoatingLayer(p, layer)) info << " - r: " << coatingSlotInsertionReady(p);
        scheduler->trace(info.str(),catom->blockId,WHITE);

        if (getCoatingLayer(p) == layer
            and isInCoatingLayer(p, layer)
            and coatingSlotInsertionReady(p)) {
            openSlot = p;
            return true;
        }
    }

    return false;
}

bool CoatingBlockCode::isInCoating(const Cell3DPosition& pos) const {
    return pos[2] >= scaffoldSeedPos[2] and isInCoatingLayer(pos, getCoatingLayer(pos));
}

bool CoatingBlockCode::isInCoatingLayer(const Cell3DPosition& pos,
                                        const unsigned int layer) const {
    return IS_EVEN(layer) ? isInRegularCoatingLayer(pos, layer) :
        (useExternalCoatingOnOddLayers ?
         isInOffsetCoatingLayer(pos, layer) : isInRegularCoatingLayer(pos, layer));
}

bool CoatingBlockCode::isInRegularCoatingLayer(const Cell3DPosition& pos,
                                               const unsigned int layer) const {
    if (layer != getCoatingLayer(pos) or isInCSG(pos))
        return false;

    return hasNeighborInCSG(pos);
}

bool CoatingBlockCode::isInOffsetCoatingLayer(const Cell3DPosition& pos,
                                              const unsigned int layer) const {
    if (pos == Cell3DPosition(4,3,4)) {
        stringstream info;
        info << " ICL: " << pos;
        info << " - l: " << (layer == getCoatingLayer(pos));
        info << " - csg: " << isInCSG(pos);
        info << " - n1: " << not hasNeighborInCSG(pos);
        info << " - n2: " << has2ndOrderNeighborInCSG(pos);
        scheduler->trace(info.str(),catom->blockId,WHITE);
    }

    if (layer != getCoatingLayer(pos) or isInCSG(pos))
        return false;

    return not hasNeighborInCSG(pos) and has2ndOrderNeighborInCSG(pos);
}

bool CoatingBlockCode::hasNeighborInCSG(const Cell3DPosition& pos) const {
    for (const Cell3DPosition& p : lattice->getNeighborhood(pos)) {
        if (isInCSG(p)) return true;
    }

    for (const Cell3DPosition& pRel : diagNeighbors) {
        if (isInCSG(pRel + pos)) return true;
    }

    return false;
}

bool CoatingBlockCode::has2ndOrderNeighborInCSG(const Cell3DPosition& pos) const {
    // if (HIGHLIGHT_RES) {
    //     lattice->highlightCell(pos, WHITE);
    // }

    for (const Cell3DPosition& pRel : _2ndOrderNeighbors) {
        // if (HIGHLIGHT_RES) {
        //     lattice->highlightCell(pRel + pos, RED);
        //     usleep(500000);
        // }

        if (isInCSG(pRel + pos)) return true;
    }

    // if (HIGHLIGHT_RES) {
    //     lattice->unhighlightCell(pos);

    return false;
}

const vector<CCWDir> CoatingBlockCode::getCCWDirectionsFrom(const CCWDir cwd) const {
    vector<CCWDir> cwDirs;

    // cout << CCWDir_to_string(cwd) << endl;

    for (int cwi = cwd; cwi < NumCCWDirs; cwi++)
        cwDirs.push_back((CCWDir)cwi);
    for (int cwi = 0; cwi < cwd; cwi++)
        cwDirs.push_back((CCWDir)cwi);

    // stringstream info;
    // for (int i = 0; i < NumCCWDirs; i++)
    //     info << CCWDir_to_string(cwDirs[i]) << " " ;
    // scheduler->trace(info.str(),catom->blockId,BLUE);


    return cwDirs;
}

string CoatingBlockCode::CCWDir_to_string(const CCWDir d) const {
    switch(d) {
        case FrontLeft: return "FrontLeft";
        case Front: return "Front";
        case FrontRight: return "FrontRight";
        case Right: return "Right";
        case RearRight: return "RearRight";
        case Rear: return "Rear";
        case RearLeft: return "RearLeft";
        case Left: return "Left";
    }

    return "err";
}

size_t CoatingBlockCode::getResourcesForCoatingLayer(const unsigned int layer) {
    // Check cache
    if (resourcesForCoatingLayer.size() > layer)
        return resourcesForCoatingLayer[layer];

    int count = 0;
    // Virtually navigate around the object to count necessary resources
    if (layer < topCoatingLayer) {
        Cell3DPosition currentPos = closingCorner[layer];
        CCWDir lastCwd = FrontLeft;

        do {
            if (HIGHLIGHT_RES) {
                // cout << currentPos << endl;
                lattice->highlightCell(currentPos, RED);
                // cout << (count + 1) << endl;
                usleep(500000);
                lattice->unhighlightCell(currentPos);
            }

            currentPos = findNextBorderLocationFrom(currentPos, lastCwd, true,
                                                    [this, layer](const Cell3DPosition& p) {
                                                        return isInCoatingLayer(p, layer);
                                                    });
            count++;
        } while (currentPos != closingCorner[layer]);

        if (count == 0 and isInCoatingLayer(currentPos, layer)) count++; // FIXME:

    } else { // Simply scan the grid for this layer and return count
        const Cell3DPosition& gs = world->lattice->gridSize;
        Cell3DPosition pos;
        short iz = topCoatingLayer + scaffoldSeedPos[2];
        for (short iy = - iz / 2; iy < gs[1] - iz / 2; iy++) {
            for (short ix = - iz / 2; ix < gs[0] - iz / 2; ix++) {
                pos.set(ix, iy, iz);
                if (isInCoatingLayer(pos, layer)) {
                    if (HIGHLIGHT_RES) {
                        lattice->highlightCell(pos, RED);
                        usleep(50000);
                        lattice->unhighlightCell(pos);
                    }

                    count++;
                }
            }
        }
    }

    // Add to cache
    resourcesForCoatingLayer.push_back(count);

    return count;
}

Cell3DPosition CoatingBlockCode::
findNextBorderLocationFrom(const Cell3DPosition& pos, CCWDir &lastCCWD, bool ignoreDiag,
                           std::function<bool (const Cell3DPosition&)> cond) const {
    vector<CCWDir> cwdPosCustom = getCCWDirectionsFrom(lastCCWD);
    for (int i = 0; i < NumCCWDirs; i++) {
        const Cell3DPosition& cwdPos = CCWDPos[cwdPosCustom[i]];
        const Cell3DPosition& tPos = cwdPos + pos;

        if (ignoreDiag and IS_EVEN(i)) continue; // ignore diag neighbors to avoid shortcuts

        // stringstream info;
        // info << " considering " << tPos
        //      << "(" << CCWDir_to_string(cwdPosCustom[i]) << ")";
        // info << " -- cond: " << cond(tPos);
        // scheduler->trace(info.str(),catom->blockId,WHITE);

        if (cond(tPos)) {
            // Set direction to the opposite of the last direction + 1
            //  (acts like an edge between the next coating pos and the last
            // http://www.imageprocessingplace.com/downloads_V3/root_downloads/tutorials/contour_tracing_Abeer_George_Ghuneim/ray.html
            lastCCWD = (CCWDir)((cwdPosCustom[i] + NumCCWDirs / 2 + 1) % NumCCWDirs);

            return tPos;
        }
    }

    // VS_ASSERT(false);

    return pos;
}

void CoatingBlockCode::scheduleNextBorderMotion(bool useInnerBorder) {
    // Move around border until encountering a coating position
    Cell3DPosition potentialOpenSlot;
    if (hasOpenCoatingSlotNeighbor(currentLayer, potentialOpenSlot)) {
        stringstream info;
        info << " moving to open coating slot at " << potentialOpenSlot;
        scheduler->trace(info.str(),catom->blockId,WHITE);

        scheduleRotationTo(potentialOpenSlot);
    } else {
        if (not isInCSG(cornerTilePos)
            and (catom->position == trainStart
                 or catom->position == trainStart + Cell3DPosition(0, -1, 0)
                 or catom->position == trainStart + Cell3DPosition(0, -2, 0)))
            lastCCWDir = Front;


        CCWDir prevCCWDir = lastCCWDir;
        Cell3DPosition tPos =
            findNextBorderLocationFrom(catom->position, lastCCWDir, false,
                                       [this](const Cell3DPosition& p){
                                           return Catoms3DMotionEngine::canMoveTo(catom, p)
                                               and p != lastPos;
                                       });

        stringstream info;
        info << " moving to next position around border: "
             << tPos << "(" << CCWDir_to_string(prevCCWDir) << ")";
        info << " --> " << CCWDir_to_string(lastCCWDir);
        scheduler->trace(info.str(),catom->blockId,ORANGE);

        scheduleRotationTo(tPos);
    }

}

void CoatingBlockCode::handleClosingCornerInsertion() {
    // Update variables and GUI
    catom->setColor(CYAN);
    lattice->unhighlightCell(catom->position);
    // if (HIGHLIGHT_COATING
    //     and (HIGHLIGHT_layer == -1 or (int)getCoatingLayer(trainStart) == HIGHLIGHT_layer))
    //     lattice->highlightCell(trainStart, ORANGE); // Revert to orange
    // else
        lattice->unhighlightCell(trainStart);

    // if (currentLayer < topCoatingLayer)
    //     closingCorner += (IS_ODD(currentLayer) or PYRAMID_MODE) ?
    //         Cell3DPosition(0,0,1) : Cell3DPosition(-1,-1,1);
    // else
    // closingCorner += Cell3DPosition(0, 1, 0);

    if (currentLayer == topCoatingLayer - 1) {
        coatingIsOver = true;
        return; // FIXME:
    }

    /// Bring up next choo-choo
    if (currentLayer < topCoatingLayer) {
        forwardPTNLToSpawnPivot();

        trainStart = firstBorderPos[currentLayer + 1] + Cell3DPosition(0, -1, 1);

        // trainStart = isInCSG(cornerTilePos) ?
        //     firstBorderPos[currentLayer + 1] + Cell3DPosition(0, -1, 1)
        //     : firstBorderPos[currentLayer + 1] + Cell3DPosition(0, 0, 1);

        lattice->highlightCell(closingCorner[currentLayer + 1], CYAN);

        if (currentLayer < (topCoatingLayer - 1)) lattice->highlightCell(trainStart, GREEN);
    }
}

void CoatingBlockCode::forwardPTNLToSpawnPivot() {
    // Attempt sending to SpawnPivot
    P2PNetworkInterface* PTNL_itf = catom->getInterface(spawnPivot);

    if (PTNL_itf == NULL)     // Attempt sending  to SpawnBTip
        PTNL_itf = catom->getInterface(spawnBTip);

    if (PTNL_itf == NULL) { // Choose next hop down the corner edge
        PTNL_itf = getInterfaceToClosingCornerBelow();
    }

    if (PTNL_itf == NULL) {
        VS_ASSERT(not isInCSG(cornerTilePos)); // locate vertical branch tip from corner tile
        if (catom->position == cornerTilePos + Cell3DPosition(1, 1, 0)) // FIXME:
            PTNL_itf = catom->getInterface(cornerTilePos + Cell3DPosition(1, 1, -1));
        else if (catom->position == ZHelperPos)
            // send to RevZ branch tip - 1
            PTNL_itf = catom->getInterface(catom->position + Cell3DPosition(0, 0, -1));
        else if (catom->position == ZHelperPos + Cell3DPosition(0, 0, -1))
            // send to RevZ branch tip
            PTNL_itf = catom->getInterface(catom->position + Cell3DPosition(-1, -1, 1));
        else // forward towards one of the branch tips between self and spawnBTip
            PTNL_itf = catom->getInterface(catom->position + Cell3DPosition(0, -1, 0));
    }

    VS_ASSERT_MSG(PTNL_itf != NULL,
                  "Closing corner coulnd't send ProceedToNextLayer");
    sendMessage(new ProceedToNextLayer(), PTNL_itf, MSG_DELAY_MC, 0);
}

Cell3DPosition CoatingBlockCode::nextRotationTowards(const Cell3DPosition& dest,
                                                     bool allowDirectMotion) {
    stringstream info;
    info << " planning rotation to " << dest;
    scheduler->trace(info.str(),catom->blockId, ORANGE);

    if (allowDirectMotion and Catoms3DMotionEngine::canMoveTo(catom, dest))
        return dest;

    // deconst
    vector<Cell3DPosition> reachablePositions =
        vector<Cell3DPosition>(Catoms3DMotionEngine::getAllReachablePositions(catom));

    if (reachablePositions.size() == 0) {
        // reattempt on next neighbor removal
        catom->setColor(RED);
        pendingPlanning = true;
        pendingPlanningAllowsDirectMotion = allowDirectMotion;
        pendingPlanningDest = dest;
        return Cell3DPosition(-1, -1, -1);
    }

    // cout << "unsorted: " << endl;
    // for (const Cell3DPosition& p : reachablePositions) {
    //     cout << p << endl;
    // }

    // Find location with minimum z distance from current position
    sort(reachablePositions.begin(), reachablePositions.end(),
         [dest](const Cell3DPosition& p1, const Cell3DPosition& p2) -> bool
         {
             return abs(dest[2] - p1[2]) < abs(dest[2] - p2[2]);
         });

    // cout << "sorted: " << endl;
    // for (const Cell3DPosition& p : reachablePositions) {
    //     cout << p << endl;
    // }

    // Reduce to just closest z results
    int zOp = (reachablePositions[0])[2]; // cout << zOp << endl;
    reachablePositions.erase(remove_if(reachablePositions.begin(), reachablePositions.end(),
                                       [zOp](const Cell3DPosition& p) -> bool
                                       {
                                           return p[2] != zOp;
                                       }),
                             reachablePositions.end());

    // cout << "filtered: " << endl;
    // for (const Cell3DPosition& p : reachablePositions) {
    //     cout << p << endl;
    // }

    // Then order by x+y distance
    sort(reachablePositions.begin(), reachablePositions.end(),
         [dest](const Cell3DPosition& p1, const Cell3DPosition& p2) -> bool
         {
             return (abs(dest[0] - p1[0]) + abs(dest[1] - p1[1]))
                 < (abs(dest[0] - p2[0]) + abs(dest[1] - p1[1]));
         });

    // cout << "rest: " << endl;
    // for (const Cell3DPosition& p : reachablePositions) {
    //     cout << p << endl;
    // }

    // cout << "best: " << reachablePositions[0] << endl;

    return reachablePositions[0] == lastPos ? reachablePositions[1] : reachablePositions[0];
}

size_t CoatingBlockCode::determineTopCoatingHeight() const {
    size_t topCSGHeight = 0;

    const Cell3DPosition& gs = world->lattice->gridSize;
    Cell3DPosition pos;
    for (short iz = 0; iz < gs[2]; iz++) {
        for (short iy = - iz / 2; iy < gs[1] - iz / 2; iy++) {
            for (short ix = - iz / 2; ix < gs[0] - iz / 2; ix++) {
                pos.set(ix, iy, iz);

                if (isInCSG(pos) and (unsigned)iz > topCSGHeight)
                    topCSGHeight = iz;
            }
        }
    }

    return (topCSGHeight + 1) - scaffoldSeedPos[2];
}

bool CoatingBlockCode::isAdjacentToPosition(const Cell3DPosition& pos) const {
    return lattice->cellsAreAdjacent(catom->position, pos);
}
void CoatingBlockCode::highlightCSGScaffold(bool debug) {
    // target->highlight();

    if (not HIGHLIGHT_CSG and not HIGHLIGHT_SCAFFOLD and not HIGHLIGHT_COATING)
        return;

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
                    if (HIGHLIGHT_CSG and isInCSG(pos))
                        lattice->highlightCell(pos, WHITE);

                    if (HIGHLIGHT_COATING and HIGHLIGHT_layer == -1 and isInCoating(pos))
                        lattice->highlightCell(pos, ORANGE);

                    if (HIGHLIGHT_COATING and HIGHLIGHT_layer != -1
                        and pos[2] >= scaffoldSeedPos[2]
                        and isInCoatingLayer(pos, HIGHLIGHT_layer))
                        lattice->highlightCell(pos, ORANGE);


                    if (HIGHLIGHT_SCAFFOLD and rm->isInCSGScaffold(norm(pos)))
                        lattice->highlightCell(pos, WHITE);
                }
            }
        }
    }
}

Cell3DPosition CoatingBlockCode::locateNextClosingCornerFrom(const Cell3DPosition& cc,
                                                             bool forceReg) const {
    // // First attempt to favor diagonal
    // for (const Cell3DPosition& p : lattice->getNeighborhood(cc)) {
    //     if ((p[2] == cc[2] + 1)
    //         and ( ((not forceReg) and isInCoating(p))
    //               or (forceReg and isInRegularCoatingLayer(p, getCoatingLayer(p))) )
    //         and isCoatingCorner(p))
    //         return p;
    // }

    for (const Cell3DPosition& p : lattice->getNeighborhood(cc)) {
        if ((p[2] == cc[2] + 1)
            and ( ((not forceReg) and isInCoating(p))
                  or (forceReg and isInRegularCoatingLayer(p, getCoatingLayer(p))) )
            and isCoatingCorner(p))
            return p;
    }

    // VS_ASSERT(false);

    return Cell3DPosition(-1,-1,-1);
}

bool CoatingBlockCode::isCoatingCorner(const Cell3DPosition& pos) const {
    // Take every pair of opposing positions on the horizontal plane around pos and see
    //  and see if there is a couple that is part of the coating (there should at most 1)
    // If there is, pos cannot be a corner, otherwise it is one

    for (size_t i = 0; i < NumCCWDirs / 2; i++) {
        const Cell3DPosition& p1 = pos + CCWDPos[i];
        const Cell3DPosition& p2 = pos + CCWDPos[(i + NumCCWDirs / 2) % NumCCWDirs];
        // diagonals (odd) do not count
        if (IS_ODD(i) and isInCoating(p1) and isInCoating(p2)) {
            // lattice->highlightCell(p1, RED);
            // lattice->highlightCell(p2, RED);
            return false;
        }
    }

    return true;
}

P2PNetworkInterface* CoatingBlockCode::getInterfaceToClosingCornerBelow() const {
    VS_ASSERT(isCoatingCorner(catom->position));

    if (catom->position[2] < scaffoldSeedPos[2]) return NULL;

    for (const Cell3DPosition& pos : lattice->getActiveNeighborCells(catom->position)) {
        if (pos[2] == catom->position[2] - 1 and isCoatingCorner(pos))
            return catom->getInterface(pos);
    }

    return NULL;
}


bool CoatingBlockCode::shouldUseExternalCoatingOnOddLayers() const {
    // return false; //FIXME

    for (size_t i = 0; i < topCoatingLayer; i++) {
        if (verticalLayerShouldOffset(i)) {
            // VS_ASSERT(IS_ODD(i));
            return true;
        }
    }

    return false;
}

void CoatingBlockCode::initializeClosingCornerAndFBPLocations(vector<Cell3DPosition>& cc,
                                                              vector<Cell3DPosition>& fbp,
                                                              bool forceExt) const {
    // Closing Corner 0
    cc.push_back(coatingSeed);

    if (closingCorner.size() > 0) {
        CCWDir d = FrontLeft;
        const Cell3DPosition& cur_fbp =
            findNextBorderLocationFrom(coatingSeed, d, true,
                                       [this](const Cell3DPosition &p) {
                                           return isInCoating(p);
                                       });
        lattice->highlightCell(cur_fbp, MAGENTA);
        fbp.push_back(cur_fbp);
    }

    for (size_t i = 1; i < topCoatingLayer; i++) {
        const Cell3DPosition& cur_cc = locateNextClosingCornerFrom(cc[i - 1], forceExt);
        // VS_ASSERT(cur_cc != Cell3DPosition(-1, -1, -1));
        if (closingCorner.size() > 0) {
            lattice->highlightCell(cur_cc, CYAN);

            CCWDir d = FrontLeft;
            const Cell3DPosition& cur_fbp =
                findNextBorderLocationFrom(cur_cc, d, true,
                                           [this](const Cell3DPosition &p) {
                                               return isInCoating(p);});
            lattice->highlightCell(cur_fbp, MAGENTA);
            fbp.push_back(cur_fbp);
        }

        cc.push_back(cur_cc);
    }
}

bool CoatingBlockCode::verticalLayerShouldOffset(const unsigned int layer) const {
    // Virtually navigate around the object to ensure that all coating positions will be
    //  reachable
    vector<Cell3DPosition> noExtCoatingClosingCorner;
    vector<Cell3DPosition> fbp;
    VS_ASSERT(not useExternalCoatingOnOddLayers);
    initializeClosingCornerAndFBPLocations(noExtCoatingClosingCorner, fbp);

    Cell3DPosition currentPos = noExtCoatingClosingCorner[layer];
    CCWDir lastCwd = FrontLeft;

    do {
        vector<CCWDir> cwdPosCustom = getCCWDirectionsFrom(lastCwd);
        for (int i = 0; i < NumCCWDirs; i++) {
            const Cell3DPosition& cwdPos = CCWDPos[cwdPosCustom[i]];
            // cout << "cwd:" << cwdPos << endl;
            const Cell3DPosition& vPos = cwdPos + currentPos;

            if (isInRegularCoatingLayer(vPos, layer)) {
                lastCwd = (CCWDir)((cwdPosCustom[i] + NumCCWDirs / 2 + 1) % NumCCWDirs);

                // Also check neighbor positions from below coating layer
                if (layer > 0) {
                    for (const Cell3DPosition& belowPos : lattice->getNeighborhood(vPos)) {
                        // lattice->highlightCell(belowPos, RED);
                        // lattice->highlightCell(vPos, WHITE);
                        // // usleep(50000);
                        // lattice->unhighlightCell(belowPos);
                        // lattice->unhighlightCell(vPos);

                        if (belowPos[2] == vPos[2] - 1
                            and isInCoatingLayer(belowPos, layer - 1)
                            and coatingPositionUnreachable(vPos, belowPos)) {
                            return true;
                        }
                    }
                }


                if (currentPos != noExtCoatingClosingCorner[layer]
                    and coatingPositionUnreachable(vPos, currentPos)) {
                    return true;
                }

                currentPos = vPos;

                break;
            }
        }
    } while (currentPos != noExtCoatingClosingCorner[layer]);

    return false;
}

bool CoatingBlockCode::coatingPositionUnreachable(const Cell3DPosition& pos,
                                                  const Cell3DPosition& blkr) const {
    for (const Cell3DPosition& np : lattice->getNeighborhood(pos)) {
        if (np == blkr) continue;

        // lattice->highlightCell(blkr, BLACK);
        // lattice->highlightCell(pos, RED);
        // lattice->highlightCell(np, WHITE);
        // // usleep(50000);

        if (cellsAreOpposite(blkr, np, pos)
            and lattice->getBlock(np)
            and not isInRegularCoatingLayer(np, getCoatingLayer(np))) {

            lattice->highlightCell(blkr, BLACK);
            lattice->highlightCell(pos, RED);
            lattice->highlightCell(np, WHITE);

            // VS_ASSERT(false);

            return true;
        }

        // lattice->unhighlightCell(blkr);
        // lattice->unhighlightCell(pos);
        // lattice->unhighlightCell(np);

    }

    return false;
}

bool CoatingBlockCode::cellsAreOpposite(const Cell3DPosition& p1,
                                        const Cell3DPosition& p2,
                                        const Cell3DPosition& ref) const {
    short dir1 = lattice->getDirection(ref, p1);
    short dir2 = lattice->getDirection(ref, p2);

    return lattice->getOppositeDirection(dir1) == dir2;
}

bool CoatingBlockCode::closingCornerInsertionReady(const Cell3DPosition& cc) const {
    for (const Cell3DPosition& np : lattice->getNeighborhood(cc)) {
        if (np[2] == cc[2] and isInCoating(np) and not lattice->getBlock(np))
            return false;
    }

    return true;
}

size_t CoatingBlockCode::countBorderCoatingNeighborsInPlace(const Cell3DPosition& pos) const {
    size_t count = 0;
    stringstream info;
    info << " around " << pos << ":";

    for (const Cell3DPosition& np : lattice->getActiveNeighborCells(pos)) {
        if (np[2] == pos[2] and np != catom->position
            and isInCoating(np) and lattice->getBlock(np)) {
            info << " " << np;
            count++;
        }
    }

    info << " - ccount: " << count;
    scheduler->trace(info.str(),catom->blockId, WHITE);

    return count;
}

bool CoatingBlockCode::coatingSlotInsertionReady(const Cell3DPosition& pos) const {
    size_t count = countBorderCoatingNeighborsInPlace(pos);

    // if (count == 1) {
    //     Cell3DPosition singleCoatingNPos;
    //     for (const Cell3DPosition& np : lattice->getActiveNeighborCells(pos)) {
    //         if (np[2] == pos[2] and isInCoating(np) and lattice->getBlock(np)) {
    //             singleCoatingNPos = np;
    //             break;
    //         }
    //     }
    // }

    const Cell3DPosition& cc = closingCorner[getCoatingLayer(pos)];

    stringstream info;
    info << " pos: " << pos;
    info << " - count: " << count;
    info << " - caa: " << lattice->cellsAreAdjacent(pos, cc);
    info << " - ccw: " << (lattice->cellsAreAdjacent(pos, cc) ?
                           CCWDir_to_string(getCCWDirectionForEdgeBetween(pos, cc)) : "-1");
    info << " - wut: " << (lattice->cellsAreAdjacent(pos, cc) ? isInRange(getCCWDirectionForEdgeBetween(pos, cc), FrontRight, RearLeft) : false);
    info << " - cc: " << (pos == cc);
    scheduler->trace(info.str(),catom->blockId, WHITE);

    return (count == 0
            and lattice->cellsAreAdjacent(pos, cc)
            and isInRange(getCCWDirectionForEdgeBetween(pos, cc), Front, RearRight)
        )
        or (count == 1 and pos != cc)
            // and (countBorderCoatingNeighborsInPlace(singleCoatingNPos) == 2
            //      or cellsAreAdjacent()))
        or (count == 2 and pos == cc);
}

CCWDir CoatingBlockCode::getCCWDirectionForEdgeBetween(const Cell3DPosition& p1,
                                                       const Cell3DPosition& p2) const {
    VS_ASSERT(lattice->cellsAreAdjacent(p1, p2));

    const Cell3DPosition &rl = p1 - p2;

    for (int i = 0; i < NumCCWDirs; i++) {
        if (CCWDPos[i] == rl) return (CCWDir)i;
    }

    VS_ASSERT(false);

    return (CCWDir)0;
}


bool CoatingBlockCode::isOnInnerBorderCoating(const Cell3DPosition& pos) const {
    size_t layer = getCoatingLayer(pos);

    if (not isInCSG(pos)) return false;

    for (const Cell3DPosition& np : lattice->getNeighborhood(pos)) {
        if (isInCoatingLayer(np, layer)) return true;
    }

    return false;
}

vector<Cell3DPosition> CoatingBlockCode::getNeighborsOnLayer(const uint layer) const {
    vector<Cell3DPosition> l1Neighbors;

    for (const Cell3DPosition& p : lattice->getActiveNeighborCells(catom->position)) {
        if (getCoatingLayer(p) == layer) l1Neighbors.push_back(p);
    }

    return l1Neighbors;
}


bool CoatingBlockCode::instructSupportRelocationIfRequired(Cell3DPosition& support) {
    // TODO: Add condition check

    P2PNetworkInterface *supportItf = catom->getInterface(support);
    Cell3DPosition prevSupport = support;
    support = support + Cell3DPosition(-1, -1, 2); //FIXME:

    // VS_ASSERT(supportItf);
    if (supportItf) {
        sendMessage(new HeadToSupportLocation(support), supportItf, MSG_DELAY_MC, 0);
    } else {
        CoatingBlockCode* pivotBC =
        static_cast<CoatingBlockCode*>(lattice->getBlock(prevSupport +
                                                         Cell3DPosition(0,1,-1))->blockCode);
        pivotBC->sendMessage(new HeadToSupportLocation(support),
                             pivotBC->catom->getInterface(prevSupport), MSG_DELAY_MC, 0);
    }

    return true;
}

bool CoatingBlockCode::introduceEvenSupportAndAssignPosition(Cell3DPosition& support){
    if (not lattice->getBlock(ZHelperSpawnLoc)) {
        world->addBlock(0, buildNewBlockCode, ZHelperSpawnLoc, LIGHTBLUE);
    }

    support = support + Cell3DPosition(-1, -1, 2); //FIXME:

    CoatingBlockCode* spawnPivotBC =
        static_cast<CoatingBlockCode*>(lattice->getBlock(Cell3DPosition(5,5,1))->blockCode);
    spawnPivotBC->sendMessage(new HeadToSupportLocation(support),
                              spawnPivotBC->catom->getInterface(ZHelperSpawnLoc),
                              MSG_DELAY_MC, 0);

    return true;
}
