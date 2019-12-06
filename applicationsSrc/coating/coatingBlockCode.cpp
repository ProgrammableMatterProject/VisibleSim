#include "coatingBlockCode.hpp"

#include <unistd.h>

#include "coatingUtils.hpp"

using namespace Catoms3D;

CoatingBlockCode::CoatingBlockCode(Catoms3DBlock *host) : Catoms3DBlockCode(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (handleSampleMessage) to the message of type SAMPLE_MSG_ID
    // addMessageEventFunc2(SAMPLE_MSG_ID,
    //                      std::bind(&CoatingBlockCode::handleSampleMessage, this,
    //                                std::placeholders::_1, std::placeholders::_2));

    world = BaseSimulator::getWorld();
    // set the module pointer
    catom = static_cast<Catoms3DBlock*>(hostBlock);

    isInG = CoatingBlockCode::isInCSG;
    if (not neighborhood) neighborhood = new Neighborhood(isInG);
  }

void CoatingBlockCode::startup() {
    if (catom->blockId == 1) COATING_SEED_POS = catom->position; // FIXME:

    if (not isInG(catom->position)) return;

    if (HIGHLIGHT_COATING or HIGHLIGHT_CSG or HIGHLIGHT_SEEDS) {
        highlight();
        HIGHLIGHT_COATING = false;
        HIGHLIGHT_CSG = false;
        HIGHLIGHT_SEEDS = false;
    }

    // Simulate authorizations
    if (watchlist.find(catom->position) != watchlist.end()) {
        auto const& it = watchlist.find(catom->position);

        stringstream info;
        info << " provides attract authorization to " << it->first;
        scheduler->trace(info.str(), catom->blockId, AUTH_DEBUG_COLOR);

        const auto& callback = it->second;
        callback();
        watchlist.erase(it);
    }

    if (catom->position == COATING_SEED_POS or isInG(catom->position)) {
        attract();
    }
}

void CoatingBlockCode::handleSampleMessage(MessagePtr msgPtr, P2PNetworkInterface* sender) {
    MessageOf<int>* msg = static_cast<MessageOf<int>*>(msgPtr.get());
}

void CoatingBlockCode::onMotionEnd() {
    console << " has reached " << catom->position << "\n";

    // do stuff
    // ...
}

void CoatingBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;

    // Do not remove line below
    BlockCode::processLocalEvent(pev);

    switch (pev->eventType) {
        case EVENT_ADD_NEIGHBOR: {
            // Do something when a neighbor is added to an interface of the catom
            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            // Do something when a neighbor is removed from an interface of the catom
            break;
        }
    }
}

/// ADVANCED BLOCKCODE FUNCTIONS BELOW

void CoatingBlockCode::onBlockSelected() {
    // Debug stuff:
    cout << endl << "--- PRINT CATOM " << *catom << "---" << endl;

    cout << "isNorthSeed(" << catom->position << "): "
         << neighborhood->isNorthSeed(catom->position) << endl;
}

void CoatingBlockCode::onAssertTriggered() {
    console << " has triggered an assert" << "\n";

    // Print debugging some info if needed below
    // ...
}

bool CoatingBlockCode::parseUserCommandLineArgument(int &argc, char **argv[]) {
    /* Reading the command line */
    if ((argc > 0) && ((*argv)[0][0] == '-')) {
        switch((*argv)[0][1]) {

            // Single character example: -b
            case 'b':   {
                cout << "-b option provided" << endl;
                return true;
            } break;

            // Composite argument example: --foo 13
            case '-': {
                string varg = string((*argv)[0] + 2); // argv[0] without "--"
                if (varg == string("coating")) { //
                    HIGHLIGHT_COATING = true;

                    try {
                        HIGHLIGHT_COATING_LAYER = stoi((*argv)[1]);
                        argc--;
                        (*argv)++;
                    } catch(std::logic_error&) {}

                    cout << "--coating option provided with value: "
                         << HIGHLIGHT_COATING_LAYER << endl;
                } else if (varg == string("csg")) {
                    HIGHLIGHT_CSG = true;

                    cout << "--csg option provided" << endl;
                } else if (varg == string("seeds")) {
                    HIGHLIGHT_SEEDS = true;

                    cout << "--seeds option provided" << endl;
                } else {
                    return false;
                }

                return true;
            }
        }
    }

    return false;
}

void CoatingBlockCode::highlight() const {
    if (HIGHLIGHT_CSG) target->highlight();

    if (HIGHLIGHT_COATING) {
        Cell3DPosition pos;
        for (short iz = 0; iz <= lattice->getGridUpperBounds()[2]; iz++) {
            const Cell3DPosition& glb = lattice->getGridLowerBounds(iz);
            const Cell3DPosition& ulb = lattice->getGridUpperBounds(iz);
            for (short iy = glb[1]; iy <= ulb[1]; iy++) {
                for (short ix = glb[0]; ix <= ulb[0]; ix++) {
                    pos.set(ix,iy,iz);

                    if (isInCoatingLayer(pos, HIGHLIGHT_COATING_LAYER))
                        lattice->highlightCell(pos);
                }
            }
        }
    }

    if (HIGHLIGHT_SEEDS) {
        lattice->highlightAllCellsThatVerify(
            [this](const Cell3DPosition& p) { return neighborhood->isNorthSeed(p); }, GREEN);
        lattice->highlightAllCellsThatVerify(
            [this](const Cell3DPosition& p) { return neighborhood->isSouthSeed(p); }, ORANGE);
        lattice->highlightAllCellsThatVerify( [this](const Cell3DPosition& p) {
            return neighborhood->isNorthLineOnMerge(p); }, RED);
        lattice->highlightAllCellsThatVerify([this](const Cell3DPosition& p) {
            return neighborhood->isSouthLineOnMerge(p); }, BLUE);
        lattice->highlightAllCellsThatVerify([this](const Cell3DPosition& p) {
            return isInG(p) and neighborhood->isOnInternalHole(p, West); }, MAGENTA);
        lattice->highlightAllCellsThatVerify([this](const Cell3DPosition& p) {
            return isInG(p) and neighborhood->isOnInternalHole(p, East); }, BLACK);
    }
}

int CoatingBlockCode::getCoatingLayer(const Cell3DPosition& pos) {
    return pos[2] - COATING_SEED_POS[2];
}

bool CoatingBlockCode::isInCoating(const Cell3DPosition& pos) {
    return BaseSimulator::getWorld()->lattice->isInGrid(pos)
        and pos[2] >= COATING_SEED_POS[2] and isInCoatingLayer(pos, getCoatingLayer(pos));
}

bool CoatingBlockCode::isInCoatingLayer(const Cell3DPosition& pos, int layer) {
    int pLayer = getCoatingLayer(pos);

    if (isInCSG(pos)) return false;

    return (layer == -1 or pLayer == layer)
        // and hasNeighborInCSG(pos);  // Coating at distance 1
        and not hasHorizontalNeighborInCSG(pos) and has2ndOrderNeighborInCSG(pos); // Coating distance 2
}

bool CoatingBlockCode::hasHorizontalNeighborInCSG(const Cell3DPosition& pos) {
    for (const Cell3DPosition& p : BaseSimulator::getWorld()->lattice->getNeighborhood(pos)) {
        if (p[2] < pos[2]) continue;

        if (isInCSG(p)) return true;
    }

    // for (const Cell3DPosition& pRel : diagNeighbors) {
    //     if (isInCSG(pRel + pos)) return true;
    // }

    return false;
}

bool CoatingBlockCode::has2ndOrderNeighborInCSG(const Cell3DPosition& pos) {
    for (const Cell3DPosition& pRel : _2ndOrderNeighbors) {
        if (isInCSG(pRel + pos)) return true;
    }

   return false;
}

void CoatingBlockCode::attract() {
    stringstream info;

    // North attraction
    if (neighborhood->isNorthSeed(catom->position)
        and not hasNeighborInDirection(SkewFCCLattice::Direction::C1North)) {
        sendAttractSignalTo(catom->position.addY(1));
    }

    // South attraction
    if (neighborhood->isSouthSeed(catom->position)
        and not hasNeighborInDirection(SkewFCCLattice::Direction::C7South)) {
        sendAttractSignalTo(catom->position.addY(-1));
    }

    // West attraction
    if (neighborhood->directionIsInCSG(catom->position, West)
        and not hasNeighborInDirection(SkewFCCLattice::Direction::C6West)) {
        const Cell3DPosition& wPos = neighborhood->cellInDirection(catom->position, West);
        if (neighborhood->directionIsInCSG(catom->position, SouthWest)
            and hasNeighborInDirection(SkewFCCLattice::Direction::C7South)) {
            getAuthorizationToAttract(neighborhood->
                                      cellInDirection(catom->position, South), West);
        } else if (neighborhood->isOnInternalHole(catom->position, West)) {
            info << " sends a WEST border following request for " << wPos;
            scheduler->trace(info.str(),catom->blockId, ATTRACT_DEBUG_COLOR);

            borderFollowingAttractRequest(neighborhood->cellInDirection(wPos, South), West);
        } else {
            sendAttractSignalTo(wPos);
        }
    }

    // East attraction
    if (neighborhood->directionIsInCSG(catom->position, East)
        and not hasNeighborInDirection(SkewFCCLattice::Direction::C0East)) {
        const Cell3DPosition& ePos = neighborhood->cellInDirection(catom->position, East);
        if (neighborhood->directionIsInCSG(catom->position, NorthEast)
            and hasNeighborInDirection(SkewFCCLattice::Direction::C1North)) {
            getAuthorizationToAttract(neighborhood->
                                      cellInDirection(catom->position, North),East);
        } else if (neighborhood->isOnInternalHole(catom->position, East)) {
            info << " sends a EAST border following request for " << ePos;
            scheduler->trace(info.str(),catom->blockId, ATTRACT_DEBUG_COLOR);

            borderFollowingAttractRequest(neighborhood->cellInDirection(ePos, North), East);
        } else {
            sendAttractSignalTo(ePos);
        }
    }
}

bool CoatingBlockCode::hasNeighborInDirection(SkewFCCLattice::Direction dir) const {
    return lattice->cellHasBlock(lattice->getCellInDirection(catom->position, dir));
}

void CoatingBlockCode::sendAttractSignalTo(const Cell3DPosition& pos) {
    stringstream info;
    info << " attracts to " << planarDirectionPositionToString(catom->position - pos)
         << " position " << pos;
    scheduler->trace(info.str(), catom->blockId, ATTRACT_DEBUG_COLOR);

    catom->setColor(AttractedColor);
    usleep(50000);

    world->addBlock(0, buildNewBlockCode, pos, DefaultColor);
}

bool CoatingBlockCode::getAuthorizationToAttract(const Cell3DPosition& requestee,
                                                 PlanarDir d) {
    stringstream info;
    const Cell3DPosition& pos = catom->position + planarPos[d];
    info << " requests authorization to attract " << pos << "("
         << planarDirectionIndexToString(d) << ") from " << requestee;
    scheduler->trace(info.str(), catom->blockId, AUTH_DEBUG_COLOR);

    // stringstream info2;
    const Cell3DPosition& checkPos = requestee + planarPos[d];
    // info2 << " checkPos: " << checkPos;
    // scheduler->trace(info2.str(), catom->blockId, AUTH_DEBUG_COLOR);

    if (lattice->getBlock(checkPos)) {
        stringstream info;
        info << checkPos << " module in place, self-granting authorization from " << requestee;
        scheduler->trace(info.str(), catom->blockId, AUTH_DEBUG_COLOR);

        sendAttractSignalTo(pos);

        return true;
    }

    // Else. Add to watchlist and get notified when module is added
    VS_ASSERT(watchlist.find(checkPos) == watchlist.end());
    watchlist.emplace(checkPos, std::bind(&CoatingBlockCode::sendAttractSignalTo, this, pos));
    catom->setColor(WaitingColor);

    return false;
}

bool CoatingBlockCode::borderFollowingAttractRequest(const Cell3DPosition& requestee,
                                                     PlanarDir d) {
    stringstream info;
    const Cell3DPosition& pos = catom->position + planarPos[d];
    info << " requests BORDER authorization to attract " << pos << "("
         << planarDirectionIndexToString(d) << ") from " << requestee;
    scheduler->trace(info.str(), catom->blockId, AUTH_DEBUG_COLOR);

    if (lattice->getBlock(requestee)) {
        stringstream info;
        info << target << " module in place, self-granting BORDER authorization";
        scheduler->trace(info.str(), catom->blockId, AUTH_DEBUG_COLOR);

        sendAttractSignalTo(pos);

        return true;
    }

    // Else. Add to watchlist and get notified when module is added
    VS_ASSERT(watchlist.find(requestee) == watchlist.end());
    watchlist.emplace(requestee, std::bind(&CoatingBlockCode::sendAttractSignalTo, this, pos));

    catom->setColor(WaitingColor);

    return false;
}


// void Neighborhood::addLeft() {
//     // Position exists and is free.
//     if(BlockCode::target->isInTarget(catom->position.addX(-1)) &&
//        !catom->getInterface(catom->position.addX(-1))->isConnected()) {
//         // Check if can block previous line or not.
//         if (!BlockCode::target->isInTarget(catom->position.addY(-1).addX(-1))
//                 || !catom->getInterface(catom->position.addY(-1))->isConnected()
//                 || reconf->canFillLeft) {

//             // Do not block previous floor
//             if (reconf->floor == 0 || reconf->canFillWest() || reconf->parentPlaneFinished) {
//                 if (syncNext->needSyncToLeft())
//                     syncNext->sync();
//                 else
//                     addNeighborToLeft();
//             }
//         }
//     }
// }

// void Neighborhood::addRight() {
//     // Position exists and is free.
//     if(BlockCode::target->isInTarget(catom->position.addX(1)) &&
//        !catom->getInterface(catom->position.addX(1))->isConnected()) {
//         // Check if can block previous line or not.
//         if (!BlockCode::target->isInTarget(catom->position.addY(1).addX(1))
//             || !catom->getInterface(catom->position.addY(1))->isConnected()
//             || reconf->canFillRight) {

//             // Do not block previous floor
//             if (reconf->floor == 0 || reconf->canFillEast() || reconf->parentPlaneFinished) {
//                 if (syncPrevious->needSyncToRight()) {
//                     syncPrevious->sync();
//                 }
//                 else {
//                     addNeighborToRight();
//                 }
//             }
//         }
//     }
// }

// void Neighborhood::addNext() {
//     if (BlockCode::target->isInTarget(catom->position.addY(1)) &&
//             reconf->isSeedNext() && ((reconf->confirmNorthLeft && reconf->confirmNorthRight) || reconf->floor == 0 || reconf->parentPlaneFinished) && !syncNext->needSyncToRight()) {
//         addNextLineNeighbor();
//     }
// }
// void Neighborhood::addPrevious() {
//     if (BlockCode::target->isInTarget(catom->position.addY(-1)) &&
//         reconf->isSeedPrevious() && ((reconf->confirmSouthLeft && reconf->confirmSouthRight) || reconf->floor == 0 || reconf->parentPlaneFinished) && !syncPrevious->needSyncToLeft()) {
//         addPreviousLineNeighbor();
//     }
// }


// bool Border::isOnBorder(Cell3DPosition pos)
// {
//     if (BlockCode::target->isInTarget(pos) &&
//         (!BlockCode::target->isInTarget(pos.addX(-1)) ||
//         !BlockCode::target->isInTarget(pos.addX(1)) ||
//         !BlockCode::target->isInTarget(pos.addY(-1)) ||
//         !BlockCode::target->isInTarget(pos.addY(1))))
//         return true;
//     return false;
// }

// int Border::getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos) {
//     vector<pair<int, int>> ccw_order = {{0,-1}, {1,0}, {0,1}, {-1,0}};
//     int newIdx;
//     for (int i = 0; i < 4; i++) {
//         newIdx = (((idx+i-1)%4)+4)%4;
//         Cell3DPosition nextPos = currentPos.addX(ccw_order[newIdx].first)
//                                           .addY(ccw_order[newIdx].second);
//         if (BlockCode::target->isInTarget(nextPos)) {
//             idx = newIdx;
//             currentPos = nextPos;
//             if (i == 0)
//                 return 1;
//             else if (i == 2)
//                 return -1;
//             else if (i == 3)
//                 return -2;
//             return 0;
//         }
//     }
//     return 0;
// }

// int Border::getIdxForBorder(Cell3DPosition pos) {
//     if (isOnBorder(pos)  && BlockCode::target->isInTarget(pos)) {
//         if (BlockCode::target->isInTarget(pos.addY(-1)) &&
//             BlockCode::target->isInTarget(pos.addY(1)) &&
//             !BlockCode::target->isInTarget(pos.addX(-1)) &&
//             BlockCode::target->isInTarget(pos.addX(1)))
//             return 0;
//         if (BlockCode::target->isInTarget(pos.addY(-1)) &&
//             !BlockCode::target->isInTarget(pos.addY(1)) &&
//             BlockCode::target->isInTarget(pos.addX(-1)) &&
//             BlockCode::target->isInTarget(pos.addX(1)))
//             return 3;
//         if (BlockCode::target->isInTarget(pos.addY(-1)) &&
//             BlockCode::target->isInTarget(pos.addY(1)) &&
//             BlockCode::target->isInTarget(pos.addX(-1)) &&
//             !BlockCode::target->isInTarget(pos.addX(1)))
//             return 2;
//         if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
//             BlockCode::target->isInTarget(pos.addY(1)) &&
//             BlockCode::target->isInTarget(pos.addX(-1)) &&
//             BlockCode::target->isInTarget(pos.addX(1)))
//             return 1;
//         // 2 empty neighbors
//         if (BlockCode::target->isInTarget(pos.addY(-1)) &&
//             !BlockCode::target->isInTarget(pos.addY(1)) &&
//             !BlockCode::target->isInTarget(pos.addX(-1)) &&
//             BlockCode::target->isInTarget(pos.addX(1)))
//             return 3;
//         if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
//             BlockCode::target->isInTarget(pos.addY(1)) &&
//             !BlockCode::target->isInTarget(pos.addX(-1)) &&
//             BlockCode::target->isInTarget(pos.addX(1)))
//             return 0;
//         if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
//             BlockCode::target->isInTarget(pos.addY(1)) &&
//             BlockCode::target->isInTarget(pos.addX(-1)) &&
//             !BlockCode::target->isInTarget(pos.addX(1)))
//             return 1;
//         if (BlockCode::target->isInTarget(pos.addY(-1)) &&
//             !BlockCode::target->isInTarget(pos.addY(1)) &&
//             BlockCode::target->isInTarget(pos.addX(-1)) &&
//             !BlockCode::target->isInTarget(pos.addX(1)))
//             return 2;
//         if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
//             !BlockCode::target->isInTarget(pos.addY(1)) &&
//             BlockCode::target->isInTarget(pos.addX(-1)) &&
//             BlockCode::target->isInTarget(pos.addX(1)))
//             return 0;
//         // critical case?
//         if (BlockCode::target->isInTarget(pos.addY(-1)) &&
//             BlockCode::target->isInTarget(pos.addY(1)) &&
//             !BlockCode::target->isInTarget(pos.addX(-1)) &&
//             !BlockCode::target->isInTarget(pos.addX(1)))
//             return 2;
//         if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
//             BlockCode::target->isInTarget(pos.addY(1)) &&
//             !BlockCode::target->isInTarget(pos.addX(-1)) &&
//             BlockCode::target->isInTarget(pos.addX(1)))
//             return 1;
//         // 3 empty neighbors
//         if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
//             BlockCode::target->isInTarget(pos.addY(1)) &&
//             !BlockCode::target->isInTarget(pos.addX(-1)) &&
//             !BlockCode::target->isInTarget(pos.addX(1)))
//             return 0;
//         if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
//             !BlockCode::target->isInTarget(pos.addY(1)) &&
//             !BlockCode::target->isInTarget(pos.addX(-1)) &&
//             BlockCode::target->isInTarget(pos.addX(1)))
//             return 3;
//         if (BlockCode::target->isInTarget(pos.addY(-1)) &&
//             !BlockCode::target->isInTarget(pos.addY(1)) &&
//             !BlockCode::target->isInTarget(pos.addX(-1)) &&
//             !BlockCode::target->isInTarget(pos.addX(1)))
//             return 2;
//         if (!BlockCode::target->isInTarget(pos.addY(-1)) &&
//             !BlockCode::target->isInTarget(pos.addY(1)) &&
//             BlockCode::target->isInTarget(pos.addX(-1)) &&
//             !BlockCode::target->isInTarget(pos.addX(1)))
//             return 1;
//     }
//     return 0;
// }
