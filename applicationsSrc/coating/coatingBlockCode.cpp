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
    // isInG = CoatingBlockCode::isInCoating;

    if (not neighborhood) neighborhood = new Neighborhood(isInG);
    if (not border) border = new Border(isInG, neighborhood);
    if (not seeding) seeding = new Seeding(isInG, neighborhood, border);
}

CoatingBlockCode::~CoatingBlockCode() {
    if (neighborhood) {
        delete neighborhood;
        neighborhood = NULL;
    }

    if (border) {
        delete border;
        border = NULL;
    }

    if (seeding) {
        delete seeding;
        seeding = NULL;
    }
};

void CoatingBlockCode::startup() {
    if (catom->blockId == 1) G_SEED_POS = catom->position; // FIXME:

    static bool delayInit = false;
    if (not delayInit) {
        if (scheduler->getMode() == SCHEDULER_MODE_FASTEST)
            ATTRACT_DELAY = 0;
        delayInit = true;
    }

    if (HIGHLIGHT_COATING or HIGHLIGHT_CSG or HIGHLIGHT_SEEDS) {
        highlight();
        HIGHLIGHT_COATING = false;
        HIGHLIGHT_CSG = false;
        HIGHLIGHT_SEEDS = false;
    }

    if (not isInG(catom->position)) return;

    if (catom->position == G_SEED_POS)
        initializePlaneSeeds();


    int layer = getGLayer(catom->position);
    if (++planeAttracted[layer] == planeRequires[layer]) {
        catom->setColor(RED);

        // Start next layer if not top plane
        // cout << "nPlanes: " << nPlanes << endl;
        // cout << "layer: " << layer << endl;
        if (layer < nPlanes - 1) {
            for (const Cell3DPosition& seed : planeSeed[layer]) {
                const Cell3DPosition& aPos = isInG(seed + seeding->forwardSeed) ?
                    seeding->forwardSeed : seeding->backwardSeed;

                VS_ASSERT_MSG(isInG(seed + aPos), "seed target is not in G!");

                sendAttractSignalTo(seed + aPos);
            }
        }
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


    if (isInG(catom->position)) {
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

    // cout << "isNorthSeed(" << catom->position << "): "
    //      << seeding->isNorthSeed(catom->position) << endl;

    // cout << "isInG(" << catom->position << "): "
    //      << isInG(catom->position) << endl;

    // cout << "couldBeSeed(" << neighborhood->cellInDirection(catom->position, East) << "): "
    //      << seeding->couldBeSeed(neighborhood->cellInDirection(catom->position, East)) << endl;

    // cout << "couldBeSeed(" << neighborhood->cellInDirection(catom->position, South) << "): "
    //      << seeding->couldBeSeed(neighborhood->cellInDirection(catom->position, South)) <<endl;

    // cout << "isSeedBorderOnNextPlane(" << catom->position + seeding->backwardSeed << "): "
    //      << seeding->isSeedBorderOnNextPlane(catom->position + seeding->backwardSeed) << endl;

    // cout << "isSeedBorderOnCurrentPlane(" << catom->position << "): "
    //      << seeding->isSeedBorderOnCurrentPlane(catom->position) << endl;

    // cout << "isOnBorder(" << catom->position << "): "
    //      << border->isOnBorder(catom->position) << endl;

    // cout << "isLowestOfBorderOnCurrentPlane(" << catom->position << "): "
    //      << seeding->isLowestOfBorderOnCurrentPlane(catom->position) << endl;

    // cout << "isLowestOfBorderOnNextPlane(" << catom->position + seeding->backwardSeed << "): "
    //      <<seeding->isLowestOfBorderOnNextPlane(catom->position + seeding->backwardSeed)<<endl;

    cout << endl << "Plane Requires: " << endl;
    for (int i = 0; i < nPlanes; i++) {
        cout << i << "\t" << planeRequires[i] << endl;
    }

    cout << endl << "Plane Attracted: " << endl;
    for (int i = 0; i < nPlanes; i++) {
        cout << i << "\t" << planeAttracted[i] << endl;
    }

    cout << endl << "Plane Seed: " << endl;
    for (int i = 0; i < nPlanes; i++) {
        cout << i;
        for (const Cell3DPosition& seed : planeSeed[i]) {
            cout << "\t" << seed;
        }
        cout << endl;
    }
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
                    } catch(std::logic_error&) {
                        stringstream err;
                        err << "Found invalid parameter after option --coating: "
                            << (*argv)[1] << endl;
                        throw CLIParsingError(err.str());
                    }

                    cout << "--coating option provided with value: "
                         << HIGHLIGHT_COATING_LAYER << endl;
                } else if (varg == string("csg")) {
                    HIGHLIGHT_CSG = true;

                    cout << "--csg option provided" << endl;
                } else if (varg == string("seeds")) {
                    HIGHLIGHT_SEEDS = true;

                    cout << "--seeds option provided" << endl;
                } else if (varg == string("delay")) { //
                    try {
                        ATTRACT_DELAY = stoi((*argv)[1]);
                        argc--;
                        (*argv)++;
                    } catch(std::logic_error&) {
                        stringstream err;
                        err << "Found invalid parameter after option --delay: '"
                            << (*argv)[1] << "' (expected integer value in ms)" << endl;
                        throw CLIParsingError(err.str());
                    }

                    cout << "--delay option provided with value: "
                         << ATTRACT_DELAY << " ms" << endl;
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
        lattice->highlightAllCellsThatVerify([this](const Cell3DPosition& p) {
            return isInCoatingLayer(p, HIGHLIGHT_COATING_LAYER); });
    }

    if (HIGHLIGHT_SEEDS) {
        // lattice->highlightAllCellsThatVerify(
        //     [this](const Cell3DPosition& p) { return seeding->isNorthSeed(p); }, GREEN);
        // lattice->highlightAllCellsThatVerify(
        //     [this](const Cell3DPosition& p) { return neighborhood->isSouthSeed(p); }, ORANGE);
        // lattice->highlightAllCellsThatVerify( [this](const Cell3DPosition& p) {
        //     return neighborhood->isNorthLineOnMerge(p); }, RED);
        // lattice->highlightAllCellsThatVerify([this](const Cell3DPosition& p) {
        //     return neighborhood->isSouthLineOnMerge(p); }, BLUE);
        // lattice->highlightAllCellsThatVerify([this](const Cell3DPosition& p) {
        //     return isInG(p) and border->isOnInternalHole(p); }, MAGENTA);
        lattice->highlightAllCellsThatVerify([this](const Cell3DPosition& p) {
            return isInG(p) and seeding->isPlaneSeed(p); }, MAGENTA);
    }
}

int CoatingBlockCode::getGLayer(const Cell3DPosition& pos) {
    return pos[2] - G_SEED_POS[2];
}

bool CoatingBlockCode::isInCoating(const Cell3DPosition& pos) {
    return BaseSimulator::getWorld()->lattice->isInGrid(pos)
        and pos[2] >= G_SEED_POS[2] and isInCoatingLayer(pos, getGLayer(pos));
}

bool CoatingBlockCode::isInCoatingLayer(const Cell3DPosition& pos, int layer) {
    int pLayer = getGLayer(pos);

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
    if (seeding->isNorthSeed(catom->position)
        and not neighborhood->hasNeighborInDirection(catom->position,
                                                     SkewFCCLattice::Direction::C1North)) {
        sendAttractSignalTo(neighborhood->cellInDirection(catom->position, North));
    }

    // South attraction
    if (seeding->isSouthSeed(catom->position)
        and not neighborhood->hasNeighborInDirection(catom->position,
                                                     SkewFCCLattice::Direction::C7South)) {
        sendAttractSignalTo(neighborhood->cellInDirection(catom->position, South));
    }

    // West attraction
    if (neighborhood->directionIsInCSG(catom->position, West)
        and not neighborhood->hasNeighborInDirection(catom->position,
                                                     SkewFCCLattice::Direction::C6West)) {
        const Cell3DPosition& wPos = neighborhood->cellInDirection(catom->position, West);
        if (neighborhood->directionIsInCSG(catom->position, SouthWest)
            and neighborhood->hasNeighborInDirection(catom->position,
                                                     SkewFCCLattice::Direction::C7South)) {
            getAuthorizationToAttract(neighborhood->
                                      cellInDirection(catom->position, South), West);
        } else if (not neighborhood->hasNeighborInDirection(catom->position,
                                                            SkewFCCLattice::Direction::C6West)
                   and not neighborhood->directionIsInCSG(catom->position, South)
                   and neighborhood->directionIsInCSG(catom->position, West)
                   and neighborhood->directionIsInCSG(catom->position, SouthWest)
                   and border->isOnInternalHole(catom->position)) {
            info << " sends a WEST border following request for " << wPos;
            scheduler->trace(info.str(),catom->blockId, ATTRACT_DEBUG_COLOR);

            borderFollowingAttractRequest(neighborhood->cellInDirection(wPos, South), West);
        } else {
            sendAttractSignalTo(wPos);
        }
    }

    // East attraction
    if (neighborhood->directionIsInCSG(catom->position, East)
        and not neighborhood->hasNeighborInDirection(catom->position,
                                                     SkewFCCLattice::Direction::C0East)) {
        const Cell3DPosition& ePos = neighborhood->cellInDirection(catom->position, East);
        if (neighborhood->directionIsInCSG(catom->position, NorthEast)
            and neighborhood->hasNeighborInDirection(catom->position,
                                                     SkewFCCLattice::Direction::C1North)) {
            getAuthorizationToAttract(neighborhood->
                                      cellInDirection(catom->position, North),East);
        } else if (not neighborhood->hasNeighborInDirection(catom->position,
                                                            SkewFCCLattice::Direction::C0East)
                   and not neighborhood->directionIsInCSG(catom->position, North)
                   and neighborhood->directionIsInCSG(catom->position, East)
                   and neighborhood->directionIsInCSG(catom->position, NorthEast)
                   and border->isOnInternalHole(catom->position)) {
            info << " sends a EAST border following request for " << ePos;
            scheduler->trace(info.str(),catom->blockId, ATTRACT_DEBUG_COLOR);

            borderFollowingAttractRequest(neighborhood->cellInDirection(ePos, North), East);
        } else {
            sendAttractSignalTo(ePos);
        }
    }
}

void CoatingBlockCode::sendAttractSignalTo(const Cell3DPosition& pos) {
    stringstream info;
    info << " attracts to " << planarDirectionPositionToString(pos - catom->position)
         << " position " << pos;
    scheduler->trace(info.str(), catom->blockId, ATTRACT_DEBUG_COLOR);

    catom->setColor(AttractedColor);
    std::this_thread::sleep_for(std::chrono::milliseconds(ATTRACT_DELAY));

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

void CoatingBlockCode::initializePlaneSeeds() {

    int maxPlane = -1;
    Cell3DPosition pos;

    for (short iz = G_SEED_POS[2]; iz <= lattice->getGridUpperBounds()[2]; iz++) {
        const Cell3DPosition& glb = lattice->getGridLowerBounds(iz);
        const Cell3DPosition& gub = lattice->getGridUpperBounds(iz);
        int idx = iz - G_SEED_POS[2];

        for (short iy = glb[1]; iy <= gub[1]; iy++) {
            for (short ix = glb[0]; ix <= gub[0]; ix++) {
                pos.set(ix,iy,iz);

                if (isInG(pos)) {
                    if (idx > maxPlane) {
                        maxPlane = idx;
                        planeRequires.push_back(0);
                        planeAttracted.push_back(0);
                        planeSeed.push_back(list<Cell3DPosition>());
                    }

                    if (seeding->isPlaneSeed(pos)) // Super costly
                        planeSeed[idx].push_back(pos);

                    planeRequires[idx]++;
                }
            }
        }
    }

    nPlanes = maxPlane + 1;
}
