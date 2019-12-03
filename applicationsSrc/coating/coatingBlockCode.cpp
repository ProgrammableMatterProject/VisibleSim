#include "coatingBlockCode.hpp"

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

    // set the module pointer
    catom = static_cast<Catoms3DBlock*>(hostBlock);

    neighborhood = new Neighborhood(catom, isInCSG);
  }

void CoatingBlockCode::startup() {
    console << "start";

    if (HIGHLIGHT_COATING or HIGHLIGHT_CSG) {
        highlight();
        HIGHLIGHT_COATING = false;
        HIGHLIGHT_CSG = false;
    }

    if (catom->position == COATING_SEED_POS) {
        attract();
    }
}

void CoatingBlockCode::handleSampleMessage(MessagePtr msgPtr, P2PNetworkInterface* sender) {
    MessageOf<int>* msg = static_cast<MessageOf<int>*>(msgPtr.get());
}

void CoatingBlockCode::onMotionEnd() {
    console << " has reached its destination" << "\n";

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
    cerr << endl << "--- PRINT CATOM " << *catom << "---" << endl;
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
                    argc--;
                    (*argv)++;

                    cout << "--csg option provided" << endl;
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
}

int CoatingBlockCode::getCoatingLayer(const Cell3DPosition& pos) const {
    return pos[2] - COATING_SEED_POS[2];
}

bool CoatingBlockCode::isInCoating(const Cell3DPosition& pos) const {
    return pos[2] >= COATING_SEED_POS[2] and isInCoatingLayer(pos, getCoatingLayer(pos));
}

bool CoatingBlockCode::isInCoatingLayer(const Cell3DPosition& pos, int layer) const {
    int pLayer = getCoatingLayer(pos);

    if (isInCSG(pos)) return false;

    return (layer == -1 or pLayer == layer)
        // and hasNeighborInCSG(pos);  // Coating at distance 1
        and not hasHorizontalNeighborInCSG(pos) and has2ndOrderNeighborInCSG(pos); // Coating distance 2
}

bool CoatingBlockCode::hasHorizontalNeighborInCSG(const Cell3DPosition& pos) const {
    for (const Cell3DPosition& p : lattice->getNeighborhood(pos)) {
        if (p[2] < pos[2]) continue;

        if (isInCSG(p)) return true;
    }

    // for (const Cell3DPosition& pRel : diagNeighbors) {
    //     if (isInCSG(pRel + pos)) return true;
    // }

    return false;
}

bool CoatingBlockCode::has2ndOrderNeighborInCSG(const Cell3DPosition& pos) const {
    for (const Cell3DPosition& pRel : _2ndOrderNeighbors) {
        if (isInCSG(pRel + pos)) return true;
    }

   return false;
}

void CoatingBlockCode::attract() {
    // North attraction
    if (neighborhood->isNorthSeed()) {
        // sendAttractSignalTo( );
    }


}
