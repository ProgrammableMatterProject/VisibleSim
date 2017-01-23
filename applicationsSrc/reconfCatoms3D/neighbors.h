#include "cell3DPosition.h"
#include "catoms3DWorld.h"

using namespace Catoms3D;

class Neighbors {
    Catoms3DWorld *world;
    Cell3DPosition simulatedBlockPosition;

    bool isPositionUnblockedSide(const Cell3DPosition &pos);
    bool isPositionUnblockedXY(const Cell3DPosition &pos);
    bool isPositionUnblockedYZ(const Cell3DPosition &pos);
    bool isPositionUnblockedXZ(const Cell3DPosition &pos);
    bool cellHasBlock(const Cell3DPosition &pos);

public:
    static int neighborDirectionsEven[12][3];
    static int neighborDirectionsOdd[12][3];

    bool isPositionBlocked(const Cell3DPosition &pos);
    bool isPositionBlockable(const Cell3DPosition &pos);
    Neighbors(Catoms3DWorld *w) : world(w) {};
};
