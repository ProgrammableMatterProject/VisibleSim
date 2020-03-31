#include "slidingCubesMotionRules.h"
#include "catoms3DRotationEvents.h"
#include "slidingCubesWorld.h"
//#include "slidingCubesMotionEngine.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace SlidingCubes
namespace SlidingCubes {

    static const Cell3DPosition NeighborDirectionVector[6]={Cell3DPosition(1,0,0), Cell3DPosition(0,1,0), Cell3DPosition(0,0,1), Cell3DPosition(0,0,-1), Cell3DPosition(0,-1,0), Cell3DPosition(-1,0,0)};

SlidingCubesMotionRule::~SlidingCubesMotionRule() {
    mustBeFree.clear();
    mustBeFull.clear();
}

SlidingCubesTranslationMotionRule::SlidingCubesTranslationMotionRule(NeighborDirection _pivot,NeighborDirection _direction):SlidingCubesMotionRule(_pivot,false),direction(_direction) {
    // Must be full: pivot and neighbor of pivot
    mustBeFull.push_back(NeighborDirectionVector[pivot]);
    mustBeFull.push_back(NeighborDirectionVector[pivot]+NeighborDirectionVector[direction]);
    // Must be free: destination
    mustBeFree.push_back(NeighborDirectionVector[direction]);
};

bool SlidingCubesMotionRule::isValid(const SlidingCubesBlock *rb) {
    Cell3DPosition pos = rb->position;
    Lattice *lattice = getWorld()->lattice;
    // destination must be in the lattice
    Cell3DPosition finalPos = getFinalPosition(rb);
    if (!lattice->isInGrid(finalPos) || lattice->cellHasBlock(finalPos)) return false;
    // verif free cells
    auto ci=mustBeFree.begin()+1;
    while (ci!=mustBeFree.end() && (!lattice->isInGrid(pos+(*ci)) || !lattice->cellHasBlock(pos+(*ci)))) {
        ci++;
    }
    if (ci!=mustBeFree.end()) return false;
    // verif full cells
    ci=mustBeFull.begin();
    while (ci!=mustBeFull.end() && lattice->isInGrid(pos+(*ci)) && lattice->cellHasBlock(pos+(*ci))) {
        ci++;
    }
    return (ci==mustBeFull.end());
}

SlidingCubesRotationMotionRule::SlidingCubesRotationMotionRule(NeighborDirection _pivot,NeighborDirection _axe):SlidingCubesMotionRule(_pivot,true),axe(_axe) {
    Vector3D vaxe(NeighborDirectionVector[axe][0],NeighborDirectionVector[axe][1],NeighborDirectionVector[axe][2],0);
    Vector3D vpivot(NeighborDirectionVector[pivot][0],NeighborDirectionVector[pivot][1],NeighborDirectionVector[pivot][2],0);
    Vector3D vdir = (vpivot^vaxe).normer();
    Cell3DPosition dir(vdir[0],vdir[1],vdir[2]);

    // Must be full: pivot
    mustBeFull.push_back(NeighborDirectionVector[pivot]);

    // Must be free: destination and neighbor of pivot
    mustBeFree.push_back(NeighborDirectionVector[pivot]+dir);
    mustBeFree.push_back(dir);
    mustBeFree.push_back(2*dir);
    mustBeFree.push_back(NeighborDirectionVector[pivot]+2*dir);
    mustBeFree.push_back(-NeighborDirectionVector[pivot]);
    mustBeFree.push_back(dir-NeighborDirectionVector[pivot]);

    // cout << "pivot:" << NeighborDirectionVector[pivot] << "; axe:" << NeighborDirectionVector[axe] << "; dir=" << dir << endl;
};

SlidingCubesMotionRules::SlidingCubesMotionRules() {
    motionRules.push_back(new SlidingCubesTranslationMotionRule(BOTTOM,EAST));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(BOTTOM,NORTH));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(BOTTOM,WEST));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(BOTTOM,SOUTH));

    motionRules.push_back(new SlidingCubesTranslationMotionRule(TOP,EAST));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(TOP,NORTH));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(TOP,WEST));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(TOP,SOUTH));

    motionRules.push_back(new SlidingCubesTranslationMotionRule(EAST,TOP));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(EAST,NORTH));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(EAST,BOTTOM));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(EAST,SOUTH));

    motionRules.push_back(new SlidingCubesTranslationMotionRule(WEST,TOP));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(WEST,NORTH));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(WEST,BOTTOM));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(WEST,SOUTH));

    motionRules.push_back(new SlidingCubesTranslationMotionRule(SOUTH,TOP));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(SOUTH,EAST));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(SOUTH,BOTTOM));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(SOUTH,WEST));

    motionRules.push_back(new SlidingCubesTranslationMotionRule(NORTH,TOP));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(NORTH,EAST));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(NORTH,BOTTOM));
    motionRules.push_back(new SlidingCubesTranslationMotionRule(NORTH,WEST));

    motionRules.push_back(new SlidingCubesRotationMotionRule(BOTTOM,EAST));
    motionRules.push_back(new SlidingCubesRotationMotionRule(BOTTOM,NORTH));
    motionRules.push_back(new SlidingCubesRotationMotionRule(BOTTOM,WEST));
    motionRules.push_back(new SlidingCubesRotationMotionRule(BOTTOM,SOUTH));

    motionRules.push_back(new SlidingCubesRotationMotionRule(TOP,EAST));
    motionRules.push_back(new SlidingCubesRotationMotionRule(TOP,NORTH));
    motionRules.push_back(new SlidingCubesRotationMotionRule(TOP,WEST));
    motionRules.push_back(new SlidingCubesRotationMotionRule(TOP,SOUTH));

    motionRules.push_back(new SlidingCubesRotationMotionRule(EAST,TOP));
    motionRules.push_back(new SlidingCubesRotationMotionRule(EAST,NORTH));
    motionRules.push_back(new SlidingCubesRotationMotionRule(EAST,BOTTOM));
    motionRules.push_back(new SlidingCubesRotationMotionRule(EAST,SOUTH));

    motionRules.push_back(new SlidingCubesRotationMotionRule(WEST,TOP));
    motionRules.push_back(new SlidingCubesRotationMotionRule(WEST,NORTH));
    motionRules.push_back(new SlidingCubesRotationMotionRule(WEST,BOTTOM));
    motionRules.push_back(new SlidingCubesRotationMotionRule(WEST,SOUTH));

    motionRules.push_back(new SlidingCubesRotationMotionRule(SOUTH,TOP));
    motionRules.push_back(new SlidingCubesRotationMotionRule(SOUTH,EAST));
    motionRules.push_back(new SlidingCubesRotationMotionRule(SOUTH,BOTTOM));
    motionRules.push_back(new SlidingCubesRotationMotionRule(SOUTH,WEST));

    motionRules.push_back(new SlidingCubesRotationMotionRule(NORTH,TOP));
    motionRules.push_back(new SlidingCubesRotationMotionRule(NORTH,EAST));
    motionRules.push_back(new SlidingCubesRotationMotionRule(NORTH,BOTTOM));
    motionRules.push_back(new SlidingCubesRotationMotionRule(NORTH,WEST));

}

SlidingCubesMotionRules::~SlidingCubesMotionRules() {
    for (auto p : motionRules) {
        delete p;
    }

    motionRules.clear();
}

vector<SlidingCubesMotionRule*> SlidingCubesMotionRules::getValidMotionList(const SlidingCubesBlock* rb) {
        vector<SlidingCubesMotionRule*> res;
    auto ci=motionRules.begin();
    while (ci!=motionRules.end()) {
        // OUTPUT << from << " -> " << (*ci)->getConToID() << ", ";
        if ((*ci)->isValid(rb)) {
            res.push_back(*ci);
            // OUTPUT << endl;
        }
        ci++;
    }
    return res;
}

std::ostream& operator<<(std::ostream &stream, SlidingCubesMotionRule const& mr) {


    return stream;
}

} // SlidingCubes namespace
