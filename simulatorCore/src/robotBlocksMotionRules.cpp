#include "robotBlocksMotionRules.h"
#include "rotation3DEvents.h"
#include "robotBlocksWorld.h"
//#include "robotBlocksMotionEngine.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace RobotBlocks
namespace RobotBlocks {

    static const Cell3DPosition NeighborDirectionVector[6]={Cell3DPosition(1,0,0), Cell3DPosition(0,1,0), Cell3DPosition(0,0,1), Cell3DPosition(0,0,-1), Cell3DPosition(0,-1,0), Cell3DPosition(-1,0,0)};

RobotBlocksMotionRule::~RobotBlocksMotionRule() {
    mustBeFree.clear();
    mustBeFull.clear();
}

RobotBlocksTranslationMotionRule::RobotBlocksTranslationMotionRule(NeighborDirection _pivot,NeighborDirection _direction):RobotBlocksMotionRule(_pivot,false),direction(_direction) {
    // Must be full: pivot and neighbor of pivot
    mustBeFull.push_back(NeighborDirectionVector[pivot]);
    mustBeFull.push_back(NeighborDirectionVector[pivot]+NeighborDirectionVector[direction]);
    // Must be free: destination
    mustBeFree.push_back(NeighborDirectionVector[direction]);
};

bool RobotBlocksMotionRule::isValid(const RobotBlocksBlock *rb) {
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

RobotBlocksRotationMotionRule::RobotBlocksRotationMotionRule(NeighborDirection _pivot,NeighborDirection _axe):RobotBlocksMotionRule(_pivot,true),axe(_axe) {
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

    cout << "pivot:" << NeighborDirectionVector[pivot] << "; axe:" << NeighborDirectionVector[axe] << "; dir=" << dir << endl;
};

RobotBlocksMotionRules::RobotBlocksMotionRules() {
    motionRules.push_back(new RobotBlocksTranslationMotionRule(BOTTOM,EAST));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(BOTTOM,NORTH));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(BOTTOM,WEST));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(BOTTOM,SOUTH));

    motionRules.push_back(new RobotBlocksTranslationMotionRule(TOP,EAST));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(TOP,NORTH));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(TOP,WEST));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(TOP,SOUTH));

    motionRules.push_back(new RobotBlocksTranslationMotionRule(EAST,TOP));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(EAST,NORTH));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(EAST,BOTTOM));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(EAST,SOUTH));

    motionRules.push_back(new RobotBlocksTranslationMotionRule(WEST,TOP));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(WEST,NORTH));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(WEST,BOTTOM));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(WEST,SOUTH));

    motionRules.push_back(new RobotBlocksTranslationMotionRule(SOUTH,TOP));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(SOUTH,EAST));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(SOUTH,BOTTOM));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(SOUTH,WEST));

    motionRules.push_back(new RobotBlocksTranslationMotionRule(NORTH,TOP));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(NORTH,EAST));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(NORTH,BOTTOM));
    motionRules.push_back(new RobotBlocksTranslationMotionRule(NORTH,WEST));

    motionRules.push_back(new RobotBlocksRotationMotionRule(BOTTOM,EAST));
    motionRules.push_back(new RobotBlocksRotationMotionRule(BOTTOM,NORTH));
    motionRules.push_back(new RobotBlocksRotationMotionRule(BOTTOM,WEST));
    motionRules.push_back(new RobotBlocksRotationMotionRule(BOTTOM,SOUTH));

    motionRules.push_back(new RobotBlocksRotationMotionRule(TOP,EAST));
    motionRules.push_back(new RobotBlocksRotationMotionRule(TOP,NORTH));
    motionRules.push_back(new RobotBlocksRotationMotionRule(TOP,WEST));
    motionRules.push_back(new RobotBlocksRotationMotionRule(TOP,SOUTH));

    motionRules.push_back(new RobotBlocksRotationMotionRule(EAST,TOP));
    motionRules.push_back(new RobotBlocksRotationMotionRule(EAST,NORTH));
    motionRules.push_back(new RobotBlocksRotationMotionRule(EAST,BOTTOM));
    motionRules.push_back(new RobotBlocksRotationMotionRule(EAST,SOUTH));

    motionRules.push_back(new RobotBlocksRotationMotionRule(WEST,TOP));
    motionRules.push_back(new RobotBlocksRotationMotionRule(WEST,NORTH));
    motionRules.push_back(new RobotBlocksRotationMotionRule(WEST,BOTTOM));
    motionRules.push_back(new RobotBlocksRotationMotionRule(WEST,SOUTH));

    motionRules.push_back(new RobotBlocksRotationMotionRule(SOUTH,TOP));
    motionRules.push_back(new RobotBlocksRotationMotionRule(SOUTH,EAST));
    motionRules.push_back(new RobotBlocksRotationMotionRule(SOUTH,BOTTOM));
    motionRules.push_back(new RobotBlocksRotationMotionRule(SOUTH,WEST));

    motionRules.push_back(new RobotBlocksRotationMotionRule(NORTH,TOP));
    motionRules.push_back(new RobotBlocksRotationMotionRule(NORTH,EAST));
    motionRules.push_back(new RobotBlocksRotationMotionRule(NORTH,BOTTOM));
    motionRules.push_back(new RobotBlocksRotationMotionRule(NORTH,WEST));

}

RobotBlocksMotionRules::~RobotBlocksMotionRules() {
    for (auto p : motionRules) {
        delete p;
    }

    motionRules.clear();
}

vector<RobotBlocksMotionRule*> RobotBlocksMotionRules::getValidMotionList(const RobotBlocksBlock* rb) {
        vector<RobotBlocksMotionRule*> res;
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

std::ostream& operator<<(std::ostream &stream, RobotBlocksMotionRule const& mr) {


    return stream;
}

} // RobotBlocks namespace
