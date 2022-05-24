#include "slidingCubesMotionRules.h"
//#include "robots/catoms3D/catoms3DRotationEvents.h"
#include "slidingCubesWorld.h"
//#include "robots/slidingCubes/slidingCubesMotionEngine.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace SlidingCubes
namespace SlidingCubes {

    static const Cell3DPosition NeighborDir[6] = {Cell3DPosition(1, 0, 0), Cell3DPosition(0, 1, 0),
                                                  Cell3DPosition(0, 0, 1), Cell3DPosition(-1, 0, 0),
                                                  Cell3DPosition(0, -1, 0), Cell3DPosition(0, 0, -1)};

    SlidingCubesMotionRule::~SlidingCubesMotionRule() {
        mustBeFree.clear();
        mustBeFull.clear();
    }

    SlidingCubesTranslationMotionRule::SlidingCubesTranslationMotionRule(NeighborDirection _pivot,
                                                                         NeighborDirection _direction)
            : SlidingCubesMotionRule(_pivot, false), direction(_direction) {
        // Must be full: pivot and neighbor of pivot
        mustBeFull.push_back(NeighborDir[pivot]);
        mustBeFull.push_back(NeighborDir[pivot] + NeighborDir[direction]);
        // Must be free: destination
        mustBeFree.push_back(NeighborDir[direction]); // warning mustBeFree[0] contains destination pos
    }

    bool SlidingCubesMotionRule::isValid(const SlidingCubesBlock *sc) {
        Cell3DPosition pos = sc->position;
        Lattice *lattice = getWorld()->lattice;
        // destination must be in the lattice
        Cell3DPosition finalPos = pos+mustBeFree[0];
        if (!lattice->isInGrid(finalPos) || lattice->cellHasBlock(finalPos)) return false;
        // verif free cells
        auto ci = mustBeFree.begin() + 1;
        while (ci != mustBeFree.end() && (!lattice->isInGrid(pos + (*ci)) || !lattice->cellHasBlock(pos + (*ci)))) {
            ci++;
        }
        if (ci != mustBeFree.end()) return false;
        // verif full cells
        ci = mustBeFull.begin();
        while (ci != mustBeFull.end() && lattice->isInGrid(pos + (*ci)) && lattice->cellHasBlock(pos + (*ci))) {
            ci++;
        }
        return (ci == mustBeFull.end());
    }

    SlidingCubesRotationMotionRule::SlidingCubesRotationMotionRule(NeighborDirection _pivot, NeighborDirection _axe)
            : SlidingCubesMotionRule(_pivot, true), axe(_axe) {
        Vector3D vaxe(NeighborDir[axe][0], NeighborDir[axe][1], NeighborDir[axe][2],  0);
        Vector3D vpivot(NeighborDir[pivot][0], NeighborDir[pivot][1], NeighborDir[pivot][2], 0);
        Vector3D vdir = (vpivot ^ vaxe).normer();
        Cell3DPosition dir(vdir[0], vdir[1], vdir[2]);

        // Must be full: pivot
        mustBeFull.push_back(NeighborDir[pivot]);

        // Must be free: destination and neighbor of pivot
        mustBeFree.push_back(NeighborDir[pivot] + dir);
        mustBeFree.push_back(dir);
        mustBeFree.push_back(2 * dir);
        mustBeFree.push_back(NeighborDir[pivot] + 2 * dir);
        mustBeFree.push_back(-NeighborDir[pivot]);
        mustBeFree.push_back(dir - NeighborDir[pivot]); // warning mustBeFree[0] contains destination pos

        // cout << "pivot:" << NeighborDir[pivot] << "; axe:" << NeighborDir[axe] << "; dir=" << dir << endl;
    }

    SlidingCubesMotionRules::SlidingCubesMotionRules() {
        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusZ, PlusY));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusZ, MinusX));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusZ, MinusY));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusZ, PlusX));

        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusZ, PlusY));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusZ, MinusX));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusZ, MinusY));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusZ, PlusX));

        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusY, PlusZ));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusY, MinusX));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusY, MinusZ));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusY, PlusX));

        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusY, PlusZ));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusY, MinusX));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusY, MinusZ));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusY, PlusX));

        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusX, PlusZ));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusX, PlusY));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusX, MinusZ));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(PlusX, MinusY));

        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusX, PlusZ));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusX, PlusY));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusX, MinusZ));
        motionRules.push_back(new SlidingCubesTranslationMotionRule(MinusX, MinusY));

        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusZ, PlusY));
        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusZ, MinusX));
        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusZ, MinusY));
        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusZ, PlusX));

        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusZ, PlusY));
        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusZ, MinusX));
        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusZ, MinusY));
        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusZ, PlusX));

        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusY, PlusZ));
        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusY, MinusX));
        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusY, MinusZ));
        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusY, PlusX));

        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusY, PlusZ));
        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusY, MinusX));
        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusY, MinusZ));
        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusY, PlusX));

        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusX, PlusZ));
        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusX, PlusY));
        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusX, MinusZ));
        motionRules.push_back(new SlidingCubesRotationMotionRule(PlusX, MinusY));

        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusX, PlusZ));
        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusX, PlusY));
        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusX, MinusZ));
        motionRules.push_back(new SlidingCubesRotationMotionRule(MinusX, MinusY));

    }

    SlidingCubesMotionRules::~SlidingCubesMotionRules() {
        for (auto p: motionRules) {
            delete p;
        }
        motionRules.clear();
    }

    vector<SlidingCubesMotionRule *> SlidingCubesMotionRules::getValidMotionList(const SlidingCubesBlock *rb) {
        vector<SlidingCubesMotionRule *> res;
        auto ci = motionRules.begin();
        while (ci != motionRules.end()) {
            // OUTPUT << from << " -> " << (*ci)->getConToID() << ", ";
            if ((*ci)->isValid(rb)) {
                res.push_back(*ci);
                // OUTPUT << endl;
            }
            ci++;
        }
        return res;
    }

    std::ostream &operator<<(std::ostream &stream, SlidingCubesMotionRule const &mr) {
        return stream;
    }

} // SlidingCubes namespace
