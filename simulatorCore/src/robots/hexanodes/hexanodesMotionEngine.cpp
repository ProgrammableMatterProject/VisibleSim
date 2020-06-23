#include "hexanodesMotionEngine.h"

namespace Hexanodes {

HexanodesMotionEngine::HexanodesMotionEngine() {
    for (int i=0; i<6; i++) {
        tabHexanodesMotions.push_back(new HexanodesMotion((HHLattice::Direction)i,CCW,{(HHLattice::Direction)((i+5)%6),(HHLattice::Direction)((i+4)%6)}));
        tabHexanodesMotions.push_back(new HexanodesMotion((HHLattice::Direction)i,CW,{(HHLattice::Direction)((i+1)%6),(HHLattice::Direction)((i+2)%6)}));
    }
}

HexanodesMotionEngine::~HexanodesMotionEngine() {
    for (HexanodesMotion*nm:tabHexanodesMotions) {
        delete nm;
    }
    tabHexanodesMotions.clear();
}

vector<HexanodesMotion*> HexanodesMotionEngine::getAllMotionsForModule(BuildingBlock *nb,const HHLattice*sl) {
    Cell3DPosition pos,neighborPos,finalPos;
    vector<HexanodesMotion*>res;

    //cout << "Get All Motions:=========================" << endl;
    for (HexanodesMotion *nm:tabHexanodesMotions) {
        neighborPos = sl->getCellInDirection(nb->position,nm->fromConId);
        finalPos = nm->getFinalPos(nb->position);
        //cout << "neighborPos=" << neighborPos << "->" << finalPos << " (" << sl->isFree(neighborPos) << "," << sl->isInGrid(neighborPos) << ")->(" << sl->isFree(finalPos) << "," << sl->isInGrid(finalPos) << ")" << endl;
        if (!sl->isFree(neighborPos) && sl->isInGrid(neighborPos) && sl->isFree(finalPos) && sl->isInGrid((finalPos))) {
            auto obsIt = nm->obstacleDirs.begin();
						while (obsIt!=nm->obstacleDirs.end() && 
							(!sl->isInGrid(sl->getCellInDirection(nb->position,*obsIt)) || sl->isFree(sl->getCellInDirection(nb->position,*obsIt)))) {
                obsIt++;
            }
            if (obsIt==nm->obstacleDirs.end()) {
                //cout << "ok: " << nm->direction << "/" << nm->fromConId << endl;
                res.push_back(nm);
            }
        }
    }
    //cout << "Get All Motions:--------------------------" << endl;
    return res;
}

}
