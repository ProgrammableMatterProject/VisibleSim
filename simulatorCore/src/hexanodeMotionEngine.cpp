
#include "hexanodeMotionEngine.h"

namespace Hexanode {
	
HexanodeMotionEngine::HexanodeMotionEngine() {
	for (int i=0; i<6; i++) {
		tabHexanodeMotions.push_back(new HexanodeMotion((HHLattice::Direction)i,CCW,{(HHLattice::Direction)((i+5)%6),(HHLattice::Direction)((i+4)%6)})); 
		tabHexanodeMotions.push_back(new HexanodeMotion((HHLattice::Direction)i,CW,{(HHLattice::Direction)((i+1)%6),(HHLattice::Direction)((i+2)%6)})); 
	}
}

HexanodeMotionEngine::~HexanodeMotionEngine() {
	for (HexanodeMotion*nm:tabHexanodeMotions) {
		delete nm;
	}
	tabHexanodeMotions.clear();
}

vector<HexanodeMotion*> HexanodeMotionEngine::getAllMotionsForModule(BuildingBlock *nb,const HHLattice*sl) {
	Cell3DPosition pos,neighborPos,finalPos;
	vector<HexanodeMotion*>res;
	
	cout << "Get All Motions:=========================" << endl;
	for (HexanodeMotion *nm:tabHexanodeMotions) {
		neighborPos = sl->getCellInDirection(nb->position,nm->fromConId);
		finalPos = nm->getFinalPos(nb->position);
		cout << "neighborPos=" << neighborPos << "->" << finalPos << " (" << sl->isFree(neighborPos) << "," << sl->isInGrid(neighborPos) << ")->(" << sl->isFree(finalPos) << "," << sl->isInGrid(finalPos) << ")" << endl;
		if (!sl->isFree(neighborPos) && sl->isInGrid(neighborPos) && sl->isFree(finalPos) && sl->isInGrid((finalPos))) {
			auto obsIt = nm->obstacleDirs.begin();
			while (obsIt!=nm->obstacleDirs.end() && (!sl->isInGrid(nb->position) || sl->isFree(sl->getCellInDirection(nb->position,*obsIt)))) {
				obsIt++;
			}
			if (obsIt==nm->obstacleDirs.end()) {
				cout << "ok: " << nm->direction << "/" << nm->fromConId << endl;
				res.push_back(nm);
			}
		}
	}
	cout << "Get All Motions:--------------------------" << endl;
	return res;
}

}