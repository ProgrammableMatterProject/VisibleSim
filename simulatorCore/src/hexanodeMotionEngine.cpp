
#include "hexanodeMotionEngine.h"

namespace Hexanode {
	
HexanodeMotionEngine::HexanodeMotionEngine() {
		//tabHexanodeMotions.push_back(new HexanodeMotion(HHLattice::South,Cell3DPosition(0,-1,0),CCW,Cell3DPosition(-1,-1,0),true,Cell3DPosition(-1,0,0))); // South, translation
		
	}

HexanodeMotionEngine::~HexanodeMotionEngine() {
	for (HexanodeMotion*nm:tabHexanodeMotions) {
		delete nm;
	}
	tabHexanodeMotions.clear();
}

vector<HexanodeMotion*> HexanodeMotionEngine::getAllMotionsForModule(BuildingBlock *nb,const HHLattice*sl) {
	Cell3DPosition pos;
	bool hasObs,hasPivot,hasFreeDest;
	vector<HexanodeMotion*>res;
	
	for (HexanodeMotion*nm:tabHexanodeMotions) {
		hasObs=!sl->isFree(nm->obstaclePos+nb->position);
		pos = nm->pivotPos+nb->position;
		hasPivot=!sl->isFree(pos) && sl->isInGrid(pos);
		pos = nm->finalPos+nb->position;
		hasFreeDest=sl->isFree(pos) && sl->isInGrid(pos) && (sl->isFree(nm->pathPos+nb->position) || !nm->isRotation);
		//cout << "motion:" << nm->fromConId << "->" << nm->toConId << " hasPivot=" << hasPivot << " hasFreeDest=" << hasFreeDest << " hasObs=" << hasObs << "/" << nm->hasObstacle <<endl;
		if (hasPivot && hasFreeDest && hasObs==nm->hasObstacle) {
			res.push_back(nm);
		}
	}
	return res;
}

}
