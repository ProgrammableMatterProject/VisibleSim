
#include "nodeMotionEngine.h"

namespace Node {
	
NodeMotionEngine::NodeMotionEngine() {
		tabNodeMotions.push_back(new NodeMotion(SLattice::South,Cell3DPosition(0,-1,0),CCW,Cell3DPosition(-1,-1,0),true,Cell3DPosition(-1,0,0))); // South, translation
		tabNodeMotions.push_back(new NodeMotion(SLattice::South,Cell3DPosition(0,-1,0),CCW,Cell3DPosition(-1,-1,0),false,Cell3DPosition(-1,0,0))); // South, rotation
		tabNodeMotions.push_back(new NodeMotion(SLattice::East,Cell3DPosition(+1,0,0),CCW,Cell3DPosition(+1,-1,0),true,Cell3DPosition(0,-1,0))); // East, translation
		tabNodeMotions.push_back(new NodeMotion(SLattice::East,Cell3DPosition(+1,0,0),CCW,Cell3DPosition(+1,-1,0),false,Cell3DPosition(0,-1,0))); // East, rotation
		tabNodeMotions.push_back(new NodeMotion(SLattice::North,Cell3DPosition(0,+1,0),CCW,Cell3DPosition(+1,+1,0),true,Cell3DPosition(+1,0,0))); // North, translation
		tabNodeMotions.push_back(new NodeMotion(SLattice::North,Cell3DPosition(0,+1,0),CCW,Cell3DPosition(+1,+1,0),false,Cell3DPosition(+1,0,0))); // North, rotation
		tabNodeMotions.push_back(new NodeMotion(SLattice::West,Cell3DPosition(-1,0,0),CCW,Cell3DPosition(-1,+1,0),true,Cell3DPosition(0,+1,0))); // West, translation
		tabNodeMotions.push_back(new NodeMotion(SLattice::West,Cell3DPosition(-1,0,0),CCW,Cell3DPosition(-1,+1,0),false,Cell3DPosition(0,+1,0))); // West, rotation
		
		tabNodeMotions.push_back(new NodeMotion(SLattice::South,Cell3DPosition(0,-1,0),CW,Cell3DPosition(+1,-1,0),true,Cell3DPosition(1,0,0))); // South, translation
		tabNodeMotions.push_back(new NodeMotion(SLattice::South,Cell3DPosition(0,-1,0),CW,Cell3DPosition(+1,-1,0),false,Cell3DPosition(1,0,0))); // South, rotation
		tabNodeMotions.push_back(new NodeMotion(SLattice::East,Cell3DPosition(+1,0,0),CW,Cell3DPosition(+1,+1,0),true,Cell3DPosition(0,1,0))); // East, translation
		tabNodeMotions.push_back(new NodeMotion(SLattice::East,Cell3DPosition(+1,0,0),CW,Cell3DPosition(+1,+1,0),false,Cell3DPosition(0,1,0))); // East, rotation
		tabNodeMotions.push_back(new NodeMotion(SLattice::North,Cell3DPosition(0,+1,0),CW,Cell3DPosition(-1,+1,0),true,Cell3DPosition(-1,0,0))); // North, translation
		tabNodeMotions.push_back(new NodeMotion(SLattice::North,Cell3DPosition(0,+1,0),CW,Cell3DPosition(-1,+1,0),false,Cell3DPosition(-1,0,0))); // North, rotation
		tabNodeMotions.push_back(new NodeMotion(SLattice::West,Cell3DPosition(-1,0,0),CW,Cell3DPosition(-1,-1,0),true,Cell3DPosition(0,-1,0))); // West, translation
		tabNodeMotions.push_back(new NodeMotion(SLattice::West,Cell3DPosition(-1,0,0),CW,Cell3DPosition(-1,-1,0),false,Cell3DPosition(0,-1,0))); // West, rotation
	}

NodeMotionEngine::~NodeMotionEngine() {
	for (NodeMotion*nm:tabNodeMotions) {
		delete nm;
	}
	tabNodeMotions.clear();
}

vector<NodeMotion*> NodeMotionEngine::getAllMotionsForModule(BuildingBlock *nb,const SLattice*sl) {
	Cell3DPosition pos;
	bool hasObs,hasPivot,hasFreeDest;
	vector<NodeMotion*>res;
	
	for (NodeMotion*nm:tabNodeMotions) {
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
