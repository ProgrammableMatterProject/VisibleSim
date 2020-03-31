/**
 * @file   nodeMotionEngine.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Oct 10 12:57:01 2018
 * 
 * @brief  Helper functions for planning Node rotations
 * 
 * 
 */

#ifndef __NODE_MOTION_ENGINE_H__
#define __NODE_MOTION_ENGINE_H__

#include <utility>
#include <vector>
#include "nodeWorld.h"
#include "cell3DPosition.h"
#include "lattice.h"

using namespace std;

namespace Node {
	
	enum motionDirection{CCW,CW};
	
class NodeMotion {
public:
	SLattice::Direction fromConId,toConId;
	Cell3DPosition pivotPos;
	motionDirection direction;
	Cell3DPosition obstaclePos;
	bool hasObstacle;
	Cell3DPosition finalPos;
	bool isRotation;
	Cell3DPosition pathPos;
	
	NodeMotion(SLattice::Direction fId,Cell3DPosition pivot,motionDirection md,Cell3DPosition obsPos, bool present,Cell3DPosition fp_or_pp)
	:fromConId(fId),pivotPos(pivot),direction(md),obstaclePos(obsPos),hasObstacle(present),finalPos(hasObstacle?fp_or_pp:obsPos),isRotation(!present),pathPos(fp_or_pp) {
		toConId=isRotation?SLattice::Direction((fId+(md==CW?1:3))%4):fId;
	};
	
};

class NodeMotionEngine {
    vector<NodeMotion*>tabNodeMotions;
    
public:
	NodeMotionEngine();
	~NodeMotionEngine();
	
	vector<NodeMotion*> getAllMotionsForModule(BuildingBlock *nb,const SLattice*sl);
};

};

#endif // __NODE_MOTION_ENGINE_H__
