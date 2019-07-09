/**
 * @file   nodeMotionEngine.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Oct 10 12:57:01 2018
 * 
 * @brief  Helper functions for planning Hexanode rotations
 * 
 * 
 */

#ifndef __HEXANODE_MOTION_ENGINE_H__
#define __HEXANODE_MOTION_ENGINE_H__

#include <utility>
#include <vector>
#include "hexanodeWorld.h"
#include "cell3DPosition.h"
#include "lattice.h"

using namespace std;

namespace Hexanode {
	
	enum motionDirection{CCW,CW};
	
class HexanodeMotion {
public:
	HHLattice::Direction fromConId,toConId;
	Cell3DPosition pivotPos;
	motionDirection direction;
	Cell3DPosition obstaclePos;
	bool hasObstacle;
	Cell3DPosition finalPos;
	bool isRotation;
	Cell3DPosition pathPos;
	
	HexanodeMotion(HHLattice::Direction fId,Cell3DPosition pivot,motionDirection md,Cell3DPosition obsPos, bool present,Cell3DPosition fp_or_pp)
	:fromConId(fId),pivotPos(pivot),direction(md),obstaclePos(obsPos),hasObstacle(present),finalPos(hasObstacle?fp_or_pp:obsPos),isRotation(!present),pathPos(fp_or_pp) {
		toConId=isRotation?HHLattice::Direction((fId+(md==CW?1:3))%4):fId;
	};
	
};

class HexanodeMotionEngine {
    vector<HexanodeMotion*>tabHexanodeMotions;
    
public:
	HexanodeMotionEngine();
	~HexanodeMotionEngine();
	
	vector<HexanodeMotion*> getAllMotionsForModule(BuildingBlock *nb,const HHLattice*hl);
};

};

#endif // __HEXANODE_MOTION_ENGINE_H__
