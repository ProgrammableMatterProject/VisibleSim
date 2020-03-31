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
	HHLattice::Direction fromConId;
	motionDirection direction;
	vector<HHLattice::Direction>obstacleDirs;
	
	HexanodeMotion(HHLattice::Direction fId,motionDirection dir,vector<HHLattice::Direction> obs)
	:fromConId(fId),direction(dir),obstacleDirs(obs) {};
	inline Cell3DPosition getFinalPos(const Cell3DPosition &nodePos) {
		return getWorld()->lattice->getCellInDirection(nodePos,(direction==CW?(fromConId+1)%6:(fromConId+5)%6));
	}
	inline HHLattice::Direction getToConId() {
		return (HHLattice::Direction )((fromConId+(direction==CW?5:1))%6);
	}
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
