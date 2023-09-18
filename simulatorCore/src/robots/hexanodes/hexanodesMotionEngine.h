/**
 * @file   nodeMotionEngine.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Oct 10 12:57:01 2018
 *
 * @brief  Helper functions for planning Hexanodes rotations
 *
 *
 */

#ifndef __HEXANODES_MOTION_ENGINE_H__
#define __HEXANODES_MOTION_ENGINE_H__

#include <utility>
#include <vector>
#include "hexanodesWorld.h"
#include "math/cell3DPosition.h"
#include "../../grid/lattice.h"

using namespace std;

namespace Hexanodes {

    class HexanodesMotion {
public:
    HHLattice::Direction fromConId;
    motionDirection direction;
    vector<HHLattice::Direction>obstacleDirs;

    HexanodesMotion(HHLattice::Direction fId,motionDirection dir,vector<HHLattice::Direction> obs)
    :fromConId(fId),direction(dir),obstacleDirs(obs) {};
    inline Cell3DPosition getFinalPos(const Cell3DPosition &nodePos) const {
        return getWorld()->lattice->getCellInDirection(nodePos,(direction==CW?(fromConId+1)%6:(fromConId+5)%6));
    }
    inline HHLattice::Direction getToConId() const {
        return (HHLattice::Direction )((fromConId+(direction==CW?5:1))%6);
    }
    inline HHLattice::Direction getFinalOrientation(uint8_t nodeOrient) const {
        return (HHLattice::Direction)((nodeOrient+(direction==CW?2:4))%6);
    }
};

class HexanodesMotionEngine {
    vector<HexanodesMotion*>tabHexanodesMotions;

public:
    HexanodesMotionEngine();
    ~HexanodesMotionEngine();

    vector<HexanodesMotion*> getAllMotionsForModule(BuildingBlock *nb,const HHLattice*hl);
};

}

#endif // __HEXANODES_MOTION_ENGINE_H__
