/**
 * @file   nodes2DMotionEngine.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Oct 10 12:57:01 2018
 *
 * @brief  Helper functions for planning Nodes2D rotations
 *
 *
 */

#ifndef __NODES2D_MOTION_ENGINE_H__
#define __NODES2D_MOTION_ENGINE_H__

#include <utility>
#include <vector>
#include "robots/nodes2D/nodes2DWorld.h"
#include "grid/cell3DPosition.h"
#include "grid/lattice.h"

using namespace std;

namespace Nodes2D {

    enum motionDirection{CCW,CW};

class Nodes2DMotion {
public:
    SLattice::Direction fromConId,toConId;
    Cell3DPosition pivotPos;
    motionDirection direction;
    Cell3DPosition obstaclePos;
    bool hasObstacle;
    Cell3DPosition finalPos;
    bool isRotation;
    Cell3DPosition pathPos;

    Nodes2DMotion(SLattice::Direction fId,Cell3DPosition pivot,motionDirection md,Cell3DPosition obsPos, bool present,Cell3DPosition fp_or_pp)
    :fromConId(fId),pivotPos(pivot),direction(md),obstaclePos(obsPos),hasObstacle(present),finalPos(hasObstacle?fp_or_pp:obsPos),isRotation(!present),pathPos(fp_or_pp) {
        toConId=isRotation?SLattice::Direction((fId+(md==CW?1:3))%4):fId;
    };

};

class Nodes2DMotionEngine {
    vector<Nodes2DMotion*>tabNodes2DMotions;

public:
    Nodes2DMotionEngine();
    ~Nodes2DMotionEngine();

    vector<Nodes2DMotion*> getAllMotionsForModule(BuildingBlock *nb,const SLattice*sl);
};

};

#endif // __NODES2D_MOTION_ENGINE_H__
