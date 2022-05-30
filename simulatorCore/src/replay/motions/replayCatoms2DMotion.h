//
// Created by daluz on 23/03/2021.
//

#ifndef VISIBLESIM_REPLAYCATOMS2DMOTION_H
#define VISIBLESIM_REPLAYCATOMS2DMOTION_H

#include "../replayMotionEvent.h"

class ReplayCatoms2DMotion : public ReplayMotionEvent{
public:
ReplayCatoms2DMotion(Time date, bID bid, Time duration_us,Cell3DPosition destination, short orientation,
        Cell3DPosition origin, short originOrientation,
        u4 fixedBlockId, u1 type, Vector3D axe1, Vector3D axe2);

virtual ~ReplayCatoms2DMotion();

void write(ofstream* exportFile, ofstream* debugFile, bool debug) override;
private:
u4 fixedBlockId;
u1 type;
Vector3D axe1, axe2;
short finalOrientation,originOrientation;
};

#endif //VISIBLESIM_REPLAYCATOMS2DMOTION_H
