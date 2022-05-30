//
// Created by daluz on 04/02/2021.
//

#ifndef VISIBLESIM_REPLAYCATOMS3DMOTION_H
#define VISIBLESIM_REPLAYCATOMS3DMOTION_H


#include "../replayMotionEvent.h"

class ReplayCatoms3DMotion : public ReplayMotionEvent{
public:
    ReplayCatoms3DMotion(Time date, bID bid, Time duration_us,Cell3DPosition destination, short orientation,
                         Cell3DPosition origin, short originOrientation,
                         u4 fixedBlockId, u1 type, Vector3D axe1, Vector3D axe2);

    virtual ~ReplayCatoms3DMotion();

    void write(ofstream* exportFile, ofstream* debugFile, bool debug) override;
private:
    u4 fixedBlockId;
    u1 type;
    Vector3D axe1, axe2;
    short finalOrientation,originOrientation;
};


#endif //VISIBLESIM_REPLAYCATOMS3DMOTION_H
