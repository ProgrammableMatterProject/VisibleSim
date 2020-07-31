/*!
 * @file replayEvent.h
 * @brief Contains motion events classes for moving blocks in the replayer
 * @author Mattéo Daluz
 */

#ifndef REPLAYEVENT
#define REPLAYEVENT
#pragma once

#include <fstream>
#include <map>
#include "../../simulatorCore/src/gui/objLoader.h"
#include "../../simulatorCore/src/gui/shaders.h"
#include "../../simulatorCore/src/replay/replayTags.h"
#include "replay.hpp"
#include "../../simulatorCore/src/robots/smartBlocks/smartBlocksGlBlock.h"
#include "../../simulatorCore/src/robots/catoms3D/catoms3DGlBlock.h"
using namespace ReplayTags;
namespace Replay {
    class ReplayPlayer;
}

class ReplayMotionEvent{
public:
    u8 beginDate;
    u8 duration;
    Cell3DPosition destinationPosition;
    Cell3DPosition initialPosition;
    ReplayMotionEvent();
};

class Catoms3DRotationEvent : public ReplayMotionEvent {
public:
    u4 fixedBlockId;
    Matrix initialMatrix, finalMatrix;
    u1 type;
    u4 radius;
    Vector3D A0C0,A0D0,A1C1,A1D1;
    Vector3D axe1, axe2;
    double angle;

    void init(Catoms3D::Catoms3DGlBlock* mobileBlock,Catoms3D::Catoms3DGlBlock* fixedBlock);

    Matrix getMatrixFromTime(u8 Time);


};

#endif