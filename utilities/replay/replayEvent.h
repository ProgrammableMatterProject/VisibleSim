/**
 * @file   ReplayWorld.h
 * @author Matteo Daluz
 * @date   Tue Jun  9 11:13:33 2020
 *
 * @brief  World for the replay application
 *
 *
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

using namespace ReplayTags;
namespace Replay {
    class ReplayPlayer;
}

class ReplayEvent{
public:
    u8 beginDate;
    u8 duration;
    Cell3DPosition destinationPosition;
    Cell3DPosition initialPosition;
    ReplayEvent();
};


#endif