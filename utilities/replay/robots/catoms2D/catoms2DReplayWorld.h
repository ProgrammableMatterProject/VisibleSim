/**
 * @file   ReplayWorld.h
 * @author Matteo Daluz
 * @date   Tue Jun  9 11:13:33 2020
 *
 * @brief  World for the replay application
 *
 *
 */


#pragma once

#include <fstream>
#include <map>
#include "../../simulatorCore/src/gui/objLoader.h"
#include "../../simulatorCore/src/gui/shaders.h"
#include "../../simulatorCore/src/replay/replayTags.h"
#include "replay.hpp"
#include "../../simulatorCore/src/robots/catoms2D/catoms2DGlBlock.h"
#include "replayEvent.h"

using namespace ReplayTags;
namespace Replay {
    class ReplayPlayer;
}
class ReplayEvent;
class Catoms2DReplayWorld : public ReplayWorld{
private:

protected:
    GLuint idTextureHexa,idTextureLines;
public:

    /**
     * @brief World constructor, initializes the camera, light, and user interaction attributes
     */
    Catoms2DReplayWorld(int argc, char *argv[], u8 duration, float scale);
    /**
     * @brief World destructor, deletes the blocks and their GL counterparts, the lattice and camera
     */
    ~Catoms2DReplayWorld();

    void addBlock(bID blockId, KeyframeBlock block) override;
    void updatePositionMotion(u4 blockId, KeyframeBlock block,u8 time, u8 readTime, Cell3DPosition initPos) override;
    void updatePosition(u4 blockId, KeyframeBlock block) override;
    void updateMotionBlocks() override;
    void glDrawBackground() override;
    void loadTextures(const string &str) override ;
    void updateDisplayedValue(u4 blockId, u2 display) override;

};
