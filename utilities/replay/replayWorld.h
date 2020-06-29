/**
 * @file   ReplayWorld.h
 * @author Matteo Daluz
 * @date   Tue Jun  9 11:13:33 2020
 *
 * @brief  World for the replay application
 *
 *
 */

#ifndef REPLAYWORLD
#define REPLAYWORLD
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

class ReplayWorld{
private:

    float exportDuration = 0.0f;
    float currentTime = 0.0f;

public:

    ObjLoader::ObjLoader* objBlock=nullptr;
    Replay::ReplayPlayer* player=nullptr;
    map<bID, GlBlock*>mapGlBlocks; //!< A hash map containing pointers to all graphical blocks, indexed by block id
    /**
     * @brief World constructor, initializes the camera, light, and user interaction attributes
     */
    ReplayWorld(int argc, char *argv[], u8 duration);
    /**
     * @brief World destructor, deletes the blocks and their GL counterparts, the lattice and camera
     */
    ~ReplayWorld();

    ObjLoader::ObjLoader getObjBlock() {return *objBlock;};
    float getExportDuration() const {return exportDuration;};
    void setExportDuration(float duration) {exportDuration=duration;};
    float getCurrentTime(){return currentTime;};
    void setCurrentTime(float time) {currentTime=time;};
    void updateMap();
    void addBlock(bID blockId, Vector3D pos, Color col);
    void glDraw();
};
#endif