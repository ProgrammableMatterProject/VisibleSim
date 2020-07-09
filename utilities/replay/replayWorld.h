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
#include "replayEvent.h"

using namespace ReplayTags;
namespace Replay {
    class ReplayPlayer;
}
class ReplayEvent;
class ReplayWorld{
private:

    float exportDuration = 0.0f;
    float currentTime = 0.0f;
    float startZoom = 0.0f;
    float endZoom = 0.0f;
    float lastFrameTime = 0.0f;

public:

    float gridScale = 25.0f;
    ObjLoader::ObjLoader* objBlock=nullptr;
    ObjLoader::ObjLoader* objRepere=nullptr;

    Replay::ReplayPlayer* player=nullptr;
    map<bID, GlBlock*>mapGlBlocks; //!< A hash map containing pointers to all graphical blocks, indexed by block id
    map<bID, ReplayEvent>eventBuffer;
    /**
     * @brief World constructor, initializes the camera, light, and user interaction attributes
     */
    ReplayWorld(int argc, char *argv[], u8 duration, float scale);
    /**
     * @brief World destructor, deletes the blocks and their GL counterparts, the lattice and camera
     */
    ~ReplayWorld();

    ObjLoader::ObjLoader getObjBlock() {return *objBlock;};
    float getExportDuration() const {return exportDuration;};
    float getStartZoom() const {return startZoom;};
    float getEndZoom() const {return endZoom;};
    void setStartZoom(float time) {startZoom=time;};
    void setEndZoom(float time) {endZoom=time;};
    void setExportDuration(float duration) {exportDuration=duration;};
    float getCurrentTime(){return currentTime;};
    void setCurrentTime(float time) {currentTime=time;};
    void updateMap();
    void addBlock(bID blockId, KeyframeBlock block);
    void glDraw();
    void updateColor(u4 blockId, Color col);
    void updatePositionMotion(u4 blockId, KeyframeBlock block,u8 time, u8 readTime, Cell3DPosition initPos);
    void updatePosition(u4 blockId, KeyframeBlock block);
    void updateFrame();
    void updateMotionBlocks();
    Vector3D getPosition(u4 blockId);


};
#endif