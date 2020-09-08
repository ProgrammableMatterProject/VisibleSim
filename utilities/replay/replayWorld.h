/**
 * @file   replayWorld.cpp
 * @author Matteo Daluz
 * @brief contains ReplayWorld abstract class herited in the folder robots
 * ReplayWorld contains the current state of the replay simulation, with current time,
 * current map, moving blocks etc.
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
public:
    float exportDuration = 0.0f;
    float currentTime = 0.0f;
    float startZoom = 0.0f;
    float endZoom = 0.0f;
    float lastFrameTime = 0.0f;


    GLuint idTextureFloor;

    float gridScale = 25.0f;
    ObjLoader::ObjLoader* objBlock=nullptr;
    ObjLoader::ObjLoader* objRepere=nullptr;

    Replay::ReplayPlayer* player=nullptr;
    map<bID, GlBlock*>mapGlBlocks; //!< A hash map containing pointers to all graphical blocks, indexed by block id
    map<bID, ReplayMotionEvent>eventBuffer;
    /**
     * @brief World constructor, initializes the camera, light, and user interaction attributes
     */
    ReplayWorld(int argc, char *argv[], u8 duration, float scale);
    /**
     * @brief World destructor, deletes the blocks and their GL counterparts, the lattice and camera
     */
    ~ReplayWorld();

    /* Getters&Setters*/
    ObjLoader::ObjLoader getObjBlock() {return *objBlock;};
    float getExportDuration() const {return exportDuration;};
    float getStartZoom() const {return startZoom;};
    float getEndZoom() const {return endZoom;};
    void setStartZoom(float time) {startZoom=time;};
    void setEndZoom(float time) {endZoom=time;};
    void setExportDuration(float duration) {exportDuration=duration;};
    float getCurrentTime(){return currentTime;};
    void setCurrentTime(float time) {currentTime=time;};

    /**
     * @brief Checks if the map needs to be recalculated or updated
     */
    void updateMap();

    /**
     * @brief Add a new blocks to the world
     */
    virtual void addBlock(bID blockId, KeyframeBlock block);

    /**
     * @brief World draw func
     */
    void glDraw();

    /**
     * @brief Updates a block color
     */
    void updateColor(u4 blockId, Color col);

    /**
     * @brief updates the new position of a moving block
     */
    virtual void updatePositionMotion(u4 blockId, KeyframeBlock block,u8 time, u8 readTime, Cell3DPosition initPos);

    /**
     * @brief Updates the new docked block position
     */
    virtual void updatePosition(u4 blockId, KeyframeBlock block);

    /**
     * @brief Starts event parsing since last frame and updates current map according
     */
    void updateFrame();

    /**
     * @brief Compute the new position of a moving block
     */
    virtual void updateMotionBlocks();

    /**
     * @brief Draw the world background
     */
    virtual void glDrawBackground();

    /**
     * @brief loads grid/background texture
     */
    virtual void loadTextures(const string &str);

    //SmartBlocks
    /**
     * @brief Updates the number written on the block (only used in SmartBlocks
     */
    virtual void updateDisplayedValue(u4 blockId, u2 display);
    virtual Vector3D getPosition(u4 blockId);


};
#endif