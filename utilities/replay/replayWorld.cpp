/**
 * @file   replayWorld.cpp
 * @author Matteo Daluz
 * @brief contains ReplayWorld abstract class herited in the folder robots
 * ReplayWorld contains the current state of the replay simulation, with current time,
 * current map, moving blocks etc.
 */

#include <iostream>
#include "replayWorld.h"
#include "replay.hpp"
#include "../../simulatorCore/src/gui/objLoader.h"
#include <map>
#include "../../simulatorCore/src/base/glBlock.h"
#include "../../simulatorCore/src/grid/lattice.h"
#include "../../simulatorCore/src/replay/replayTags.h"
#include "replayPlayer.h"
using namespace std;
using namespace ReplayTags;
ReplayWorld::ReplayWorld(int argc, char *argv[], u8 duration, float scale) {
    exportDuration = (float)duration*pow(10,-6);
    endZoom = exportDuration;
    gridScale = scale;
}

ReplayWorld::~ReplayWorld() {
    delete objBlock;
}

void ReplayWorld::updateMap()
{
    if(currentTime*pow(10,6)>=player->getKeyframeEndTime() || currentTime<=lastFrameTime)
    {
        for (const auto& pair : mapGlBlocks) {
            delete (GlBlock*)pair.second;
        }
        mapGlBlocks.clear();
        eventBuffer.clear();
        player->parseFrame(currentTime*pow(10,6));
    }
    else
    {
        cout<<"Updating Frame from t = "<<lastFrameTime<<" to " << currentTime<<endl;
        updateFrame();
    }
    lastFrameTime = currentTime;
}

void ReplayWorld::updateFrame()
{
    player->parseEvents(player->getLastFrameEndParsePosition(),
            currentTime*pow(10,6),
            player->findNextKeyframe(currentTime*pow(10,6)));
    updateMotionBlocks();
}

void ReplayWorld::updateMotionBlocks()
{

}

void ReplayWorld::addBlock(bID blockId, KeyframeBlock block) {

}

void ReplayWorld::updateColor(u4 blockId, Color col) {
    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId) {
            pair.second->setColor(col);
        }
    }
}
void ReplayWorld::updatePosition(u4 blockId, KeyframeBlock block)
{

}

void ReplayWorld::updatePositionMotion(u4 blockId, KeyframeBlock block,
        u8 time,u8 readTime, Cell3DPosition initPos)
{

}
Vector3D ReplayWorld::getPosition(u4 blockId)
{
    Vector3D returnVector;
    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId)
        {
            return pair.second->getPosition();
        }
    }
    return returnVector;
}

void ReplayWorld::glDraw() {
    glPushMatrix();
    glDrawBackground();
    objRepere->glDraw();
    for (const auto& pair : mapGlBlocks) {
        ((GlBlock*)pair.second)->glDraw(objBlock);
    }
    glPopMatrix();
}

void ReplayWorld::updateDisplayedValue(u4 blockId, u2 display)
{

}

void ReplayWorld::glDrawBackground() {

}

void ReplayWorld::loadTextures(const string &str) {

}