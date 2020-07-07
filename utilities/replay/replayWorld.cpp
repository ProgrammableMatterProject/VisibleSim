#include <iostream>
#include "replayWorld.h"
#include "replay.hpp"
#include "../../simulatorCore/src/gui/objLoader.h"
#include <map>
#include "../../simulatorCore/src/base/glBlock.h"
#include "../../simulatorCore/src/grid/lattice.h"
#include "replayPlayer.h"
using namespace std;

ReplayWorld::ReplayWorld(int argc, char *argv[], u8 duration, float scale) {
    cout << "Debug Replay World "<<endl;
    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/smartBlocksTextures",
                                        "smartBlockSimple.obj");
    objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures",
                                         "repere25.obj");
    exportDuration = (float)duration*pow(10,-6);
    gridScale = scale;
}

ReplayWorld::~ReplayWorld() {
    delete objBlock;
}

void ReplayWorld::updateMap()
{

    //Test if a map update is need or just a frame update
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
        updateFrame();
    }
    lastFrameTime = currentTime;
}

void ReplayWorld::updateFrame()
{
    //Tracing for blocks
    for(auto&pair : mapGlBlocks)
    {
        if(pair.first == 36)
        {
            Color col;
            col.rgba[0] = 1.0;
            col.rgba[1] = 1.0;
            col.rgba[2]= 1.0;
            col.rgba[3]= 1.0;
            pair.second->setColor(col);
        }
    }
    //mapGlBlocks.insert(make_pair(blockId, glBlock));
    player->parseEvents(player->getLastFrameEndParsePosition(),
            currentTime*pow(10,6),
            player->getKeyframeEndTime());
    updateMotionBlocks();
}

void ReplayWorld::updateMotionBlocks()
{
    KeyframeBlock block;

    list<int> blackList;
    for(auto &pair : eventBuffer)
    {

        cout << "ON ANALYSE LE BLOC "<< pair.first<<endl;
        //Tracing block 36
        if(pair.first == 36)
        {
            cout << "INIT POS "<< pair.second.initialPosition.pt[0]/gridScale
                    <<" "<<pair.second.initialPosition.pt[1]/gridScale
                    <<" "<<pair.second.initialPosition.pt[2]/gridScale<<endl;
            cout << "DEST POS "<< pair.second.destinationPosition.pt[0]
                 <<" "<<pair.second.destinationPosition.pt[1]
                    <<" "<<pair.second.destinationPosition.pt[2]<<endl;
            cout << "From  Time  TO TIME"<< pair.second.beginDate
                    <<" "<< pair.second.beginDate + pair.second.duration<<endl;
        }
        u8 time = currentTime*pow(10,6);
        u8 readTime = pair.second.beginDate;
        if(time<=readTime+pair.second.duration)
        {
            block.x = pair.second.destinationPosition.pt[0];
            block.y = pair.second.destinationPosition.pt[1];
            block.z = pair.second.destinationPosition.pt[2];
            updatePositionMotion(pair.first,block,time, readTime, pair.second.initialPosition);
        }
        else
        {
            cout << "ON VEUT RETIRER LE BLOC "<< pair.first<<endl;
            blackList.push_back(pair.first);
        }
    }
    for(auto element : blackList)
    {
        cout << "ON RETIRE LE BLOC "<< element<<endl;
        eventBuffer.erase(element);
    }
    blackList.clear();

}

void ReplayWorld::addBlock(bID blockId, KeyframeBlock block) {
    auto *glBlock = new SmartBlocks::SmartBlocksGlBlock(blockId);

    Cell3DPosition pos;
    pos.pt[0] = gridScale*block.x;
    pos.pt[1] = gridScale*block.y;
    pos.pt[2] = gridScale*block.z;
    Vector3D newPos = SLattice::gridToUnscaledWorldPosition_base(pos);
    glBlock->setPosition(newPos);

    Color col;
    col.rgba[0] = block.r/255.0f;
    col.rgba[1] = block.g/255.0f;
    col.rgba[2]= block.b/255.0f;
    col.rgba[3]= 1.0;
    glBlock->setColor(col);
    mapGlBlocks.insert(make_pair(blockId, glBlock));
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
    Cell3DPosition pos;
    pos.pt[0] = block.x*gridScale;
    pos.pt[1] = block.y*gridScale;
    pos.pt[2] = block.z*gridScale;

    Vector3D newPos = SLattice::gridToUnscaledWorldPosition_base(pos);

    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId)
        {

            pair.second->setPosition(pos);
        }
    }

}

void ReplayWorld::updatePositionMotion(u4 blockId, KeyframeBlock block,
        u8 time,u8 readTime, Cell3DPosition initPos)
{
    Vector3D pos;
    Cell3DPosition oldPos = initPos;

    pos.pt[0] = oldPos.pt[0]+(block.x*gridScale-oldPos.pt[0])*(time-readTime-2000)/1000000;
    pos.pt[1] = oldPos.pt[1]+(block.y*gridScale-oldPos.pt[1])*(time-readTime-2000)/1000000;
    pos.pt[2] = oldPos.pt[2]+(block.z*gridScale-oldPos.pt[2])*(time-readTime-2000)/1000000;
    pos.pt[3] = 1;

    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId)
        {

            pair.second->setPosition(pos);
        }
    }

}
Vector3D ReplayWorld::getPosition(u4 blockId)
{
    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId)
        {
            return pair.second->getPosition();
        }
    }
}

void ReplayWorld::glDraw() {
    glPushMatrix();
    objRepere->glDraw();
    for (const auto& pair : mapGlBlocks) {
        ((SmartBlocks::SmartBlocksGlBlock*)pair.second)->glDraw(objBlock);
    }
    glPopMatrix();
}