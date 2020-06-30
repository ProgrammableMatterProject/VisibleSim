#include <iostream>
#include "replayWorld.h"
#include "replay.hpp"
#include "../../simulatorCore/src/gui/objLoader.h"
#include <map>
#include "../../simulatorCore/src/base/glBlock.h"

using namespace std;

ReplayWorld::ReplayWorld(int argc, char *argv[], u8 duration)
{
    cout << "Debug Replay World "<<endl;
    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/smartBlocksTextures",
                                        "smartBlockSimple.obj");
    exportDuration = (float)duration*pow(10,-6);
}

ReplayWorld::~ReplayWorld()
{

}

//TODO quand les GlBlocks seront importés
void ReplayWorld::updateMap()
{
    for (const auto& pair : mapGlBlocks) {
        delete (GlBlock*)pair.second;
    }
    mapGlBlocks.clear();
    //TODO Changer l'échelle de temps
    player->parseFrame(currentTime*pow(10,6));

}

void ReplayWorld::addBlock(bID blockId, Vector3D pos, Color col)
{
//    SmartBlocks::SmartBlocksGlBlock *glBlock = new SmartBlocks::SmartBlocksGlBlock(blockId);
    auto *glBlock = new SmartBlocks::SmartBlocksGlBlock(blockId);
    glBlock->setPosition(pos);
    glBlock->setColor(col);
    //mapGlBlocks.insert(make_pair(blockId, glBlock));
    mapGlBlocks.insert(make_pair(blockId, glBlock));

}

void ReplayWorld::updateColor(u4 blockId, Color col)
{
    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId)
        {
            pair.second->setColor(col);
        }
    }
}

void ReplayWorld::updatePosition(u4 blockId, Vector3D pos)
{
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

void ReplayWorld::glDraw()
{
    //TODO optimisable
    for (const auto& pair : mapGlBlocks) {
        ((SmartBlocks::SmartBlocksGlBlock*)pair.second)->glDraw(objBlock);
    }
}