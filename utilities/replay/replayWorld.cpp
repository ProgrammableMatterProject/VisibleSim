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
    player->parseKeyframe(player->findKeyframeWithTime(currentTime*pow(10,6)));
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



void ReplayWorld::glDraw()
{
    for (const auto& pair : mapGlBlocks) {
        ((SmartBlocks::SmartBlocksGlBlock*)pair.second)->glDraw(objBlock);
    }
}