#include <iostream>
#include "replayWorld.h"
#include "replay.hpp"
#include "../../simulatorCore/src/gui/objLoader.h"
#include <map>
#include "../../simulatorCore/src/base/glBlock.h"
#include "../../simulatorCore/src/grid/lattice.h"
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
    for (const auto& pair : mapGlBlocks) {
        delete (GlBlock*)pair.second;
    }
    mapGlBlocks.clear();
    player->parseFrame(currentTime*pow(10,6));

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

void ReplayWorld::glDraw() {
    glPushMatrix();
    objRepere->glDraw();
    for (const auto& pair : mapGlBlocks) {
        ((SmartBlocks::SmartBlocksGlBlock*)pair.second)->glDraw(objBlock);
    }
    glPopMatrix();
}