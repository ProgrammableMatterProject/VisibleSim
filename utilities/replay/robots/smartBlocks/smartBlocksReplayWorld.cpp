#include <iostream>
#include "replayWorld.h"
#include "replay.hpp"
#include "../../simulatorCore/src/gui/objLoader.h"
#include <map>
#include "../../simulatorCore/src/base/glBlock.h"
#include "../../simulatorCore/src/grid/lattice.h"
#include "replayPlayer.h"
#include "smartBlocksReplayWorld.h"
using namespace std;

SmartBlocksReplayWorld::SmartBlocksReplayWorld(int argc, char *argv[], u8 duration, float scale)
: ReplayWorld (argc, argv, duration, scale){
    cout << "Debug Replay World "<<endl;
    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/smartBlocksTextures",
                                        "smartBlockSimple.obj");
    objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures",
                                         "repere25.obj");

    loadTextures("../../simulatorCore/resources/textures/latticeTextures");
}

SmartBlocksReplayWorld::~SmartBlocksReplayWorld() {
    delete objBlock;
}

void SmartBlocksReplayWorld::updateMotionBlocks()
{
    KeyframeBlock block;

    list<int> blackList;
    for(auto &pair : eventBuffer)
    {

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
            blackList.push_back(pair.first);
        }
    }
    for(auto element : blackList)
    {
        eventBuffer.erase(element);
    }
    blackList.clear();

}

void SmartBlocksReplayWorld::addBlock(bID blockId, KeyframeBlock block) {
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

    glBlock->setDisplayedValue(block.displayedValue);
    mapGlBlocks.insert(make_pair(blockId, glBlock));
}

void SmartBlocksReplayWorld::updatePosition(u4 blockId, KeyframeBlock block)
{
    Cell3DPosition pos;
    pos.pt[0] = block.x*gridScale;
    pos.pt[1] = block.y*gridScale;
    pos.pt[2] = block.z*gridScale;

    Vector3D newPos = SLattice::gridToUnscaledWorldPosition_base(pos);

    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId)
        {

            pair.second->setPosition(newPos);
        }
    }

}

void SmartBlocksReplayWorld::updatePositionMotion(u4 blockId, KeyframeBlock block,
        u8 time,u8 readTime, Cell3DPosition initPos)
{
    Vector3D pos;
    Cell3DPosition oldPos = initPos;

    pos.pt[0] = oldPos.pt[0]+(block.x*gridScale-oldPos.pt[0])*(time-readTime-2000)/1000000;
    pos.pt[1] = oldPos.pt[1]+(block.y*gridScale-oldPos.pt[1])*(time-readTime-2000)/1000000;
    pos.pt[2] = oldPos.pt[2]+(block.z*gridScale-oldPos.pt[2])*(time-readTime-2000)/1000000;
    pos.pt[3] = 1;
    cout<<"DEBUGGAGE 3 : "<<pos.pt[0]<< " " <<pos.pt[1]<<" "<<pos.pt[2]<<endl;
    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId)
        {

            pair.second->setPosition(pos);
        }
    }

}


void SmartBlocksReplayWorld::updateDisplayedValue(u4 blockId, u2 display)
{
    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId) {
            dynamic_cast<SmartBlocks::SmartBlocksGlBlock*>(pair.second)->setDisplayedValue(display);
        }
    }
}

void SmartBlocksReplayWorld::glDrawBackground() {
    //cout<<"Drawing background "<<gridScale<<endl;
    static const GLfloat white[]={1.0,1.0,1.0,1.0},
            gray[]={0.2,0.2,0.2,1.0};

    glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
    glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
    glMaterialfv(GL_FRONT,GL_SPECULAR,gray);
    glMaterialf(GL_FRONT,GL_SHININESS,40.0);
    glPushMatrix();
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D,idTextureFloor);
    glNormal3f(0,0,1.0f);
    glScalef(gridScale*player->gridSizeX,
             gridScale*player->gridSizeY,1.0f);
    glBegin(GL_QUADS);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(static_cast<float>(player->gridSizeX)/4.0f,0); // textureCarre is a used as a 4x4 square texture
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(static_cast<float>(player->gridSizeX)/4.0f,static_cast<float>(player->gridSizeY)/4.0f);
    glVertex3f(1.0,1.0,0.0f);
    glTexCoord2f(0,static_cast<float>(player->gridSizeY)/4.0f);
    glVertex3f(0.0,1.0,0.0f);
    glEnd();
    glPopMatrix();
}

void SmartBlocksReplayWorld::loadTextures(const string &str) {


    string path = str+"/textureCarre.tga";
    int lx,ly;
    idTextureFloor = loadTexture(path.c_str(),lx,ly);
    cout<<"Loading texture"<<endl;
    cout<<"LOADING TEXTURE SBWORLD"<<endl;

}
