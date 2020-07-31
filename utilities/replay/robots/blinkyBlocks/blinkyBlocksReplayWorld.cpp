#include <iostream>
#include "replayWorld.h"
#include "replay.hpp"
#include "../../simulatorCore/src/gui/objLoader.h"
#include <map>
#include "../../simulatorCore/src/base/glBlock.h"
#include "../../simulatorCore/src/grid/lattice.h"
#include "replayPlayer.h"
#include "blinkyBlocksReplayWorld.h"
using namespace std;

BlinkyBlocksReplayWorld::BlinkyBlocksReplayWorld(int argc, char *argv[], u8 duration, float scale)
: ReplayWorld (argc, argv, duration, scale){
    cout << "Debug Replay World "<<endl;
    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/blinkyBlocksTextures",
                                        "blinkyBlockCentered.obj");
    objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures",
            "repere25.obj");

    loadTextures("../../simulatorCore/resources/textures/latticeTextures");
}

BlinkyBlocksReplayWorld::~BlinkyBlocksReplayWorld() {
    delete objBlock;
}



void BlinkyBlocksReplayWorld::addBlock(bID blockId, KeyframeBlock block) {
    auto *glBlock = new BlinkyBlocks::BlinkyBlocksGlBlock(blockId);

    Cell3DPosition pos;
    pos.pt[0] = gridScale*block.x;
    pos.pt[1] = gridScale*block.y;
    pos.pt[2] = gridScale*block.z;
    Vector3D newPos = SCLattice::gridToUnscaledWorldPosition_base(pos);
    glBlock->setPosition(newPos);

    Color col;
    col.rgba[0] = block.r/255.0f;
    col.rgba[1] = block.g/255.0f;
    col.rgba[2]= block.b/255.0f;
    col.rgba[3]= 1.0;
    glBlock->setColor(col);
    mapGlBlocks.insert(make_pair(blockId, glBlock));
}

void BlinkyBlocksReplayWorld::updatePosition(u4 blockId, KeyframeBlock block)
{
    Cell3DPosition pos;
    pos.pt[0] = block.x*gridScale;
    pos.pt[1] = block.y*gridScale;
    pos.pt[2] = block.z*gridScale;

    Vector3D newPos = SCLattice::gridToUnscaledWorldPosition_base(pos);

    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId)
        {

            pair.second->setPosition(newPos);
        }
    }

}

void BlinkyBlocksReplayWorld::glDrawBackground() {
    static const GLfloat white[]={0.8f,0.8f,0.8f,1.0f},
            gray[]={0.2f,0.2f,0.2f,1.0};
    glPopMatrix();
    glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
    glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
    glMaterialfv(GL_FRONT,GL_SPECULAR,gray);
    glMaterialf(GL_FRONT,GL_SHININESS,40.0);
    glPushMatrix();
    enableTexture(true);
    glBindTexture(GL_TEXTURE_2D,idTextureWall);
    glScalef(player->gridSizeX*gridScale,
             player->gridSizeY*gridScale,
             player->gridSizeZ*gridScale);
    glBegin(GL_QUADS);
    // bottom
    glNormal3f(0,0,1.0f);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(player->gridSizeX/4.0f,0);
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(player->gridSizeX/4.0f,player->gridSizeY/4.0f);
    glVertex3f(1.0,1.0,0.0f);
    glTexCoord2f(0,player->gridSizeY/4.0f);
    glVertex3f(0.0,1.0,0.0f);
    // top
    glNormal3f(0,0,-1.0f);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,1.0f);
    glTexCoord2f(0,player->gridSizeY/4.0f);
    glVertex3f(0.0,1.0,1.0f);
    glTexCoord2f(player->gridSizeX/4.0f,player->gridSizeY/4.0f);
    glVertex3f(1.0,1.0,1.0f);
    glTexCoord2f(player->gridSizeX/4.0f,0);
    glVertex3f(1.0f,0.0f,1.0f);
    // left
    glNormal3f(1.0,0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(player->gridSizeY/4.0f,0);
    glVertex3f(0.0f,1.0f,0.0f);
    glTexCoord2f(player->gridSizeY/4.0f,player->gridSizeZ/4.0f);
    glVertex3f(0.0,1.0,1.0f);
    glTexCoord2f(0,player->gridSizeZ/4.0f);
    glVertex3f(0.0,0.0,1.0f);
    // right
    glNormal3f(-1.0,0,0);
    glTexCoord2f(0,0);
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(0,player->gridSizeZ/4.0f);
    glVertex3f(1.0,0.0,1.0f);
    glTexCoord2f(player->gridSizeY/4.0f,player->gridSizeZ/4.0f);
    glVertex3f(1.0,1.0,1.0f);
    glTexCoord2f(player->gridSizeY/4.0f,0);
    glVertex3f(1.0f,1.0f,0.0f);
    // back
    glNormal3f(0,-1.0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,1.0f,0.0f);
    glTexCoord2f(player->gridSizeX/4.0f,0);
    glVertex3f(1.0f,1.0f,0.0f);
    glTexCoord2f(player->gridSizeX/4.0f,player->gridSizeZ/4.0f);
    glVertex3f(1.0f,1.0,1.0f);
    glTexCoord2f(0,player->gridSizeZ/4.0f);
    glVertex3f(0.0,1.0,1.0f);
    // front
    glNormal3f(0,1.0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(0,player->gridSizeZ/4.0f);
    glVertex3f(0.0,0.0,1.0f);
    glTexCoord2f(player->gridSizeX/4.0f,player->gridSizeZ/4.0f);
    glVertex3f(1.0f,0.0,1.0f);
    glTexCoord2f(player->gridSizeX/4.0f,0);
    glVertex3f(1.0f,0.0f,0.0f);
    glEnd();
    glPopMatrix();
    // draw the axes
}

void BlinkyBlocksReplayWorld::loadTextures(const string &str) {


    string path = str+"/texture_plane.tga";
    int lx,ly;
    idTextureWall = loadTexture(path.c_str(),lx,ly);
    cout<<"Loading texture"<<endl;
    cout<<"LOADING TEXTURE SBWORLD"<<endl;

}
