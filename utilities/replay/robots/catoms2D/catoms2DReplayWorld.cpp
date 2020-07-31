#include <iostream>
#include "replayWorld.h"
#include "replay.hpp"
#include "../../simulatorCore/src/gui/objLoader.h"
#include <map>
#include "../../simulatorCore/src/base/glBlock.h"
#include "../../simulatorCore/src/grid/lattice.h"
#include "replayPlayer.h"
#include "catoms2DReplayWorld.h"
#include "../../simulatorCore/src/utils/utils.h"
using namespace std;
using namespace BaseSimulator::utils;
Catoms2DReplayWorld::Catoms2DReplayWorld(int argc, char *argv[], u8 duration, float scale)
: ReplayWorld (argc, argv, duration, scale){
    cout << "Debug Replay World "<<endl;
    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms2DTextures",
                             "catom2D.obj");
    objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures",
                                         "repere25.obj");

    loadTextures("../../simulatorCore/resources/textures/latticeTextures");
}

Catoms2DReplayWorld::~Catoms2DReplayWorld() {
    delete objBlock;
}

void Catoms2DReplayWorld::updateMotionBlocks()
{
    //TODO
}

void Catoms2DReplayWorld::updateDisplayedValue(u4 blockId, u2 display)
{

}

void Catoms2DReplayWorld::addBlock(bID blockId, KeyframeBlock block) {
    auto *glBlock = new Catoms2D::Catoms2DGlBlock(blockId);

    Cell3DPosition pos;
    pos.pt[0] = gridScale*block.x;
    pos.pt[1] = gridScale*block.y;
    pos.pt[2] = gridScale*block.z;
    Vector3D newPos = HLattice::gridToUnscaledWorldPosition_base(pos);
    glBlock->setPosition(newPos);

    Color col;
    col.rgba[0] = block.r/255.0f;
    col.rgba[1] = block.g/255.0f;
    col.rgba[2]= block.b/255.0f;
    col.rgba[3]= 1.0;
    glBlock->setColor(col);

    mapGlBlocks.insert(make_pair(blockId, glBlock));
}

void Catoms2DReplayWorld::updatePosition(u4 blockId, KeyframeBlock block)
{
    Cell3DPosition pos;
    pos.pt[0] = block.x*gridScale;
    pos.pt[1] = block.y*gridScale;
    pos.pt[2] = block.z*gridScale;

    Vector3D newPos = HLattice::gridToUnscaledWorldPosition_base(pos);

    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId)
        {

            pair.second->setPosition(newPos);
        }
    }

}

void Catoms2DReplayWorld::updatePositionMotion(u4 blockId, KeyframeBlock block,
        u8 time,u8 readTime, Cell3DPosition initPos)
{
    //TODO
}

void Catoms2DReplayWorld::glDrawBackground() {
    static const GLfloat white[]={0.8f,0.8f,0.8f,1.0f},gray[]={0.2f,0.2f,0.2f,1.0f};

    glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
    glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
    glMaterialfv(GL_FRONT,GL_SPECULAR,gray);
    glMaterialf(GL_FRONT,GL_SHININESS,40.0);
    glPushMatrix();
    enableTexture(true);
    glBindTexture(GL_TEXTURE_2D,idTextureLines);
    glScalef(player->gridSizeX*gridScale,
             player->gridSizeX*gridScale,
             gridScale+(player->gridSizeX-1)*gridScale*M_SQRT3_2);
    glBegin(GL_QUADS);
    // bottom
    glNormal3f(0,0,1.0f);
    glTexCoord2f(1.0f,0.25f);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(1.0f,player->gridSizeX+0.25f);
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(0,player->gridSizeX+0.25f);
    glVertex3f(1.0,1.0,0.0f);
    glTexCoord2f(0,0.25f);
    glVertex3f(0.0,1.0,0.0f);
    // top
    glNormal3f(0,0,-1.0f);
    glTexCoord2f(1.0f,0.25f);
    glVertex3f(0.0f,0.0f,1.0f);
    glTexCoord2f(0,0.25f);
    glVertex3f(0.0,1.0,1.0f);
    glTexCoord2f(0,player->gridSizeX+0.25f);
    glVertex3f(1.0,1.0,1.0f);
    glTexCoord2f(1.0f,player->gridSizeX+0.25f);
    glVertex3f(1.0f,0.0f,1.0f);
    // left
    glNormal3f(1.0f,0,0);
    glTexCoord2f(0,0.25f*M_SQRT3_2);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(1.0f,0.25f*M_SQRT3_2);
    glVertex3f(0.0f,1.0f,0.0f);
    glTexCoord2f(1.0f,(player->gridSizeX+0.25f)*M_SQRT3_2);
    glVertex3f(0.0,1.0,1.0f);
    glTexCoord2f(0.0f,(player->gridSizeX+0.25f)*M_SQRT3_2);
    glVertex3f(0.0,0.0,1.0f);
    // right
    glNormal3f(-1.0f,0,0);
    glTexCoord2f(0,0.25f*M_SQRT3_2);
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(0.0f,(player->gridSizeX+0.25f)*M_SQRT3_2);
    glVertex3f(1.0,0.0,1.0f);
    glTexCoord2f(1.0f,(player->gridSizeX+0.25f)*M_SQRT3_2);
    glVertex3f(1.0,1.0,1.0f);
    glTexCoord2f(1.0f,0.25f*M_SQRT3_2);
    glVertex3f(1.0f,1.0f,0.0f);
    glEnd();
    glPopMatrix();
    // draw hexa
    glPushMatrix();
    glBindTexture(GL_TEXTURE_2D,idTextureHexa);
    glScalef(player->gridSizeX*gridScale,
             player->gridSizeX*gridScale,
             gridScale+(player->gridSizeX-1)*gridScale*M_SQRT3_2);
    float h=((player->gridSizeX-1)+1.0/M_SQRT3_2)/2.0;
    glBegin(GL_QUADS);
    // back
    glNormal3f(0,-1.0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,1.0f,0.0f);
    glTexCoord2f(player->gridSizeX/3.0f,0);
    glVertex3f(1.0f,1.0f,0.0f);
    glTexCoord2f(player->gridSizeX/3.0f,h);
    glVertex3f(1.0f,1.0,1.0f);
    glTexCoord2f(0,h);
    glVertex3f(0.0,1.0,1.0f);
    // front
    glNormal3f(0,1.0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(0,h);
    glVertex3f(0.0,0.0,1.0f);
    glTexCoord2f(player->gridSizeX/3.0f,h);
    glVertex3f(1.0f,0.0,1.0f);
    glTexCoord2f(player->gridSizeX/3.0f,0);
    glVertex3f(1.0f,0.0f,0.0f);
    glEnd();
    glPopMatrix();
    // draw the axes
    glPushMatrix();
    glScalef(0.05f,0.05f,0.05f);
    objRepere->glDraw();
    glPopMatrix();
}

void Catoms2DReplayWorld::loadTextures(const string &str) {


    string path = str+"//hexa.tga";
    int lx,ly;
    idTextureHexa = loadTexture(path.c_str(),lx,ly);
    path = str+"//lignes.tga";
    idTextureLines = loadTexture(path.c_str(),lx,ly);
}
