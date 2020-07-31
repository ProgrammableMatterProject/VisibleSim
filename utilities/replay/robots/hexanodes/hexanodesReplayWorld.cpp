#include <iostream>
#include "replayWorld.h"
#include "replay.hpp"
#include "../../simulatorCore/src/gui/objLoader.h"
#include <map>
#include "../../simulatorCore/src/base/glBlock.h"
#include "../../simulatorCore/src/grid/lattice.h"
#include "replayPlayer.h"
#include "hexanodesReplayWorld.h"
#include "../../simulatorCore/src/utils/utils.h"
using namespace std;
using namespace BaseSimulator::utils;
HexanodesReplayWorld::HexanodesReplayWorld(int argc, char *argv[], u8 duration, float scale)
: ReplayWorld (argc, argv, duration, scale){
    cout << "Debug Replay World "<<endl;
    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/hexanodesTextures","hexanodes.obj");
    objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures",
                                         "repere25.obj");

    loadTextures("../../simulatorCore/resources/textures/latticeTextures");
}

HexanodesReplayWorld::~HexanodesReplayWorld() {
    delete objBlock;
}

void HexanodesReplayWorld::updateMotionBlocks()
{
    //TODO

}

void HexanodesReplayWorld::updateDisplayedValue(u4 blockId, u2 display)
{

}

void HexanodesReplayWorld::addBlock(bID blockId, KeyframeBlock block) {
    auto *glBlock = new Hexanodes::HexanodesGlBlock(blockId);

    Cell3DPosition pos;
    pos.pt[0] = gridScale*block.x;
    pos.pt[1] = gridScale*block.y;
    pos.pt[2] = gridScale*block.z;
    Vector3D newPos = HHLattice::gridToUnscaledWorldPosition_base(pos);
    glBlock->setPosition(newPos);

    Color col;
    col.rgba[0] = block.r/255.0f;
    col.rgba[1] = block.g/255.0f;
    col.rgba[2]= block.b/255.0f;
    col.rgba[3]= 1.0;
    glBlock->setColor(col);

    mapGlBlocks.insert(make_pair(blockId, glBlock));
}

void HexanodesReplayWorld::updatePosition(u4 blockId, KeyframeBlock block)
{
    Cell3DPosition pos;
    pos.pt[0] = block.x*gridScale;
    pos.pt[1] = block.y*gridScale;
    pos.pt[2] = block.z*gridScale;

    Vector3D newPos = HHLattice::gridToUnscaledWorldPosition_base(pos);

    for (const auto& pair : mapGlBlocks) {
        if(pair.first==blockId)
        {

            pair.second->setPosition(newPos);
        }
    }

}

void HexanodesReplayWorld::updatePositionMotion(u4 blockId, KeyframeBlock block,
        u8 time,u8 readTime, Cell3DPosition initPos)
{
    //TODO

}

void HexanodesReplayWorld::glDrawBackground() {
    static const GLfloat white[]={1.0,1.0,1.0,1.0},
            gray[]={0.2,0.2,0.2,1.0},black[]={0.0,0.0,0.0,1.0};
    glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
    glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
    glMaterialfv(GL_FRONT,GL_SPECULAR,black);
    glMaterialf(GL_FRONT,GL_SHININESS,40.0);
    glPushMatrix();
    enableTexture(true);
    glBindTexture(GL_TEXTURE_2D,idTextureWall);
    glNormal3f(0,0,1.0f);
    //glScalef(player->gridSizeX*gridScale,player->gridSizeY*gridScale*M_SQRT3_2,1.0f);

    glBegin(GL_QUADS);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(0.5*player->gridSizeX,0);
    glVertex3f(player->gridSizeX*gridScale,0.0f,0.0f);
    glTexCoord2f(0.5*(player->gridSizeX+0.5*player->gridSizeY),0.5*player->gridSizeY);
    glVertex3f((player->gridSizeX+0.5*player->gridSizeY)*gridScale,player->gridSizeY*gridScale*M_SQRT3_2,0.0f);
    glTexCoord2f(0.25*player->gridSizeY,0.5*player->gridSizeY);
    glVertex3f(0.5*player->gridSizeY*gridScale,player->gridSizeY*gridScale*M_SQRT3_2,0.0f);
    glEnd();
    glPopMatrix();
    // draw the axes
    //objRepere->glDraw();

    glPushMatrix();
}

void HexanodesReplayWorld::loadTextures(const string &str) {


    string path = str+"/hexanodesgrid.tga";
    int lx,ly;
    idTextureWall = loadTexture(path.c_str(),lx,ly);
    path=str+"/../smartBlocksTextures/digits.tga";
    idTextureDigits = loadTexture(path.c_str(),lx,ly);
}
