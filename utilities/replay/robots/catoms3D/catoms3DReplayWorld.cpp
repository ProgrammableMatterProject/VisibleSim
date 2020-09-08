#include <iostream>
#include "replayWorld.h"
#include "replay.hpp"
#include "../../simulatorCore/src/gui/objLoader.h"
#include <map>
#include "../../simulatorCore/src/base/glBlock.h"
#include "../../simulatorCore/src/grid/lattice.h"
#include "replayPlayer.h"
#include "catoms3DReplayWorld.h"
#include "../../simulatorCore/src/utils/utils.h"

using namespace std;
using namespace BaseSimulator::utils;
Catoms3DReplayWorld::Catoms3DReplayWorld(int argc, char *argv[], u8 duration, float scale)
: ReplayWorld (argc, argv, duration, scale){
    cout << "Debug Replay World "<<endl;
    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures",
            "catom3DV2connectorID.obj");
    objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures",
            "repereCatom3D.obj");
    //exportDuration = (float)duration*pow(10,-6);
    //endZoom = exportDuration;
    //gridScale = scale;
    loadTextures("../../simulatorCore/resources/textures/latticeTextures");
}

Catoms3DReplayWorld::~Catoms3DReplayWorld() {
    delete objBlock;
}

void Catoms3DReplayWorld::updateMotionBlocks()
{
    list<int> blackList;
    for(auto &pair : eventBuffer)
    {
        u8 time = currentTime*pow(10,6);
        u8 readTime = pair.second.beginDate;
        if(time<=readTime+pair.second.duration)
        {
            Matrix newMatrix = static_cast<Catoms3DRotationEvent*>
                    (&pair.second)->getMatrixFromTime(time);
            updatePositionMotionWithMatrix(pair.first,newMatrix);
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

void Catoms3DReplayWorld::updateDisplayedValue(u4 blockId, u2 display)
{

}

void Catoms3DReplayWorld::addBlock(bID blockId, KeyframeBlock block) {
    auto *glBlock = new Catoms3D::Catoms3DGlBlock(blockId);
    cout<<"Adding new block"<<endl;

    Cell3DPosition pos;
    pos.pt[0] = block.x;
    pos.pt[1] = block.y;
    pos.pt[2] = block.z;
    /* Unused with catoms : they are only using matrix*/
//    Vector3D newPos = FCCLattice::gridToUnscaledWorldPosition_base(pos);
//    newPos.pt[0] *= gridScale;
//    newPos.pt[1] *= gridScale;
//    newPos.pt[2] *= gridScale;
    //glBlock->setPosition(newPos);

    glBlock->mat = getMatrixFromPositionAndOrientation(pos,block.rotation);
    Color col;
    col.rgba[0] = block.r/255.0f;
    col.rgba[1] = block.g/255.0f;
    col.rgba[2]= block.b/255.0f;
    col.rgba[3]= 1.0;

    glBlock->setColor(col);

    mapGlBlocks.insert(make_pair(blockId, glBlock));
}

void Catoms3DReplayWorld::updatePosition(u4 blockId, KeyframeBlock block)
{
    Cell3DPosition pos;
    pos.pt[0] = block.x;
    pos.pt[1] = block.y;
    pos.pt[2] = block.z;

    for (auto& pair : mapGlBlocks) {
        if(pair.first==blockId)
        {
            Catoms3D::Catoms3DGlBlock* newBlock =
                    dynamic_cast<Catoms3D::Catoms3DGlBlock*>(pair.second); //Compile bien
            newBlock->mat = getMatrixFromPositionAndOrientation(pos,block.rotation);
            pair.second = newBlock;
            //pair.second->setPosition(newPos);
        }
    }

}

void Catoms3DReplayWorld::updatePositionMotion(u4 blockId, KeyframeBlock block,
        u8 time,u8 readTime, Cell3DPosition initPos)
{

}

void Catoms3DReplayWorld::glDrawBackground() {
    static const GLfloat white[]={0.8f,0.8f,0.8f,1.0f},
            gray[]={0.2f,0.2f,0.2f,1.0f};
    glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
    glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
    glMaterialfv(GL_FRONT,GL_SPECULAR,white);
    glMaterialf(GL_FRONT,GL_SHININESS,40.0);
    glPushMatrix();
    enableTexture(true);
    glBindTexture(GL_TEXTURE_2D,idTextureGrid);
    glTranslatef(0,0,gridScale*(0.5-M_SQRT2_2));
    glScalef(player->gridSizeX*gridScale,player->gridSizeY*gridScale,player->gridSizeZ*gridScale*M_SQRT2_2);
    glBegin(GL_QUADS);
    // bottom
    glNormal3f(0,0,1.0f);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,-0.0f);
    glTexCoord2f(0.5f*player->gridSizeX,0);
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(0.5f*player->gridSizeX,0.5f*player->gridSizeY);
    glVertex3f(1.0,1.0,0.0f);
    glTexCoord2f(0,0.5f*player->gridSizeY);
    glVertex3f(0.0,1.0,0.0f);
    // top
    glNormal3f(0,0,-1.0f);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,1.0f);
    glTexCoord2f(0.5f*player->gridSizeX,0);
    glVertex3f(0.0,1.0,1.0f);
    glTexCoord2f(0.5f*player->gridSizeX,0.5f*player->gridSizeY);
    glVertex3f(1.0,1.0,1.0f);
    glTexCoord2f(0,0.5f*player->gridSizeY);
    glVertex3f(1.0f,0.0f,1.0f);
    glEnd();
    // draw hexa
    glBindTexture(GL_TEXTURE_2D,idTextureHexa);
    glBegin(GL_QUADS);
    // left
    glNormal3f(1.0f,0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(player->gridSizeY/3.0f,0);
    glVertex3f(0.0f,1.0f,0.0f);
    glTexCoord2f(player->gridSizeY/3.0f,player->gridSizeZ/1.5f);
    glVertex3f(0.0,1.0,1.0f);
    glTexCoord2f(0,player->gridSizeZ/1.5f);
    glVertex3f(0.0,0.0,1.0f);
    // right
    glNormal3f(-1.0f,0,0);
    glTexCoord2f(0,0);
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(0,player->gridSizeZ/1.5f);
    glVertex3f(1.0,0.0,1.0f);
    glTexCoord2f(player->gridSizeY/3.0f,player->gridSizeZ/1.5f);
    glVertex3f(1.0,1.0,1.0f);
    glTexCoord2f(player->gridSizeY/3.0f,0);
    glVertex3f(1.0f,1.0f,0.0f);
    // back
    glNormal3f(0,-1.0f,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,1.0f,0.0f);
    glTexCoord2f(player->gridSizeX/3.0f,0);
    glVertex3f(1.0f,1.0f,0.0f);
    glTexCoord2f(player->gridSizeX/3.0f,player->gridSizeZ/1.5f);
    glVertex3f(1.0f,1.0,1.0f);
    glTexCoord2f(0,player->gridSizeZ/1.5f);
    glVertex3f(0.0,1.0,1.0f);
    // front
    glNormal3f(0,1.0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(0,player->gridSizeZ/1.5f);
    glVertex3f(0.0,0.0,1.0f);
    glTexCoord2f(player->gridSizeX/3.0f,player->gridSizeZ/1.5f);
    glVertex3f(1.0f,0.0,1.0f);
    glTexCoord2f(player->gridSizeX/3.0f,0);
    glVertex3f(1.0f,0.0f,0.0f);
    glEnd();
    glPopMatrix();

}

void Catoms3DReplayWorld::loadTextures(const string &str) {


    string path = str+"//hexa.tga";
    int lx,ly;
    idTextureHexa = loadTexture(path.c_str(),lx,ly);
    path = str+"//textureCarre.tga";
    idTextureGrid = loadTexture(path.c_str(),lx,ly);
}

Matrix Catoms3DReplayWorld::getMatrixFromPositionAndOrientation(const Cell3DPosition &pos,
                                                          uint8_t code) {
    uint8_t orientation = code % 12;
    uint8_t up = code / 12;

    Matrix M1, M2, M3, M;
    M1.setRotationZ(tabOrientationAngles[orientation][2]);
    M2.setRotationY(tabOrientationAngles[orientation][1]);
    M3.setRotationX(tabOrientationAngles[orientation][0] + up * 180.0);
    M = M2 * M1;
    M1 = M3 * M;
    Vector3D newPos = FCCLattice::gridToUnscaledWorldPosition_base(pos);
    newPos.pt[0] *= gridScale;
    newPos.pt[1] *= gridScale;
    newPos.pt[2] *= gridScale;
    M2.setTranslation(newPos);
    M = M2 * M1;
    return M;
}

void Catoms3DReplayWorld::updatePositionMotionWithMatrix(u4 blockId, Matrix newMatrix)
{
    for(auto& pair : mapGlBlocks)
    {
        if(pair.first == blockId)
        {
            dynamic_cast<Catoms3D::Catoms3DGlBlock*>(pair.second)->mat = newMatrix;
        }
    }
}
