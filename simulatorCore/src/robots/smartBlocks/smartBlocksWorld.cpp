/*
 * smartBlockWorld.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include <cstdlib>
#include <string>

#include "smartBlocksWorld.h"
#include "smartBlocksBlock.h"
#include "smartBlocksGlBlock.h"
#include "../../events/scheduler.h"
#include "../../events/events.h"
#include "../../utils/configExporter.h"
#include "../../replay/replayExporter.h"

using namespace std;

namespace SmartBlocks {

[[maybe_unused]] SmartBlocksWorld::SmartBlocksWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                   int argc, char *argv[]):World(argc, argv) {
    cout << TermColor::LifecycleColor << "SmartBlocksWorld constructor" << TermColor::Reset << endl;
    idTextureFloor=0;
    if (GlutContext::GUIisEnabled) {
        string directory;
#ifdef WIN32
        directory = string(ROOT_DIR) + "/simulatorCore/resources/textures/smartBlocksTextures";
#else
        directory = "../../simulatorCore/resources/textures/smartBlocksTextures";
#endif
        objBlock = new ObjLoader::ObjLoader(directory.c_str(),"smartBlockSimple.obj");
        objBlockForPicking = new ObjLoader::ObjLoader(directory.c_str(),"smartBlockPicking.obj");
#ifdef WIN32
        directory = string(ROOT_DIR) + "/simulatorCore/resources/textures/latticeTextures";
#else
        directory = "../../simulatorCore/resources/textures/latticeTextures";
#endif
        objRepere = new ObjLoader::ObjLoader(directory.c_str(),"repere25.obj");
    }

    lattice = new SLattice(gridSize, gridScale.isZero() ? defaultBlockSize : gridScale);
}

SmartBlocksWorld::~SmartBlocksWorld() {
    cout << TermColor::LifecycleColor << "SmartBlocksWorld destructor" << TermColor::Reset << endl;
}

void SmartBlocksWorld::deleteWorld() {
    delete((SmartBlocksWorld*)world);
    world=nullptr;
}

void SmartBlocksWorld::addBlock(bID blockId, BlockCodeBuilder bcb,
                                const Cell3DPosition &pos, const Color &col,
                                uint8_t orient) {
    if (blockId > maxBlockId)
        maxBlockId = blockId;
    else if (blockId == 0)
        blockId = incrementBlockId();

    auto *smartBlock = new SmartBlocksBlock(blockId, bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                             (smartBlock->blockId, (BaseSimulator::BuildingBlock*)smartBlock) );
    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), smartBlock));

    auto *glBlock = new SmartBlocksGlBlock(blockId);
    mapGlBlocks.insert(make_pair(blockId, glBlock));
    smartBlock->setGlBlock(glBlock);
    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeAddModule(getScheduler()->now(), blockId);
    smartBlock->setPosition(pos);
    smartBlock->setColor(col);

    
    if (lattice->isInGrid(pos)) {
        lattice->insert(smartBlock, pos);
    } else {
        cerr << "ERROR : BLOCK #" << blockId << " out of the grid !!!!!" << endl;
        exit(1);
    }
}

void SmartBlocksWorld::linkBlock(const Cell3DPosition &pos) {
    SmartBlocksBlock *ptrNeighbor;
    auto *ptrBlock = (SmartBlocksBlock*)lattice->getBlock(pos);
    vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
    Cell3DPosition nPos;

    // Check neighbors for each interface
    for (int i = 0; i < 4; i++) {
        nPos = pos + nRelCells[i];
        ptrNeighbor = (SmartBlocksBlock*)lattice->getBlock(nPos);
        if (ptrNeighbor) {
            (ptrBlock)->getInterface(SLattice::Direction(i))->
                connect(ptrNeighbor->getInterface(SLattice::Direction(
                                                      lattice->getOppositeDirection(i))));

#ifdef DEBUG_NEIGHBORHOOD
            OUTPUT << "connection #" << (ptrBlock)->blockId <<
                " to #" << ptrNeighbor->blockId << endl;
#endif
        } else {
            (ptrBlock)->getInterface(SLattice::Direction(i))->connect(nullptr);
        }
    }
}

void SmartBlocksWorld::glDraw() {
        glDisable(GL_TEXTURE_2D);
        lock();
        BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
        if (bb) bb->blockCode->onGlDraw();

        for (const auto& pair : mapGlBlocks) {
            ((SmartBlocksGlBlock*)pair.second)->glDraw(objBlock);
        }
        unlock();

        /*// drawing the mobiles
          Physics::glDraw();
        */
}

void SmartBlocksWorld::glDrawIdByMaterial() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);

    int n;
    lock();
    for (const auto& pair : mapGlBlocks) {
        n = pair.first * numPickingTextures;
        ((SmartBlocksGlBlock*)pair.second)->glDrawIdByMaterial(objBlockForPicking,n);
    }
    unlock();
    glPopMatrix();
}

void SmartBlocksWorld::glDrawId() {
    glPushMatrix();
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((SmartBlocksGlBlock*)pair.second)->glDrawId(objBlock, pair.first);
    }
    unlock();
    glPopMatrix();
}

void SmartBlocksWorld::glDrawBackground() {
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
    glScalef(lattice->gridSize[0]*lattice->gridScale[0],
             lattice->gridSize[1]*lattice->gridScale[1],1.0f);
    glBegin(GL_QUADS);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(static_cast<float>(lattice->gridSize[0])/4.0f,0); // textureCarre is a used as a 4x4 square texture
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(static_cast<float>(lattice->gridSize[0])/4.0f,static_cast<float>(lattice->gridSize[1])/4.0f);
    glVertex3f(1.0,1.0,0.0f);
    glTexCoord2f(0,static_cast<float>(lattice->gridSize[1])/4.0f);
    glVertex3f(0.0,1.0,0.0f);
    glEnd();
    glPopMatrix();
    // draw the axes
    objRepere->glDraw();
}

void SmartBlocksWorld::loadTextures(const string &str) {
    if (GlutContext::GUIisEnabled) {
        string path = str+"/textureCarre.tga";
        int lx,ly;
        idTextureFloor = loadTexture(path.c_str(),lx,ly);
    }
}

void SmartBlocksWorld::setSelectedFace(int n) {
    numSelectedGlBlock = n / numPickingTextures;
    string name = objBlockForPicking->getObjMtlName(n % numPickingTextures);

    if (name == "Material__72") numSelectedFace = SLattice::South;
    else if (name == "Material__66") numSelectedFace = SLattice::East;
    else if (name == "Material__71") numSelectedFace = SLattice::West;
    else if (name == "Material__68") numSelectedFace = SLattice::North;
    else {
        numSelectedFace = 4;  // Top
    }

    // cerr << "SET " << name << " = " << numSelectedFace << " = "
    //      << lattice->getDirectionString(numSelectedFace) << endl;
}

void SmartBlocksWorld::exportConfiguration() {
    auto exporter = SmartBlocksConfigExporter(this);
    exporter.exportConfiguration();
}


} // SmartBlocks namespace
