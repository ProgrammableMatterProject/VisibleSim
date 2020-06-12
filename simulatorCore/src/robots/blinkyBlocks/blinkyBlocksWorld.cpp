/*
 * blinkyBlockWorld.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <string>
#include <stdlib.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

#include "blinkyBlocksWorld.h"
#include "blinkyBlocksBlock.h"
#include "blinkyBlocksEvents.h"
#include "../../utils/configExporter.h"
#include "../../utils/trace.h"
#include "../../replay/replayExporter.h"

using namespace std;

namespace BlinkyBlocks {

BlinkyBlocksWorld::BlinkyBlocksWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                     int argc, char *argv[]):World(argc, argv) {
    OUTPUT << TermColor::LifecycleColor << "BlinkyBlocksWorld constructor" << TermColor::Reset << endl;

    if (GlutContext::GUIisEnabled) {
        objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/blinkyBlocksTextures",
                                            "blinkyBlockCentered.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/blinkyBlocksTextures",
                                                      "blinkyBlockPickingCentered.obj");
        objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures","repere25.obj");
    }

    lattice = new SCLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
}

BlinkyBlocksWorld::~BlinkyBlocksWorld() {
#ifdef OBJECT_LIFECYCLE_DEBUG
    OUTPUT << "BlinkyBlocksWorld destructor" << endl;
#endif
    /*	block linked are deleted by world::~world() */
}

void BlinkyBlocksWorld::deleteWorld() {
    delete((BlinkyBlocksWorld*)world);
}

void BlinkyBlocksWorld::addBlock(bID blockId, BlockCodeBuilder bcb,
                                 const Cell3DPosition &pos, const Color &col,
                                 short orientation, bool master) {
    if (blockId > maxBlockId)
        maxBlockId = blockId;
    else if (blockId == 0)
        blockId = incrementBlockId();

    BlinkyBlocksBlock *blinkyBlock = new BlinkyBlocksBlock(blockId, bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                             (blinkyBlock->blockId, (BaseSimulator::BuildingBlock*)blinkyBlock));
    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), blinkyBlock));

    BlinkyBlocksGlBlock *glBlock = new BlinkyBlocksGlBlock(blockId);
    mapGlBlocks.insert(make_pair(blockId, glBlock));
    blinkyBlock->setGlBlock(glBlock);

    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeAddModule(getScheduler()->now(), blockId);
    blinkyBlock->setPosition(pos);
    blinkyBlock->setColor(col);

    if (lattice->isInGrid(pos)) {
        lattice->insert(blinkyBlock, pos);
    } else {
        ERRPUT << "ERROR : BLOCK #" << blockId << " out of the grid !!!!!" << endl;
        exit(1);
    }
}

void BlinkyBlocksWorld::linkBlock(const Cell3DPosition &pos) {
    BlinkyBlocksBlock *ptrNeighbor;
    BlinkyBlocksBlock *ptrBlock = (BlinkyBlocksBlock*)lattice->getBlock(pos);
    vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
    Cell3DPosition nPos;


    // Check neighbors for each interface
    for (int i = 0; i < 6; i++) {
        nPos = pos + nRelCells[i];
        ptrNeighbor = (BlinkyBlocksBlock*)lattice->getBlock(nPos);
        if (ptrNeighbor) {
            (ptrBlock)->getInterface(SCLattice::Direction(i))->
                connect(ptrNeighbor->getInterface(SCLattice::Direction(
                                                      lattice->getOppositeDirection(i))));

#ifdef DEBUG_NEIGHBORHOOD
            OUTPUT << "connection #" << (ptrBlock)->blockId <<
                " to #" << ptrNeighbor->blockId << endl;
#endif
        } else {
            (ptrBlock)->getInterface(SCLattice::Direction(i))->connect(NULL);
        }
    }
}

void BlinkyBlocksWorld::glDraw() {
    glPushMatrix();
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
    glDisable(GL_TEXTURE_2D);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((BlinkyBlocksGlBlock*)pair.second)->glDraw(objBlock);
        isBlinkingBlocks |= ((BlinkyBlocksGlBlock*)pair.second)->isHighlighted;
    }
    unlock();
    glPopMatrix();

    BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
    if (bb) bb->blockCode->onGlDraw();

    lattice->glDraw();
}

void BlinkyBlocksWorld::glDrawShadows() {
    glPushMatrix();
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
    glDisable(GL_TEXTURE_2D);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((BlinkyBlocksGlBlock*)pair.second)->glDraw(objBlock);
    }
    unlock();
    glPopMatrix();

    BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
    if (bb) bb->blockCode->onGlDraw();

    lattice->glDraw();
    glPopMatrix();
}

void BlinkyBlocksWorld::glDrawId() {
    glPushMatrix();
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0);
    glDisable(GL_TEXTURE_2D);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((BlinkyBlocksGlBlock*)pair.second)->glDrawId(objBlock, pair.first);
    }
    unlock();
    glPopMatrix();
}

void BlinkyBlocksWorld::glDrawIdByMaterial() {
    glPushMatrix();
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0);

    glDisable(GL_TEXTURE_2D);
    int n;
    lock();
    for (const auto& pair : mapGlBlocks) {
        n = pair.first * numPickingTextures;
        ((BlinkyBlocksGlBlock*)pair.second)->glDrawIdByMaterial(objBlockForPicking,n);
    }
    unlock();
    glPopMatrix();
}

void BlinkyBlocksWorld::glDrawBackground() {
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
    glScalef(lattice->gridSize[0]*lattice->gridScale[0],
             lattice->gridSize[1]*lattice->gridScale[1],
             lattice->gridSize[2]*lattice->gridScale[2]);
    glBegin(GL_QUADS);
    // bottom
    glNormal3f(0,0,1.0f);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(lattice->gridSize[0]/4.0f,0);
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(lattice->gridSize[0]/4.0f,lattice->gridSize[1]/4.0f);
    glVertex3f(1.0,1.0,0.0f);
    glTexCoord2f(0,lattice->gridSize[1]/4.0f);
    glVertex3f(0.0,1.0,0.0f);
    // top
    glNormal3f(0,0,-1.0f);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,1.0f);
    glTexCoord2f(0,lattice->gridSize[1]/4.0f);
    glVertex3f(0.0,1.0,1.0f);
    glTexCoord2f(lattice->gridSize[0]/4.0f,lattice->gridSize[1]/4.0f);
    glVertex3f(1.0,1.0,1.0f);
    glTexCoord2f(lattice->gridSize[0]/4.0f,0);
    glVertex3f(1.0f,0.0f,1.0f);
    // left
    glNormal3f(1.0,0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(lattice->gridSize[1]/4.0f,0);
    glVertex3f(0.0f,1.0f,0.0f);
    glTexCoord2f(lattice->gridSize[1]/4.0f,lattice->gridSize[2]/4.0f);
    glVertex3f(0.0,1.0,1.0f);
    glTexCoord2f(0,lattice->gridSize[2]/4.0f);
    glVertex3f(0.0,0.0,1.0f);
    // right
    glNormal3f(-1.0,0,0);
    glTexCoord2f(0,0);
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(0,lattice->gridSize[2]/4.0f);
    glVertex3f(1.0,0.0,1.0f);
    glTexCoord2f(lattice->gridSize[1]/4.0f,lattice->gridSize[2]/4.0f);
    glVertex3f(1.0,1.0,1.0f);
    glTexCoord2f(lattice->gridSize[1]/4.0f,0);
    glVertex3f(1.0f,1.0f,0.0f);
    // back
    glNormal3f(0,-1.0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,1.0f,0.0f);
    glTexCoord2f(lattice->gridSize[0]/4.0f,0);
    glVertex3f(1.0f,1.0f,0.0f);
    glTexCoord2f(lattice->gridSize[0]/4.0f,lattice->gridSize[2]/4.0f);
    glVertex3f(1.0f,1.0,1.0f);
    glTexCoord2f(0,lattice->gridSize[2]/4.0f);
    glVertex3f(0.0,1.0,1.0f);
    // front
    glNormal3f(0,1.0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(0,lattice->gridSize[2]/4.0f);
    glVertex3f(0.0,0.0,1.0f);
    glTexCoord2f(lattice->gridSize[0]/4.0f,lattice->gridSize[2]/4.0f);
    glVertex3f(1.0f,0.0,1.0f);
    glTexCoord2f(lattice->gridSize[0]/4.0f,0);
    glVertex3f(1.0f,0.0f,0.0f);
    glEnd();
    glPopMatrix();
    // draw the axes
    objRepere->glDraw();
}

void BlinkyBlocksWorld::loadTextures(const string &str) {
    string path = str+"/texture_plane.tga";
    int lx,ly;
    idTextureWall = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void BlinkyBlocksWorld::setSelectedFace(int n) {
    numSelectedGlBlock=n/numPickingTextures;
    string name = objBlockForPicking->getObjMtlName(n%numPickingTextures);

    if (name=="_blinkyBlockPickingface_top") numSelectedFace=SCLattice::Top;
    else if (name=="_blinkyBlockPickingface_bottom") numSelectedFace=SCLattice::Bottom;
    else if (name=="_blinkyBlockPickingface_right") numSelectedFace=SCLattice::Right;
    else if (name=="_blinkyBlockPickingface_left") numSelectedFace=SCLattice::Left;
    else if (name=="_blinkyBlockPickingface_front") numSelectedFace=SCLattice::Front;
    else if (name=="_blinkyBlockPickingface_back") numSelectedFace=SCLattice::Back;
    else {
        cerr << "warning: Unrecognized picking face" << endl;
        numSelectedFace = 7;	// UNDEFINED
    }
}

/**
 * @brief Schedules an accel change event for block with id bId, at time date,
 *  and with the coordinates (x,y,z).
 *
 * @param date : the date at which the tap event must be consumed
 * @param id : the id of the target block
 * @param x : x coordinate of accelerometer change
 * @param y : y coordinate of accelerometer change
 * @param z : z coordinate of accelerometer change
 */
void BlinkyBlocksWorld::accelBlock(Time date, bID id, int x, int y, int z) {
    BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*)getBlockById(id);
    bb->accel(date, x,y,z);
}

/**
 * @brief Schedules an accel change event for block with id id, at time date,
 *  and with force f.
 *
 * @param date : the date at which the tap event must be consumed
 * @param id : the id of the target block
 * @param f : force of the shake
 */
void BlinkyBlocksWorld::shakeBlock(Time date, bID id, int f) {
    BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*)getBlockById(id);
    bb->shake(date, f);
}

/**
 * @brief Schedules the stopping of block with id id at a given date
 *
 * @param date : the date at which the tap event must be consumed
 * @param id : the id of the target block
 */
void BlinkyBlocksWorld::stopBlock(Time date, bID id) {
    if (id == 0) {
        // Delete the block	without deleting the links
        map<bID, BaseSimulator::BuildingBlock*>::iterator it;
        for(it = buildingBlocksMap.begin();
            it != buildingBlocksMap.end(); it++) {
            BlinkyBlocksBlock* bb = (BlinkyBlocksBlock*) it->second;
            if (bb->getState() >= BlinkyBlocksBlock::ALIVE )
                bb->stop(date, BlinkyBlocksBlock::STOPPED);
        }
    } else {
        // Delete all the links and then the block
        BlinkyBlocksBlock *bb = (BlinkyBlocksBlock *)getBlockById(id);
        if(bb->getState() >= BlinkyBlocksBlock::ALIVE) {
            // cut links between bb and others
            disconnectBlock(bb, false);
            bb->stop(date, BlinkyBlocksBlock::STOPPED); // schedule stop event, set STOPPED state
            linkNeighbors(bb->position);
        }
    }
}

void BlinkyBlocksWorld::exportConfiguration() {
    BlinkyBlocksConfigExporter exporter = BlinkyBlocksConfigExporter(this);
    exporter.exportConfiguration();
}

/**
 * @brief Dumps the content of the world (all blocks) to *stdout*
 *
 */
void BlinkyBlocksWorld::dump() {
    map<bID, BaseSimulator::BuildingBlock*>::iterator it;
    cout << "World:" << endl;
    for(it = buildingBlocksMap.begin();
        it != buildingBlocksMap.end(); it++) {
        BlinkyBlocksBlock* bb = (BlinkyBlocksBlock*) it->second;
        cout << *bb << endl;
    }
}

} // BlinkyBlock namespace
