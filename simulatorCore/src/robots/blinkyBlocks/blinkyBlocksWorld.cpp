/*
 * blinkyBlockWorld.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <string>
#include <stdlib.h>
#include <signal.h>

#include "blinkyBlocksWorld.h"
#include "blinkyBlocksBlock.h"
#include "blinkyBlocksEvents.h"
#include "../../utils/configExporter.h"
#include "../../utils/trace.h"
#include "../../replay/replayExporter.h"

using namespace std;

namespace BlinkyBlocks {

#ifdef WIN32
    string directory = string(ROOT_DIR) + "/simulatorCore/resources/textures/blinkyBlocksTextures";
#else
    string directory = "../../simulatorCore/resources/textures/blinkyBlocksTextures";
#endif

    BlinkyBlocksWorld::BlinkyBlocksWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                         int argc, char *argv[]):World(argc, argv) {
        OUTPUT << TermColor::LifecycleColor << "BlinkyBlocksWorld constructor" << TermColor::Reset << endl;

        if (GlutContext::GUIisEnabled) {
            objBlock = new ObjLoader::ObjLoader(directory.c_str(),"blinkyBlock_texture.obj");
            //objBlock = new ObjLoader::ObjLoader(directory.c_str(),"blinkyBlockCentered.obj");
            objBlockForPicking = new ObjLoader::ObjLoader(directory.c_str(),"blinkyBlockPickingCentered.obj");
#ifdef WIN32
            directory = string(ROOT_DIR) + "/simulatorCore/resources/textures/latticeTextures";
#else
            directory = "../../simulatorCore/resources/textures/latticeTextures";
#endif
            objRepere = new ObjLoader::ObjLoader(directory.c_str(),"repere25.obj");
        }
        lattice = new SCLattice(gridSize, gridScale.isZero() ? defaultBlockSize : gridScale);
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
                                     uint8_t orient) {
        //cout << "orientation in addBlock(" << to_string(blockId) << ") =" << to_string(orient) << endl;
        if (blockId > maxBlockId) maxBlockId = blockId;
        else if (blockId == 0) blockId = incrementBlockId();

        BlinkyBlocksBlock *blinkyBlock = new BlinkyBlocksBlock(blockId, bcb);
        buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                                         (blinkyBlock->blockId, (BaseSimulator::BuildingBlock*)blinkyBlock));
        getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), blinkyBlock));

        BlinkyBlocksGlBlock *glBlock = new BlinkyBlocksGlBlock(blockId);
        glBlock->setRotation(orient);
        mapGlBlocks.insert(make_pair(blockId, glBlock));
        blinkyBlock->setGlBlock(glBlock);

        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writeAddModule(getScheduler()->now(), blockId);
        blinkyBlock->setPosition(pos);
        blinkyBlock->setColor(col);
        blinkyBlock->orientationCode=orient;

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
        Cell3DPosition nPos;

        // Check neighbors for each interface
        for (short i = 0; i < SCLattice::Direction::MAX_NB_NEIGHBORS; i++) {
            nPos = ptrBlock->position + ptrBlock->getRelativePosition(i);
            ptrNeighbor = (BlinkyBlocksBlock*)lattice->getBlock(nPos);
            if (ptrNeighbor) {
                P2PNetworkInterface *p2pNeighbor = ptrNeighbor->getInterfaceToNeighborPos(pos);
                (ptrBlock)->getInterface(i)->connect(p2pNeighbor);
#ifdef DEBUG_NEIGHBORHOOD
                OUTPUT << "connection #" << (ptrBlock)->blockId <<
                " to #" << ptrNeighbor->blockId << endl;
#endif
            } else {
                (ptrBlock)->getInterface(i)->connect(nullptr);
            }
        }
    }

    void BlinkyBlocksWorld::glDraw() {
        glPushMatrix();
        glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
        lock();
        for (const auto& pair : mapGlBlocks) {
            ((BlinkyBlocksGlBlock*)pair.second)->glDraw(objBlock);
            isBlinkingBlocks |= ((BlinkyBlocksGlBlock*)pair.second)->isHighlighted();
        }
        unlock();
        glPopMatrix();

        BuildingBlock *bb = getSelectedBuildingBlock();
        if (bb==nullptr && !getMap().empty()) bb=getMap().begin()->second;
        if (bb!= nullptr) bb->blockCode->onGlDraw();
        lattice->glDraw();
        if (GlutContext::editMode && bb) {
            Cell3DPosition nPos;
            if (bb->getNeighborPos(numSelectedFace, nPos)) {
                static const GLfloat transpRed[4] = {255.0, 0, 0, 0.5};
                auto pos = lattice->gridToWorldPosition(nPos);
                glPushMatrix();
                glTranslated(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
                glTranslated(pos[0],pos[1],pos[2]);
                objBlock->setLightedColor(transpRed);
                objBlock->glDraw();
                glPopMatrix();
            } else {
                cerr << "Position out of the grid" << endl;
            }
        }
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

        BuildingBlock *bb = getSelectedBuildingBlock() ?getSelectedBuildingBlock(): getMap().begin()->second;
        if (bb) bb->blockCode->onGlDraw();

        lattice->glDraw();
        glPopMatrix();
    }

    void BlinkyBlocksWorld::glDrawId() {
        glPushMatrix();
        glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
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
        glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);

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

   /* void BlinkyBlocksWorld::updateGlData(BuildingBlock *bb,short angleCode) {
        auto *glblc = (BlinkyBlocksGlBlock*)bb->getGlBlock();
        if (glblc) {
            lock();
            glblc->setRotation(angleCode);
            unlock();
        }
    }
*/

    void BlinkyBlocksWorld::loadTextures(const string &str) {
        string path = str+"/texture_plane.tga";
        int lx,ly;
        idTextureWall = loadTexture(path.c_str(),lx,ly);
    }

    void BlinkyBlocksWorld::setSelectedFace(int n) {
        numSelectedGlBlock=n/numPickingTextures;
        string name = objBlockForPicking->getObjMtlName(n%numPickingTextures);

        if (name=="_blinkyBlockPickingface_top") numSelectedFace=SCLattice::Direction::Top;
        else if (name=="_blinkyBlockPickingface_bottom") numSelectedFace=SCLattice::Direction::Bottom;
        else if (name=="_blinkyBlockPickingface_right") numSelectedFace=SCLattice::Direction::South;
        else if (name=="_blinkyBlockPickingface_left") numSelectedFace=SCLattice::Direction::North;
        else if (name=="_blinkyBlockPickingface_front") numSelectedFace=SCLattice::Direction::West;
        else if (name=="_blinkyBlockPickingface_back") numSelectedFace=SCLattice::Direction::East;
        else {
            cerr << "warning: Unrecognized picking face" << endl;
            numSelectedFace = SCLattice::Direction::MAX_NB_NEIGHBORS;	// UNDEFINED
        }
        cout << name << "->" << int(numSelectedFace) << endl;
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
