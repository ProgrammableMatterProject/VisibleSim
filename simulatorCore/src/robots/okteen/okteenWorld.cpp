/*!
 * \file OkteenWorld.cpp
 * \brief Okteen world
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#include <iostream>
#include <string>
#include <cstdlib>
#include <sys/wait.h>
#include <sys/types.h>
#include <csignal>

#include "okteenWorld.h"
#include "okteenBlock.h"
#include "../../utils/trace.h"
#include "../../utils/configExporter.h"
#include "../../replay/replayExporter.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace Okteen
namespace Okteen {

/**
   \brief Constructor
   \param gridSize : size of the grid
   \param gridScale : size of a block
   \param argc : number of execution parameters
   \param argv : string array of parameters
*/
OkteenWorld::OkteenWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                             int argc, char *argv[]):World(argc, argv) {
    OUTPUT << TermColor::LifecycleColor << "OkteenWorld constructor" << TermColor::Reset << endl;

    if (GlutContext::GUIisEnabled) {
        objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/OkteenTextures","OkteenModule.obj");
        objConnector = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/OkteenTextures","OkteenConnector.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/OkteenTextures","OkteenModule.obj");
        objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures","repere25.obj");
    }

    lattice = new SCLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
}

OkteenWorld::~OkteenWorld() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "OkteenWorld destructor" << endl;
#endif
    /*	block linked are deleted by world::~world() */
}

void OkteenWorld::deleteWorld() {
    delete((OkteenWorld*)world);
}

void OkteenWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                             short orientation, bool master) {
    if (blockId > maxBlockId)
        maxBlockId = blockId;
    else if (blockId == 0)
        blockId = incrementBlockId();

    OkteenBlock *module = new OkteenBlock(blockId,bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                             (module->blockId, (BaseSimulator::BuildingBlock*)module));

    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), module));

    OkteenGlBlock *glBlock = new OkteenGlBlock(blockId);
    mapGlBlocks.insert(make_pair(blockId, glBlock));
    module->setGlBlock(glBlock);
    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeAddModule(getScheduler()->now(), blockId);
    module->setColor(col);
    module->setPosition(pos);
    lattice->insert(module, pos);
    glBlock->setPosition(lattice->gridToWorldPosition(pos));
    OUTPUT << "ADD #" << blockId << ", "<< pos << endl;
}

/**
 * \brief Connect the block placed on the cell at position pos
 */

 /* revoir avec robotblock*/
void OkteenWorld::linkBlock(const Cell3DPosition& pos) {
    OkteenBlock *module = (OkteenBlock *)lattice->getBlock(pos);
    OkteenBlock* neighborBlock;
    vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
    Cell3DPosition nPos;

    OUTPUT << "pos:" << pos << "  #" << module->blockId << endl;

    // Check neighbors for each interface
    for (int i = 0; i < 6; i++) {
        nPos = pos + nRelCells[i];
        OUTPUT << "npos:" << nPos << "  i=" << i << endl;
        neighborBlock = (OkteenBlock*)lattice->getBlock(nPos);
        if (neighborBlock) {
            module->getInterface(SCLattice::Direction(i))->connect(neighborBlock->getInterface(SCLattice::Direction(lattice->getOppositeDirection(i))));

#ifdef DEBUG_NEIGHBORHOOD
            OUTPUT << "connection #" << module->blockId << ":" << lattice->getDirectionString(i) <<
                " to #" << neighborBlock->blockId << ":"
                   << lattice->getDirectionString(lattice->getOppositeDirection(i)) << endl;
#endif

            updateGlData(module,i,1.0);
        } else {
            module->getInterface(SCLattice::Direction(i))->connect(NULL);
            updateGlData(module,i,0.0);
        }
    }
}

/**
 * \brief Draw modules and axes
 */
void OkteenWorld::glDraw() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
// draw modules
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((OkteenGlBlock*)pair.second)->glDraw(objBlock);
        ((OkteenGlBlock*)pair.second)->glDrawConnectors(objConnector);
    }
    unlock();
    glPopMatrix();

    BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
    if (bb) bb->blockCode->onGlDraw();

// material for the grid walls
    static const GLfloat white[]={0.8f,0.8f,0.8f,1.0f},
        gray[]={0.2f,0.2f,0.2f,1.0f};
        glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
        glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
        glMaterialfv(GL_FRONT,GL_SPECULAR,white);
        glMaterialf(GL_FRONT,GL_SHININESS,40.0);

        lattice->glDraw();

        glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
        glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
        glMaterialfv(GL_FRONT,GL_SPECULAR,white);
        glMaterialf(GL_FRONT,GL_SHININESS,40.0);

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
        glTexCoord2f(lattice->gridSize[0],0);
        glVertex3f(1.0f,0.0f,0.0f);
        glTexCoord2f(lattice->gridSize[0],lattice->gridSize[1]);
        glVertex3f(1.0,1.0,0.0f);
        glTexCoord2f(0,lattice->gridSize[1]);
        glVertex3f(0.0,1.0,0.0f);
        // top
        glNormal3f(0,0,-1.0f);
        glTexCoord2f(0,0);
        glVertex3f(0.0f,0.0f,1.0f);
        glTexCoord2f(0,lattice->gridSize[1]);
        glVertex3f(0.0,1.0,1.0f);
        glTexCoord2f(lattice->gridSize[0],lattice->gridSize[1]);
        glVertex3f(1.0,1.0,1.0f);
        glTexCoord2f(lattice->gridSize[0],0);
        glVertex3f(1.0f,0.0f,1.0f);
        // left
        glNormal3f(1.0,0,0);
        glTexCoord2f(0,0);
        glVertex3f(0.0f,0.0f,0.0f);
        glTexCoord2f(lattice->gridSize[1],0);
        glVertex3f(0.0f,1.0f,0.0f);
        glTexCoord2f(lattice->gridSize[1],lattice->gridSize[2]);
        glVertex3f(0.0,1.0,1.0f);
        glTexCoord2f(0,lattice->gridSize[2]);
        glVertex3f(0.0,0.0,1.0f);
        // right
        glNormal3f(-1.0,0,0);
        glTexCoord2f(0,0);
        glVertex3f(1.0f,0.0f,0.0f);
        glTexCoord2f(0,lattice->gridSize[2]);
        glVertex3f(1.0,0.0,1.0f);
        glTexCoord2f(lattice->gridSize[1],lattice->gridSize[2]);
        glVertex3f(1.0,1.0,1.0f);
        glTexCoord2f(lattice->gridSize[1],0);
        glVertex3f(1.0f,1.0f,0.0f);
        // back
        glNormal3f(0,-1.0,0);
        glTexCoord2f(0,0);
        glVertex3f(0.0f,1.0f,0.0f);
        glTexCoord2f(lattice->gridSize[0],0);
        glVertex3f(1.0f,1.0f,0.0f);
        glTexCoord2f(lattice->gridSize[0],lattice->gridSize[2]);
        glVertex3f(1.0f,1.0,1.0f);
        glTexCoord2f(0,lattice->gridSize[2]);
        glVertex3f(0.0,1.0,1.0f);
        // front
        glNormal3f(0,1.0,0);
        glTexCoord2f(0,0);
        glVertex3f(0.0f,0.0f,0.0f);
        glTexCoord2f(0,lattice->gridSize[2]);
        glVertex3f(0.0,0.0,1.0f);
        glTexCoord2f(lattice->gridSize[0],lattice->gridSize[2]);
        glVertex3f(1.0f,0.0,1.0f);
        glTexCoord2f(lattice->gridSize[0],0);
        glVertex3f(1.0f,0.0f,0.0f);
        glEnd();
        glPopMatrix();
        // draw the axes
        glPushMatrix();
        glScalef(0.2f,0.2f,0.2f);
        objRepere->glDraw();
        glPopMatrix();
}

void OkteenWorld::glDrawId() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
    lock();
    for (const auto& pair : mapGlBlocks) {
        // FIXME: Check that this is working properly after glBlock hash map container change
        ((OkteenGlBlock*)pair.second)->glDrawIdConnectors(objConnector, pair.first);
        ((OkteenGlBlock*)pair.second)->glDrawId(objBlock, pair.first);
    }
    unlock();
    glPopMatrix();
}

void OkteenWorld::glDrawIdByMaterial() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
    int n,m;
    lock();
    // 6 objects per module
    for (const auto& pair : mapGlBlocks) {
        // FIXME: Check that this is working properly after glBlock hash map container change
        m=0;
        n = pair.first * numPickingTextures;
        ((OkteenGlBlock*)pair.second)->glDrawId(objBlockForPicking,m); // structure
        ((OkteenGlBlock*)pair.second)->glDrawIdByMaterial(objConnector,n); // connectors
    }
    unlock();
    glPopMatrix();
}


void OkteenWorld::loadTextures(const string &str) {
    string path = str+"/texture_plane.tga";
    int lx,ly;
    idTextureWall = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void OkteenWorld::updateGlData(BuildingBlock *bb) {
    OkteenGlBlock *glblc = (OkteenGlBlock*)bb->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setPosition(lattice->gridToWorldPosition(bb->position));
        glblc->setColor(bb->color);
        unlock();
    }
}

void OkteenWorld::updateGlData(OkteenBlock*blc, const Color &color) {
    OkteenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setColor(color);
        unlock();
    }
}

void OkteenWorld::updateGlData(OkteenBlock*blc, bool visible) {
    OkteenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setVisible(visible);
        unlock();
    }
}

void OkteenWorld::updateGlData(OkteenBlock*blc, const Vector3D &position) {
    OkteenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setPosition(position);
        unlock();
    }
}

void OkteenWorld::updateGlData(OkteenBlock*blc, const Cell3DPosition &position) {
    OkteenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setPosition(lattice->gridToWorldPosition(position));
        unlock();
    }
}

void OkteenWorld::updateGlData(OkteenBlock*blc, const Matrix &mat) {
    OkteenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        glblc->mat = mat;
        unlock();
    }
}

void OkteenWorld::updateGlData(OkteenBlock*blc, short id, float length) {
    OkteenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        glblc->tabPosConnectors[id] = (uint8_t)(length*255.0);
        //OUTPUT << "#" << blc->blockId << ":" << id << "=" << (int)glblc->tabPosConnectors[id] << endl;
        unlock();
    }
}

void OkteenWorld::setSelectedFace(int n) {
    numSelectedGlBlock=n/numPickingTextures;
    numSelectedFace = n%numPickingTextures;
}

void OkteenWorld::exportConfiguration() {
    OkteenConfigExporter exporter = OkteenConfigExporter(this);
    exporter.exportConfiguration();
}

void OkteenWorld::disconnectBlock(BuildingBlock *block, bool count) {
    P2PNetworkInterface *fromBlock,*toBlock;

    for(int i = 0; i < block->getNbInterfaces(); i++) {
        fromBlock = block->getInterface(i);
        if (fromBlock && fromBlock->connectedInterface) {
            toBlock = fromBlock->connectedInterface;

            // Clear message queue
            fromBlock->outgoingQueue.clear();
            toBlock->outgoingQueue.clear();

            // Notify respective codeBlocks
            block->removeNeighbor(fromBlock);
            fromBlock->connectedInterface->hostBlock->removeNeighbor(fromBlock->connectedInterface);

            // Disconnect the interfaces
            fromBlock->connectedInterface = NULL;
            toBlock->connectedInterface = NULL;
        }
    }

    lattice->remove(block->position, count);

    OUTPUT << getScheduler()->now() << " : Disconnect Block " << block->blockId <<
        " pos = " << block->position << endl;
}



} // Okteen namespace
