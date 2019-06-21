/**
 * @file   nodeWorld.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:59:44 2019
 *
 * @brief
 *
 *
 */

#include <iostream>
#include <string>
#include <stdlib.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

#include "nodeWorld.h"
#include "nodeBlock.h"
#include "trace.h"
#include "configExporter.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace Node
namespace Node {

/**
   \brief Constructor
   \param gridSize : size of the grid
   \param gridScale : size of a block
   \param argc : number of execution parameters
   \param argv : string array of parameters
*/
NodeWorld::NodeWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                             int argc, char *argv[]):World(argc, argv) {
    OUTPUT << TermColor::LifecycleColor << "NodeWorld constructor" << TermColor::Reset << endl;

    if (GlutContext::GUIisEnabled) {
        objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/nodeTextures",
                                            "node.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/nodeTextures",
                                                      "node_picking.obj");
        objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures",
                                             "repere25.obj");
    }

    lattice = new SLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
}

NodeWorld::~NodeWorld() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "NodeWorld destructor" << endl;
#endif
    /*	block linked are deleted by world::~world() */
}

void NodeWorld::deleteWorld() {
    delete((NodeWorld*)world);
}

void NodeWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                             short orientation, bool master) {
    if (blockId > maxBlockId)
        maxBlockId = blockId;
    else if (blockId == 0)
        blockId = incrementBlockId();

    NodeBlock *module = new NodeBlock(blockId,bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                             (module->blockId, (BaseSimulator::BuildingBlock*)module));

    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), module));

    NodeGlBlock *glBlock = new NodeGlBlock(blockId);
    mapGlBlocks.insert(make_pair(blockId, glBlock));
    module->setGlBlock(glBlock);
    module->setColor(col);
    module->setPosition(pos);
    lattice->insert(module, pos);
    glBlock->setPosition(lattice->gridToWorldPosition(pos));
    linkBlock(pos);
}

/**
 * \brief Connect the block placed on the cell at position pos
 */

 /* revoir avec robotblock*/
void NodeWorld::linkBlock(const Cell3DPosition& pos) {
    NodeBlock *module = (NodeBlock *)lattice->getBlock(pos);
    NodeBlock* neighborBlock;
    vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
    Cell3DPosition nPos;

    // Check neighbors for each interface
    for (int i = 0; i < SLattice::MAX_NB_NEIGHBORS; i++) {
        nPos = pos + nRelCells[i];
        neighborBlock = (NodeBlock*)lattice->getBlock(nPos);
        if (neighborBlock) {
            module->getInterface(SLattice::Direction(i))->connect(neighborBlock->getInterface(SLattice::Direction(lattice->getOppositeDirection(i))));

#ifdef DEBUG_NEIGHBORHOOD
            OUTPUT << "connection #" << module->blockId << ":" << lattice->getDirectionString(i) <<
                " to #" << neighborBlock->blockId << ":"
                   << lattice->getDirectionString(lattice->getOppositeDirection(i)) << endl;
#endif

            updateGlData(module,i,1.0);
        } else {
            module->getInterface(SLattice::Direction(i))->connect(NULL);
            updateGlData(module,i,0.0);
        }
    }
}

/**
 * \brief Draw modules and axes
 */
void NodeWorld::glDraw() {
	/*glTranslatef(-lattice->gridSize[0]/2.0f*lattice->gridScale[0],
	 *          -lattice->gridSize[1]/2.0f*lattice->gridScale[1],0); */
	glDisable(GL_TEXTURE_2D);
	lock();
	for (const auto& pair : mapGlBlocks) {
		((NodeGlBlock*)pair.second)->glDraw(objBlock);
	}
	unlock();
	
	glPopMatrix();
	
	BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
	if (bb) bb->blockCode->onGlDraw();
	
	glDrawBackground();
}

void NodeWorld::glDrawShadows() {
	/*glTranslatef(-lattice->gridSize[0]/2.0f*lattice->gridScale[0],
	 *          -lattice->gridSize[1]/2.0f*lattice->gridScale[1],0); */
	glDisable(GL_TEXTURE_2D);
	lock();
	for (const auto& pair : mapGlBlocks) {
		((NodeGlBlock*)pair.second)->glDrawShadows(objBlock);
	}
	unlock();
	
	glPopMatrix();
}


// void NodeWorld::glDraw() {
//     glPushMatrix();
//     glDisable(GL_TEXTURE_2D);
//     glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
// // draw modules
//     lock();
//     for (const auto& pair : mapGlBlocks) {
//         ((NodeGlBlock*)pair.second)->glDraw(objBlock);
//     }
//     unlock();
//     glPopMatrix();
// 
//     BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
//     if (bb) bb->blockCode->onGlDraw();
// 
// // material for the grid walls
//     static const GLfloat white[]={0.8f,0.8f,0.8f,1.0f},
//         gray[]={0.2f,0.2f,0.2f,1.0f};
//         glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
//         glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
//         glMaterialfv(GL_FRONT,GL_SPECULAR,white);
//         glMaterialf(GL_FRONT,GL_SHININESS,40.0);
// 
//         lattice->glDraw();
// 
//         glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
//         glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
//         glMaterialfv(GL_FRONT,GL_SPECULAR,white);
//         glMaterialf(GL_FRONT,GL_SHININESS,40.0);
// 
//         glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
//         glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
//         glMaterialfv(GL_FRONT,GL_SPECULAR,gray);
//         glMaterialf(GL_FRONT,GL_SHININESS,40.0);
//         glPushMatrix();
//         enableTexture(true);
//         glBindTexture(GL_TEXTURE_2D,idTextureWall);
//         glScalef(lattice->gridSize[0]*lattice->gridScale[0],
//                  lattice->gridSize[1]*lattice->gridScale[1],
//                  lattice->gridSize[2]*lattice->gridScale[2]);
//         glBegin(GL_QUADS);
//         // bottom
//         glNormal3f(0,0,1.0f);
//         glTexCoord2f(0,0);
//         glVertex3f(0.0f,0.0f,0.0f);
//         glTexCoord2f(lattice->gridSize[0],0);
//         glVertex3f(1.0f,0.0f,0.0f);
//         glTexCoord2f(lattice->gridSize[0],lattice->gridSize[1]);
//         glVertex3f(1.0,1.0,0.0f);
//         glTexCoord2f(0,lattice->gridSize[1]);
//         glVertex3f(0.0,1.0,0.0f);
//         // top
//         glNormal3f(0,0,-1.0f);
//         glTexCoord2f(0,0);
//         glVertex3f(0.0f,0.0f,1.0f);
//         glTexCoord2f(0,lattice->gridSize[1]);
//         glVertex3f(0.0,1.0,1.0f);
//         glTexCoord2f(lattice->gridSize[0],lattice->gridSize[1]);
//         glVertex3f(1.0,1.0,1.0f);
//         glTexCoord2f(lattice->gridSize[0],0);
//         glVertex3f(1.0f,0.0f,1.0f);
//         // left
//         glNormal3f(1.0,0,0);
//         glTexCoord2f(0,0);
//         glVertex3f(0.0f,0.0f,0.0f);
//         glTexCoord2f(lattice->gridSize[1],0);
//         glVertex3f(0.0f,1.0f,0.0f);
//         glTexCoord2f(lattice->gridSize[1],lattice->gridSize[2]);
//         glVertex3f(0.0,1.0,1.0f);
//         glTexCoord2f(0,lattice->gridSize[2]);
//         glVertex3f(0.0,0.0,1.0f);
//         // right
//         glNormal3f(-1.0,0,0);
//         glTexCoord2f(0,0);
//         glVertex3f(1.0f,0.0f,0.0f);
//         glTexCoord2f(0,lattice->gridSize[2]);
//         glVertex3f(1.0,0.0,1.0f);
//         glTexCoord2f(lattice->gridSize[1],lattice->gridSize[2]);
//         glVertex3f(1.0,1.0,1.0f);
//         glTexCoord2f(lattice->gridSize[1],0);
//         glVertex3f(1.0f,1.0f,0.0f);
//         // back
//         glNormal3f(0,-1.0,0);
//         glTexCoord2f(0,0);
//         glVertex3f(0.0f,1.0f,0.0f);
//         glTexCoord2f(lattice->gridSize[0],0);
//         glVertex3f(1.0f,1.0f,0.0f);
//         glTexCoord2f(lattice->gridSize[0],lattice->gridSize[2]);
//         glVertex3f(1.0f,1.0,1.0f);
//         glTexCoord2f(0,lattice->gridSize[2]);
//         glVertex3f(0.0,1.0,1.0f);
//         // front
//         glNormal3f(0,1.0,0);
//         glTexCoord2f(0,0);
//         glVertex3f(0.0f,0.0f,0.0f);
//         glTexCoord2f(0,lattice->gridSize[2]);
//         glVertex3f(0.0,0.0,1.0f);
//         glTexCoord2f(lattice->gridSize[0],lattice->gridSize[2]);
//         glVertex3f(1.0f,0.0,1.0f);
//         glTexCoord2f(lattice->gridSize[0],0);
//         glVertex3f(1.0f,0.0f,0.0f);
//         glEnd();
//         glPopMatrix();
//         // draw the axes
//         glPushMatrix();
//         glScalef(0.2f,0.2f,0.2f);
//         objRepere->glDraw();
//         glPopMatrix();
// }

void NodeWorld::glDrawId() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((NodeGlBlock*)pair.second)->glDrawId(objBlock, pair.first);
    }
    unlock();
    glPopMatrix();
}

void NodeWorld::glDrawIdByMaterial() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
    int m;
    lock();
    // 6 objects per module
    for (const auto& pair : mapGlBlocks) {
        m=0;
        ((NodeGlBlock*)pair.second)->glDrawId(objBlockForPicking,m); // structure
    }
    unlock();
    glPopMatrix();
}

void NodeWorld::glDrawSpecificBg() {
	static const GLfloat white[]={1.0,1.0,1.0,1.0},
	gray[]={0.2,0.2,0.2,1.0};
	
	glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
	glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
	glMaterialfv(GL_FRONT,GL_SPECULAR,gray);
	glMaterialf(GL_FRONT,GL_SHININESS,40.0);
	glPushMatrix();
	enableTexture(true);
	glBindTexture(GL_TEXTURE_2D,idTextureWall);
	glNormal3f(0,0,1.0f);
	glScalef(lattice->gridSize[0]*lattice->gridScale[0],
					 lattice->gridSize[1]*lattice->gridScale[1],1.0f);
	glBegin(GL_QUADS);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,0.0f,0.0f);
	glTexCoord2f(lattice->gridSize[0],0);
	glVertex3f(1.0f,0.0f,0.0f);
	glTexCoord2f(lattice->gridSize[0],lattice->gridSize[1]);
	glVertex3f(1.0,1.0,0.0f);
	glTexCoord2f(0,lattice->gridSize[1]);
	glVertex3f(0.0,1.0,0.0f);
	glEnd();
	glPopMatrix();
	// draw the axes
	objRepere->glDraw();
	
	glPushMatrix();
}



void NodeWorld::loadTextures(const string &str) {
    string path = str+"/texture_plane.tga";
    int lx,ly;
    idTextureWall = GlutWindow::loadTexture(path.c_str(),lx,ly);
		cout << "idWall=" << idTextureWall << endl;
		
		path=str+"/../smartBlocksTextures/digits.tga";
		idTextureDigits = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void NodeWorld::updateGlData(BuildingBlock *bb) {
    NodeGlBlock *glblc = (NodeGlBlock*)bb->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setPosition(lattice->gridToWorldPosition(bb->position));
        glblc->setColor(bb->color);
        unlock();
    }
}

void NodeWorld::updateGlData(NodeBlock*blc, const Color &color) {
    NodeGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setColor(color);
        unlock();
    }
}

void NodeWorld::updateGlData(NodeBlock*blc, bool visible) {
    NodeGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setVisible(visible);
        unlock();
    }
}

void NodeWorld::updateGlData(NodeBlock*blc, const Vector3D &position) {
    NodeGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setPosition(position);
        unlock();
    }
}

void NodeWorld::updateGlData(NodeBlock*blc, const Cell3DPosition &position) {
    NodeGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setPosition(lattice->gridToWorldPosition(position));
        unlock();
    }
}

void NodeWorld::updateGlData(NodeBlock*blc, const Matrix &mat) {
    NodeGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        glblc->mat = mat;
        unlock();
    }
}

void NodeWorld::updateGlData(NodeBlock*blc, short id, float length) {
    NodeGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        glblc->tabPosConnectors[id] = (uint8_t)(length*255.0);
        //OUTPUT << "#" << blc->blockId << ":" << id << "=" << (int)glblc->tabPosConnectors[id] << endl;
        unlock();
    }
}

void NodeWorld::setSelectedFace(int n) {
    numSelectedGlBlock = n / SLattice::MAX_NB_NEIGHBORS;
    numSelectedFace = n % SLattice::MAX_NB_NEIGHBORS;
}

void NodeWorld::exportConfiguration() {
    NodeConfigExporter exporter = NodeConfigExporter(this);
    exporter.exportConfiguration();
}

void NodeWorld::disconnectBlock(BuildingBlock *block) {
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

    lattice->remove(block->position);
}



} // Node namespace
