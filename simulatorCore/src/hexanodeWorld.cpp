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

#include "hexanodeWorld.h"
#include "hexanodeBlock.h"
#include "hexanodeMotionEngine.h"
#include "trace.h"
#include "configExporter.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace Hexanode
namespace Hexanode {

/**
   \brief Constructor
   \param gridSize : size of the grid
   \param gridScale : size of a block
   \param argc : number of execution parameters
   \param argv : string array of parameters
*/
HexanodeWorld::HexanodeWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                             int argc, char *argv[]):World(argc, argv) {
    OUTPUT << TermColor::LifecycleColor << "HexanodeWorld constructor" << TermColor::Reset << endl;

    if (GlutContext::GUIisEnabled) {
			objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/hexanodeTextures","hexanode.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/hexanodeTextures","hexanode_picking.obj");
        //objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures","repere25.obj");
    }
    lattice = new HHLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
		nodeMotionEngine = new HexanodeMotionEngine();
}

HexanodeWorld::~HexanodeWorld() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "HexanodeWorld destructor" << endl;
#endif
    /*	block linked are deleted by world::~world() */
		delete nodeMotionEngine;
}

void HexanodeWorld::deleteWorld() {
    delete((HexanodeWorld*)world);
}

void HexanodeWorld::createPopupMenu(int ix, int iy) {
	if (!GlutContext::popupMenu) {
		GlutContext::popupMenu = new GlutPopupMenuWindow(NULL,0,0,202,215);
		// create submenu "Add"
		GlutPopupMenuWindow *addBlockSubMenu = new GlutPopupMenuWindow(NULL,0,0,202,112);
		addBlockSubMenu->id=50;
		addBlockSubMenu->addButton(11,"../../simulatorCore/resources/textures/menuTextures/menu_add_normal.tga");
		addBlockSubMenu->addButton(12,"../../simulatorCore/resources/textures/menuTextures/menu_add_same.tga");
		addBlockSubMenu->addButton(13,"../../simulatorCore/resources/textures/menuTextures/menu_add_random.tga");
		// create submenu "Rotate"
		GlutPopupMenuWindow *rotateBlockSubMenu = new GlutPopupMenuWindow(NULL,0,0,116,40);
		rotateBlockSubMenu->id=51;
		
		GlutContext::popupMenu->addButton(1,"../../simulatorCore/resources/textures/menuTextures/menu_add_sub.tga",addBlockSubMenu);
		GlutContext::popupMenu->addButton(2,"../../simulatorCore/resources/textures/menuTextures/menu_del.tga");
		GlutContext::popupMenu->addButton(6,"../../simulatorCore/resources/textures/menuTextures/menu_rotate_sub.tga",rotateBlockSubMenu);
		GlutContext::popupMenu->addButton(3,"../../simulatorCore/resources/textures/menuTextures/menu_tap.tga");
		GlutContext::popupMenu->addButton(4,"../../simulatorCore/resources/textures/menuTextures/menu_save.tga");
		GlutContext::popupMenu->addButton(5,"../../simulatorCore/resources/textures/menuTextures/menu_cancel.tga");
	}
	
	// update rotateSubMenu depending on rotation/translation HexanodeCapabilities
	HexanodeBlock *bb = (HexanodeBlock *)getSelectedBuildingBlock();
	vector<HexanodeMotion*> tab = getAllMotionsForModule(bb);
	int nbreMenus=tab.size();
	if (nbreMenus==0) {
		((GlutButton*)GlutContext::popupMenu->getButton(6))->activate(false);
	} else {
		((GlutButton*)GlutContext::popupMenu->getButton(6))->activate(true);
		GlutPopupMenuWindow *rotateBlockSubMenu = (GlutPopupMenuWindow*)GlutContext::popupMenu->getButton(6)->getChild(0);
		rotateBlockSubMenu->h = nbreMenus*35+10;
		rotateBlockSubMenu->clearChildren();
		int i=100;
		short orient;
		Cell3DPosition finalPos;
		for(auto &motion:tab) {
			orient=motion->isRotation?(bb->orientationCode+(motion->direction==CW?1:5))%6:bb->orientationCode;
			finalPos = motion->finalPos + bb->position;
			cout << finalPos << "," << orient << endl;
			rotateBlockSubMenu->addButton(new GlutRotation2DButton(NULL,i++,0,0,0,0,"../../simulatorCore/resources/textures/menuTextures/menu_link_node.tga", motion->isRotation,motion->fromConId,motion->direction+4,finalPos,orient,0.083333333));
		}
	}
	
	if (iy < GlutContext::popupMenu->h) iy = GlutContext::popupMenu->h;
	cerr << "Block " << numSelectedGlBlock << ":" << lattice->getDirectionString(numSelectedFace) << " selected" << endl;
	GlutContext::popupMenu->activate(1, canAddBlockToFace((int)numSelectedGlBlock, (int)numSelectedFace));
	GlutContext::popupMenu->setCenterPosition(ix,GlutContext::screenHeight-iy);
	GlutContext::popupMenu->show(true);
	if (GlutContext::popupSubMenu) GlutContext::popupSubMenu->show(false);
}

void HexanodeWorld::menuChoice(int n) {
	HexanodeBlock *bb = (HexanodeBlock *)getSelectedBuildingBlock();
	Cell3DPosition nPos;
	switch (n) {
		case 1: case 6:
			GlutContext::popupMenu->show(true);
			GlutContext::popupSubMenu = (GlutPopupMenuWindow*)GlutContext::popupMenu->getButton(n)->getChild(0);
			GlutContext::popupSubMenu->show(true);
			GlutContext::popupSubMenu->x=GlutContext::popupMenu->x+GlutContext::popupMenu->w+5;
			GlutContext::popupSubMenu->y=GlutContext::popupMenu->y+GlutContext::popupMenu->getButton(n)->y-GlutContext::popupSubMenu->h/2;
			// avoid placing submenu over the top of the window
			if (GlutContext::popupSubMenu->y+GlutContext::popupSubMenu->h > GlutContext::screenHeight) {
				GlutContext::popupSubMenu->y = GlutContext::screenHeight-GlutContext::popupSubMenu->h;
			}
			break;
		case 11:
			GlutContext::popupSubMenu->show(false);
			GlutContext::popupMenu->show(false);
			if (bb->getNeighborPos(HHLattice::Direction(numSelectedFace),nPos)) {
				addBlock(0, bb->buildNewBlockCode, nPos,bb->color,0,false);
				linkBlock(nPos);
				linkNeighbors(nPos);
			} else {
				cerr << "Position out of the grid" << endl;
			}
			break;
		case 12:
			GlutContext::popupSubMenu->show(false);
			GlutContext::popupMenu->show(false);
			if (bb->getNeighborPos(HHLattice::Direction(numSelectedFace),nPos)) {
				addBlock(0, bb->buildNewBlockCode,nPos,bb->color,bb->orientationCode,false);
				linkBlock(nPos);
				linkNeighbors(nPos);
			} else {
				cerr << "Position out of the grid" << endl;
			}
			break;
		case 13:
			GlutContext::popupSubMenu->show(false);
			GlutContext::popupMenu->show(false);
			if (bb->getNeighborPos(HHLattice::Direction(numSelectedFace),nPos)) {
				int orient = rand()%24;
				addBlock(0, bb->buildNewBlockCode,nPos,bb->color,orient,false);
				linkBlock(nPos);
				linkNeighbors(nPos);
			} else {
				cerr << "Position out of the grid" << endl;
			}
			break;
		default:
			if (n>=100) {
				GlutContext::popupSubMenu->show(false);
				GlutContext::popupMenu->show(false);
				// if (getScheduler()->state == RUNNING) {
				// scheduler->schedule(new Rotation3DStartEvent(getScheduler()->now(), bb, Rotations3D r));
				// } else {
				Cell3DPosition pos = ((GlutRotationButton*)GlutContext::popupSubMenu->getButton(n))->finalPosition;
				short orient = ((GlutRotationButton*)GlutContext::popupSubMenu->getButton(n))->finalOrientation;
				HexanodeWorld *wrld = getWorld();
				wrld->disconnectBlock(bb);
				bb->setPositionAndOrientation(pos,orient);
				wrld->connectBlock(bb);
				//}
			} else World::menuChoice(n); // For all non-specific cases
			break;
	}
}


void HexanodeWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                             short orientation, bool master) {
    if (blockId > maxBlockId)
        maxBlockId = blockId;
    else if (blockId == 0)
        blockId = incrementBlockId();

    HexanodeBlock *module = new HexanodeBlock(blockId,bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                             (module->blockId, (BaseSimulator::BuildingBlock*)module));

    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), module));

    HexanodeGlBlock *glBlock = new HexanodeGlBlock(blockId);
    mapGlBlocks.insert(make_pair(blockId, glBlock));
    module->setGlBlock(glBlock);
    module->setColor(col);
		cout << "addBlock(" << pos << ")" << endl;
		module->setPositionAndOrientation(pos,orientation);
		cout << "module->position = " << module->position << endl;
		lattice->insert(module, pos);
    glBlock->setPosition(lattice->gridToWorldPosition(pos));
    linkBlock(pos);
}

/**
 * \brief Connect the block placed on the cell at position pos
 */

 /* revoir avec robotblock*/
void HexanodeWorld::linkBlock(const Cell3DPosition& pos) {
    HexanodeBlock *module = (HexanodeBlock *)lattice->getBlock(pos);
    HexanodeBlock* neighborBlock;
    vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
    Cell3DPosition nPos;

    // Check neighbors for each interface
    for (int i = 0; i < HHLattice::MAX_NB_NEIGHBORS; i++) {
        nPos = pos + nRelCells[i];
        neighborBlock = (HexanodeBlock*)lattice->getBlock(nPos);
        if (neighborBlock) {
            module->getInterface(HHLattice::Direction(i))->connect(neighborBlock->getInterface(HHLattice::Direction(lattice->getOppositeDirection(i))));

#ifdef DEBUG_NEIGHBORHOOD
            OUTPUT << "connection #" << module->blockId << ":" << lattice->getDirectionString(i) <<
                " to #" << neighborBlock->blockId << ":"
                   << lattice->getDirectionString(lattice->getOppositeDirection(i)) << endl;
#endif
        } else {
            module->getInterface(HHLattice::Direction(i))->connect(NULL);
        }
    }
}

/**
 * \brief Draw modules and axes
 */
void HexanodeWorld::glDraw() {
		glDisable(GL_TEXTURE_2D);
	lock();
	for (const auto& pair : mapGlBlocks) {
		((HexanodeGlBlock*)pair.second)->glDraw(objBlock);
	}
	unlock();
	
	glPopMatrix();
	
	BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
	if (bb) bb->blockCode->onGlDraw();
	
	glDrawBackground();
}

void HexanodeWorld::glDrawShadows() {
	glDisable(GL_TEXTURE_2D);
	lock();
	for (const auto& pair : mapGlBlocks) {
		((HexanodeGlBlock*)pair.second)->glDrawShadows(objBlockForPicking);
	}
	unlock();
	
	glPopMatrix();
}

void HexanodeWorld::glDrawId() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((HexanodeGlBlock*)pair.second)->glDrawId(objBlock, pair.first);
    }
    unlock();
    glPopMatrix();
}

void HexanodeWorld::glDrawIdByMaterial() {
	glPushMatrix();
	glDisable(GL_TEXTURE_2D);
	int m;
	lock();
	for (const auto& pair : mapGlBlocks) {
		m = pair.first*6;
		((HexanodeGlBlock*)pair.second)->glDrawIdByMaterial(objBlockForPicking,m);
	}
	unlock();
	glPopMatrix();
}

void HexanodeWorld::glDrawSpecificBg() {
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
	//glScalef(lattice->gridSize[0]*lattice->gridScale[0],lattice->gridSize[1]*lattice->gridScale[1]*M_SQRT3_2,1.0f);
	
	glBegin(GL_QUADS);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,0.0f,0.0f);
	glTexCoord2f(0.5*lattice->gridSize[0],0);
	glVertex3f(lattice->gridSize[0]*lattice->gridScale[0],0.0f,0.0f);
	glTexCoord2f(0.5*(lattice->gridSize[0]+0.5*lattice->gridSize[1]),0.5*lattice->gridSize[1]);
	glVertex3f((lattice->gridSize[0]+0.5*lattice->gridSize[1])*lattice->gridScale[0],lattice->gridSize[1]*lattice->gridScale[1]*M_SQRT3_2,0.0f);
	glTexCoord2f(0.25*lattice->gridSize[1],0.5*lattice->gridSize[1]);
	glVertex3f(0.5*lattice->gridSize[1]*lattice->gridScale[0],lattice->gridSize[1]*lattice->gridScale[1]*M_SQRT3_2,0.0f);
	glEnd();
	glPopMatrix();
	// draw the axes
	//objRepere->glDraw();
	
	glPushMatrix();
}

void HexanodeWorld::loadTextures(const string &str) {
	string path = str+"/hexanodegrid.tga";
	int lx,ly;
	idTextureWall = GlutWindow::loadTexture(path.c_str(),lx,ly);
	path=str+"/../smartBlocksTextures/digits.tga";
	idTextureDigits = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void HexanodeWorld::updateGlData(BuildingBlock *bb) {
	HexanodeGlBlock *glblc = (HexanodeGlBlock*)bb->getGlBlock();
	if (glblc) {
			lock();
			//cout << "update pos:" << position << endl;
			glblc->setPosition(lattice->gridToWorldPosition(bb->position));
			glblc->setColor(bb->color);
			unlock();
	}
}

void HexanodeWorld::updateGlData(HexanodeBlock*blc, const Color &color) {
	HexanodeGlBlock *glblc = blc->getGlBlock();
	if (glblc) {
			lock();
			//cout << "update pos:" << position << endl;
			glblc->setColor(color);
			unlock();
	}
}

void HexanodeWorld::updateGlData(HexanodeBlock*blc, bool visible) {
	HexanodeGlBlock *glblc = blc->getGlBlock();
	if (glblc) {
			lock();
			//cout << "update pos:" << position << endl;
			glblc->setVisible(visible);
			unlock();
	}
}

void HexanodeWorld::updateGlData(HexanodeBlock*blc, const Vector3D &position) {
    HexanodeGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setPosition(position);
        unlock();
    }
}

void HexanodeWorld::updateGlData(HexanodeBlock*blc, const Cell3DPosition &position) {
    HexanodeGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setPosition(lattice->gridToWorldPosition(position));
        unlock();
    }
}

void HexanodeWorld::updateGlData(HexanodeBlock*blc, const Matrix &mat) {
    HexanodeGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        glblc->mat = mat;
        unlock();
    }
}

void HexanodeWorld::setSelectedFace(int n) {
	cout << "SelectedFace("<< n << ")" <<endl;
	
    numSelectedGlBlock = n / 6;
		string name = objBlockForPicking->getObjMtlName(n%6);
	
	if (name == "NORTH_EAST") numSelectedFace = HHLattice::Direction::NORTH_EAST;
	else if (name == "EAST") numSelectedFace = HHLattice::Direction::EAST;
	else if (name == "SOUTH_EAST") numSelectedFace = HHLattice::Direction::SOUTH_EAST;
	else if (name == "SOUTH_WEST") numSelectedFace = HHLattice::Direction::SOUTH_WEST;
	else if (name == "WEST") numSelectedFace = HHLattice::Direction::WEST;
	else if (name == "NORTH_WEST") numSelectedFace = HHLattice::Direction::NORTH_WEST;
	else {
		cerr << "warning: Unrecognized picking face" << endl;
		numSelectedFace = HHLattice::Direction::MAX_NB_NEIGHBORS; // UNDEFINED
	}
		cerr << name << " => " << numSelectedFace << endl;
}

void HexanodeWorld::exportConfiguration() {
    HexanodeConfigExporter exporter = HexanodeConfigExporter(this);
    exporter.exportConfiguration();
}

void HexanodeWorld::disconnectBlock(BuildingBlock *block) {
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

vector<HexanodeMotion*> HexanodeWorld::getAllMotionsForModule(HexanodeBlock*nb) {
	return nodeMotionEngine->getAllMotionsForModule(nb,(HHLattice*)lattice);
}


} // Hexanode namespace
