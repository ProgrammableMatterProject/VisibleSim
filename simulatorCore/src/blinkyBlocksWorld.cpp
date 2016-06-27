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
#include "configExporter.h"
#include "trace.h"

using namespace std;

namespace BlinkyBlocks {

BlinkyBlocksWorld::BlinkyBlocksWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
									 int argc, char *argv[]):World(argc, argv) {
	OUTPUT << "\033[1;31mBlinkyBlocksWorld constructor\033[0m" << endl;
	objBlock = new ObjLoader::ObjLoader("../../simulatorCore/blinkyBlocksTextures","blinkyBlockCentered.obj");
	objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/blinkyBlocksTextures",
												  "blinkyBlockPickingCentered.obj");
	objRepere = new ObjLoader::ObjLoader("../../simulatorCore/smartBlocksTextures","repere25.obj");

	lattice = new SCLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
}

BlinkyBlocksWorld::~BlinkyBlocksWorld() {
	OUTPUT << "BlinkyBlocksWorld destructor" << endl;
	/*	block linked are deleted by world::~world() */
	delete objBlock;
	delete objBlockForPicking;
	delete objRepere;
	/* free Scenario Events */
	vector<ScenarioEvent*>::const_iterator it=tabEvents.begin();
	while (it!=tabEvents.end()) {
		delete (*it);
		it++;
	}
	tabEvents.clear();
}

void BlinkyBlocksWorld::deleteWorld() {
	delete((BlinkyBlocksWorld*)world);
}

void BlinkyBlocksWorld::addBlock(int blockId, BlockCodeBuilder bcb,
								 const Cell3DPosition &pos, const Color &col,
								 short orientation, bool master) {
	if (blockId == -1) {
		map<int, BaseSimulator::BuildingBlock*>::iterator it;
		for(it = buildingBlocksMap.begin();
			it != buildingBlocksMap.end(); it++) {
			BlinkyBlocksBlock* bb = (BlinkyBlocksBlock*) it->second;
			if (it->second->blockId > blockId) blockId = bb->blockId;
		}
		blockId++;
	}
	BlinkyBlocksBlock *blinkyBlock = new BlinkyBlocksBlock(blockId, bcb);
	buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
							 (blinkyBlock->blockId, (BaseSimulator::BuildingBlock*)blinkyBlock));
	getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), blinkyBlock));

	BlinkyBlocksGlBlock *glBlock = new BlinkyBlocksGlBlock(blockId);
	tabGlBlocks.push_back(glBlock);
	blinkyBlock->setGlBlock(glBlock);
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
	vector<Cell3DPosition> nCells = lattice->getNeighborhood(pos);

	// Check neighbors for each interface
	for (int i = 0; i < 6; i++) {
		ptrNeighbor = (BlinkyBlocksBlock*)lattice->getBlock(nCells[i]);
		if (ptrNeighbor) {
			(ptrBlock)->getInterface(NeighborDirection::Direction(i))->
				connect(ptrNeighbor->getInterface(NeighborDirection::Direction(
													  NeighborDirection::getOpposite(i))));

			OUTPUT << "connection #" << (ptrBlock)->blockId <<
				" to #" << ptrNeighbor->blockId << endl;
		} else {
			(ptrBlock)->getInterface(NeighborDirection::Direction(i))->connect(NULL);
		}
	}
}

void BlinkyBlocksWorld::deleteBlock(BuildingBlock *blc) {
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock *)blc;

	if (bb->getState() >= BlinkyBlocksBlock::ALIVE ) {
		// cut links between bb and others
		for(int i=0; i<6; i++) {
			P2PNetworkInterface *bbi = bb->getInterface(NeighborDirection::Direction(i));
			if (bbi->connectedInterface) {
				//bb->removeNeighbor(bbi); //Useless
				bbi->connectedInterface->hostBlock->removeNeighbor(bbi->connectedInterface);
				bbi->connectedInterface->connectedInterface=NULL;
				bbi->connectedInterface=NULL;
			}
		}
		// free grid cell
		lattice->remove(bb->position);

		// remove the block from the lists
		//buildingBlocksMap.erase(bb->blockId);
		// remove event from the list
		//getScheduler()->removeEventsToBlock(bb);

		bb->stop(getScheduler()->now(), BlinkyBlocksBlock::REMOVED); // schedule stop event, set REMOVED state
		linkBlocks();
	}

	if (selectedBlock == bb->ptrGlBlock) {
		selectedBlock = NULL;
		GlutContext::mainWindow->select(NULL);
	}

	// remove the associated glBlock
	std::vector<GlBlock*>::iterator cit=tabGlBlocks.begin();
	if (*cit==bb->ptrGlBlock) tabGlBlocks.erase(cit);
	else {
		while (cit!=tabGlBlocks.end() && (*cit)!=bb->ptrGlBlock) {
			cit++;
		}
		if (*cit==bb->ptrGlBlock) tabGlBlocks.erase(cit);
	}
	delete bb->ptrGlBlock;
}


void BlinkyBlocksWorld::glDraw() {
	static const GLfloat white[]={0.8f,0.8f,0.8f,1.0f},
		gray[]={0.2f,0.2f,0.2f,1.0};

		glPushMatrix();
		glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
		// glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0);
		glDisable(GL_TEXTURE_2D);
		vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
		lock();
		while (ic!=tabGlBlocks.end()) {
			((BlinkyBlocksGlBlock*)(*ic))->glDraw(objBlock);
			ic++;
		}
		unlock();

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

void BlinkyBlocksWorld::glDrawId() {
	glPushMatrix();
	glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0);
	glDisable(GL_TEXTURE_2D);
	vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
	int n=1;
	lock();
	while (ic!=tabGlBlocks.end()) {
		((BlinkyBlocksGlBlock*)(*ic))->glDrawId(objBlock,n);
		ic++;
	}
	unlock();
	glPopMatrix();
}

void BlinkyBlocksWorld::glDrawIdByMaterial() {
	glPushMatrix();
	glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0);

	glDisable(GL_TEXTURE_2D);
	vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
	int n=1;
	lock();
	while (ic!=tabGlBlocks.end()) {
		((BlinkyBlocksGlBlock*)(*ic))->glDrawIdByMaterial(objBlockForPicking,n);
		ic++;
	}
	unlock();
	glPopMatrix();
}


void BlinkyBlocksWorld::loadTextures(const string &str) {
	string path = str+"/texture_plane.tga";
	int lx,ly;
	idTextureWall = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void BlinkyBlocksWorld::setSelectedFace(int n) {
	numSelectedBlock=n/6;
	cerr << "Face n = " << n << " / " << numSelectedBlock << endl;
	string name = objBlockForPicking->getObjMtlName(n%6);
	cerr << name << endl;
	if (name=="face_top") numSelectedFace=NeighborDirection::Top;
	else if (name=="face_bottom") numSelectedFace=NeighborDirection::Bottom;
	else if (name=="face_right") numSelectedFace=NeighborDirection::Right;
	else if (name=="face_left") numSelectedFace=NeighborDirection::Left;
	else if (name=="face_front") numSelectedFace=NeighborDirection::Front;
	else if (name=="face_back") numSelectedFace=NeighborDirection::Back;
}

/**
 * @brief Schedules an accel change event for block with id bId, at time date,
 *  and with the coordinates (x,y,z).
 *
 * @param date : the date at which the tap event must be consumed
 * @param bId : the id of the target block
 * @param x : x coordinate of accelerometer change
 * @param y : y coordinate of accelerometer change
 * @param z : z coordinate of accelerometer change
 */
void BlinkyBlocksWorld::accelBlock(uint64_t date, int bId, int x, int y, int z) {
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*)getBlockById(bId);
	bb->accel(date, x,y,z);
}

/**
 * @brief Schedules an accel change event for block with id bId, at time date,
 *  and with force f.
 *
 * @param date : the date at which the tap event must be consumed
 * @param bId : the id of the target block
 * @param f : force of the shake
 */
void BlinkyBlocksWorld::shakeBlock(uint64_t date, int bId, int f) {
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*)getBlockById(bId);
	bb->shake(date, f);
}

/**
 * @brief Schedules the stopping of block with id bId at a given date
 *
 * @param date : the date at which the tap event must be consumed
 * @param bId : the id of the target block
 */
void BlinkyBlocksWorld::stopBlock(uint64_t date, int bId) {
	if (bId < 0) {
		// Delete the block	without deleting the links
		map<int, BaseSimulator::BuildingBlock*>::iterator it;
		for(it = buildingBlocksMap.begin();
			it != buildingBlocksMap.end(); it++) {
			BlinkyBlocksBlock* bb = (BlinkyBlocksBlock*) it->second;
			if (bb->getState() >= BlinkyBlocksBlock::ALIVE )
				bb->stop(date, BlinkyBlocksBlock::STOPPED);
		}
	} else {
		// Delete all the links and then the block
		BlinkyBlocksBlock *bb = (BlinkyBlocksBlock *)getBlockById(bId);
		if(bb->getState() >= BlinkyBlocksBlock::ALIVE) {
			// cut links between bb and others
			for(int i=0; i<6; i++) {
				P2PNetworkInterface *bbi = bb->getInterface(NeighborDirection::Direction(i));
				if (bbi->connectedInterface) {
					//bb->removeNeighbor(bbi); //Useless
					bbi->connectedInterface->hostBlock->removeNeighbor(bbi->connectedInterface);
					bbi->connectedInterface->connectedInterface=NULL;
					bbi->connectedInterface=NULL;
				}
			}
			// free grid cell
			lattice->remove(bb->position);
			bb->stop(date, BlinkyBlocksBlock::STOPPED); // schedule stop event, set STOPPED state
			linkNeighbors(bb->position);
		}
	}
}

void BlinkyBlocksWorld::exportConfiguration() {
	// ofstream configFile;
	// BlinkyBlocksBlock *bb = (BlinkyBlocksBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
	// string configFilename = ConfigUtils::generateConfigFilename();

	// configFile.open(configFilename);
	// configFile << ConfigUtils::xmlVersion() << endl;
	// configFile << ConfigUtils::xmlWorldOpen(gridSize, GlutContext::screenWidth,
	//										GlutContext::screenHeight) << endl;
	// configFile << ConfigUtils::xmlCamera(getCamera()) << endl;
	// configFile << ConfigUtils::xmlSpotlight(&getCamera()->ls) << endl;
	// configFile << ConfigUtils::xmlBlockList(bb->color, (float*)blockSize, getMap()) << endl;
	// configFile << ConfigUtils::xmlWorldClose() << endl;

	// configFile.close();

	// OUTPUT << "Configuration exported to: " << configFilename << endl;
	// cerr << "Configuration exported to: " << configFilename << endl;

	BlinkyBlocksConfigExporter *exporter = new BlinkyBlocksConfigExporter(this);
	exporter->exportConfiguration();
}

/**
 * @brief Dumps the content of the world (all blocks) to *stdout*
 *
 */
void BlinkyBlocksWorld::dump() {
	map<int, BaseSimulator::BuildingBlock*>::iterator it;
	cout << "World:" << endl;
	for(it = buildingBlocksMap.begin();
		it != buildingBlocksMap.end(); it++) {
		BlinkyBlocksBlock* bb = (BlinkyBlocksBlock*) it->second;
		cout << *bb << endl;
	}
}

} // BlinkyBlock namespace
