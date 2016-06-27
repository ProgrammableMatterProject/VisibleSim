/*
 * robotBlockWorld.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include <string>
#include <stdlib.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

#include "robotBlocksWorld.h"
#include "robotBlocksBlock.h"
#include "trace.h"

using namespace std;

namespace RobotBlocks {

RobotBlocksWorld::RobotBlocksWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
								   int argc, char *argv[]):World(argc, argv) {
	OUTPUT << "\033[1;31mRobotBlocksWorld constructor\033[0m" << endl;

	targetGrid=NULL;

	idTextureWall=0;
	objBlock = new ObjLoader::ObjLoader("../../simulatorCore/robotBlocksTextures","robotBlock.obj");
	objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/robotBlocksTextures",
												  "robotBlockPicking.obj");
	objRepere = new ObjLoader::ObjLoader("../../simulatorCore/smartBlocksTextures","repere25.obj");

	lattice = new SCLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
}

RobotBlocksWorld::~RobotBlocksWorld() {
	OUTPUT << "RobotBlocksWorld destructor" << endl;
	/*	block linked are deleted by world::~world() */
	delete [] targetGrid;
	delete objBlock;
	delete objBlockForPicking;
	delete objRepere;
}

void RobotBlocksWorld::deleteWorld() {
	delete((RobotBlocksWorld*)world);
}

void RobotBlocksWorld::addBlock(int blockId, BlockCodeBuilder bcb,
								const Cell3DPosition &pos, const Color &col,
								short orientation, bool master) {

	if (blockId == -1) {
		map<int, BaseSimulator::BuildingBlock*>::iterator it;
		for(it = buildingBlocksMap.begin();
			it != buildingBlocksMap.end(); it++) {
			RobotBlocksBlock* bb = (RobotBlocksBlock*) it->second;
			if (it->second->blockId > blockId) blockId = bb->blockId;
		}
		blockId++;
	}
	RobotBlocksBlock *robotBlock = new RobotBlocksBlock(blockId, bcb);
	buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
							 (robotBlock->blockId, (BaseSimulator::BuildingBlock*)robotBlock));

	getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), robotBlock));

	RobotBlocksGlBlock *glBlock = new RobotBlocksGlBlock(blockId);
	tabGlBlocks.push_back(glBlock);
	robotBlock->setGlBlock(glBlock);
	robotBlock->setPosition(pos);
	robotBlock->setColor(col);
	robotBlock->isMaster=master;

	if (lattice->isInGrid(pos)) {
		lattice->insert(robotBlock, pos);
	} else {
		ERRPUT << "ERROR : BLOCK #" << blockId << " out of the grid !!!!!" << endl;
		exit(1);
	}
}

void RobotBlocksWorld::connectBlock(RobotBlocksBlock *block) {
	Cell3DPosition pos = block->position;
	lattice->insert(block, pos);
	OUTPUT << "Reconnection " << block->blockId << " pos = " << pos << endl;
	linkBlock(pos);
	linkNeighbors(pos);
}

void RobotBlocksWorld::disconnectBlock(RobotBlocksBlock *block) {
	P2PNetworkInterface *fromBlock,*toBlock;

	for(int i=0; i<6; i++) {
		fromBlock = block->getInterface(NeighborDirection::Direction(i));
		if (fromBlock && fromBlock->connectedInterface) {
			toBlock = fromBlock->connectedInterface;
			fromBlock->connectedInterface=NULL;
			toBlock->connectedInterface=NULL;
		}
	}

	lattice->remove(block->position);
	OUTPUT << getScheduler()->now() << " : Disconnection " << block->blockId
		   << " pos =" << block->position << endl;
}

void RobotBlocksWorld::linkBlock(const Cell3DPosition &pos) {
	RobotBlocksBlock *ptrNeighbor;
	RobotBlocksBlock *ptrBlock = (RobotBlocksBlock*)lattice->getBlock(pos);
	vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
	Cell3DPosition nPos;

	// Check neighbors for each interface
	for (int i = 0; i < 6; i++) {
		nPos = pos + nRelCells[i];
		ptrNeighbor = (RobotBlocksBlock*)lattice->getBlock(nPos);
		if (ptrNeighbor) {
			(ptrBlock)->getInterface(NeighborDirection::Direction(i))->
				connect(ptrNeighbor->getInterface(NeighborDirection::Direction(
													  NeighborDirection::getOpposite(i))));

			OUTPUT << "connection #" << (ptrBlock)->blockId << ":" << NeighborDirection::getString(i) <<
				" to #" << ptrNeighbor->blockId << ":"
				   << NeighborDirection::getString(NeighborDirection::getOpposite(i)) << endl;
		} else {
			(ptrBlock)->getInterface(NeighborDirection::Direction(i))->connect(NULL);
		}
	}
}

void RobotBlocksWorld::deleteBlock(BuildingBlock *blc) {
	RobotBlocksBlock *bb = (RobotBlocksBlock *)blc;

	if (bb->getState() >= RobotBlocksBlock::ALIVE ) {
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

		disconnectBlock(bb);
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

void RobotBlocksWorld::glDraw() {
	static const GLfloat white[]={0.8f,0.8f,0.8f,1.0f},
		gray[]={0.2f,0.2f,0.2f,1.0f};


		glPushMatrix();
		glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
		glDisable(GL_TEXTURE_2D);
		vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
		lock();
		while (ic!=tabGlBlocks.end()) {
			((RobotBlocksGlBlock*)(*ic))->glDraw(objBlock);
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

void RobotBlocksWorld::glDrawId() {
	glPushMatrix();
	glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
	glDisable(GL_TEXTURE_2D);
	vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
	int n=1;
	lock();
	while (ic!=tabGlBlocks.end()) {
		((RobotBlocksGlBlock*)(*ic))->glDrawId(objBlock,n);
		ic++;
	}
	unlock();
	glPopMatrix();
}

void RobotBlocksWorld::glDrawIdByMaterial() {
	glPushMatrix();
	glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);

	glDisable(GL_TEXTURE_2D);
	vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
	int n=1;
	lock();
	while (ic!=tabGlBlocks.end()) {
		((RobotBlocksGlBlock*)(*ic))->glDrawIdByMaterial(objBlockForPicking,n);
		ic++;
	}
	unlock();
	glPopMatrix();
}


void RobotBlocksWorld::loadTextures(const string &str) {
	string path = str+"/texture_plane.tga";
	int lx,ly;
	idTextureWall = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void RobotBlocksWorld::updateGlData(RobotBlocksBlock*blc,int prev,int next) {
	RobotBlocksGlBlock *glblc = blc->getGlBlock();
	if (glblc) {
		lock();
		glblc->setPrevNext(prev,next);
		unlock();
	}
}

void RobotBlocksWorld::menuChoice(int n) {
	RobotBlocksBlock *bb = (RobotBlocksBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
	switch (n) {
	case 1 : {
		OUTPUT << "ADD block link to : " << bb->blockId << "     num Face : " << numSelectedFace << endl;
		vector<Cell3DPosition> nCells = lattice->getRelativeConnectivity(bb->position);
		Cell3DPosition nPos = bb->position + nCells[numSelectedFace];

		addBlock(-1, bb->buildNewBlockCode, nPos, bb->color);
		linkBlock(nPos);
		linkNeighbors(nPos);
	} break;
	case 2 : {
		OUTPUT << "DEL num block : " << tabGlBlocks[numSelectedBlock]->blockId << endl;
		deleteBlock(bb);
	} break;
	case 3 : {
		tapBlock(getScheduler()->now(), bb->blockId);
	} break;
	case 4:                 // Save current configuration
		exportConfiguration();
		break;
	}
}

void RobotBlocksWorld::setSelectedFace(int n) {
	numSelectedBlock=n/6;
	string name = objBlockForPicking->getObjMtlName(n%6);
	if (name=="face_top") numSelectedFace=NeighborDirection::Top;
	else if (name=="face_bottom") numSelectedFace=NeighborDirection::Bottom;
	else if (name=="face_right") numSelectedFace=NeighborDirection::Right;
	else if (name=="face_left") numSelectedFace=NeighborDirection::Left;
	else if (name=="face_front") numSelectedFace=NeighborDirection::Front;
	else if (name=="face_back") numSelectedFace=NeighborDirection::Back;
}

void RobotBlocksWorld::getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm) {
	presence *gpm=pm.grid;
	RobotBlocksBlock **grb;

	//memset(pm.grid,wall,27*sizeof(presence));

	for (int i=0; i<27; i++) { *gpm++ = wallCell; };

	int ix0 = (pos.x<1)?1-pos.x:0,
		ix1 = (pos.x>lattice->gridSize[0]-2)?lattice->gridSize[0]-pos.x+1:3,
		iy0 = (pos.y<1)?1-pos.y:0,
		iy1 = (pos.y>lattice->gridSize[1]-2)?lattice->gridSize[1]-pos.y+1:3,
		iz0 = (pos.z<1)?1-pos.z:0,
		iz1 = (pos.z>lattice->gridSize[2]-2)?lattice->gridSize[2]-pos.z+1:3,
		ix,iy,iz;
	for (iz=iz0; iz<iz1; iz++) {
		for (iy=iy0; iy<iy1; iy++) {
			gpm = pm.grid+((iz*3+iy)*3+ix0);
			grb = (RobotBlocksBlock **)lattice->grid+
				(ix0+pos.x-1+(iy+pos.y-1+(iz+pos.z-1)*lattice->gridSize[1])*lattice->gridSize[0]);
			for (ix=ix0; ix<ix1; ix++) {
				*gpm++ = (*grb++)?fullCell:emptyCell;
			}
		}
	}
}

void RobotBlocksWorld::exportConfiguration() {
	// ofstream configFile;
	// RobotBlocksBlock *bb = (RobotBlocksBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
	// string configFilename = ConfigUtils::generateConfigFilename();

	// configFile.open(configFilename);
	// configFile << ConfigUtils::xmlVersion() << endl;
	// configFile << ConfigUtils::xmlWorldOpen(gridSize, GlutContext::screenWidth,
	//						GlutContext::screenHeight) << endl;
	// configFile << ConfigUtils::xmlCamera(getCamera()) << endl;
	// configFile << ConfigUtils::xmlSpotlight(&getCamera()->ls) << endl;
	// configFile << ConfigUtils::xmlBlockList(bb->color, (float*)blockSize, getMap()) << endl;
	// configFile << ConfigUtils::xmlWorldClose() << endl;

	// configFile.close();

	// OUTPUT << "Configuration exported to: " << configFilename << endl;
	// cerr << "Configuration exported to: " << configFilename << endl;
}


void RobotBlocksWorld::initTargetGrid() {
	if (targetGrid) delete [] targetGrid;
	int sz = lattice->gridSize[0]*lattice->gridSize[1]*lattice->gridSize[2];
	targetGrid = new presence[lattice->gridSize[0]*lattice->gridSize[1]*lattice->gridSize[2]];
	memset(targetGrid,0,sz*sizeof(presence));
}

} // RobotBlock namespace
