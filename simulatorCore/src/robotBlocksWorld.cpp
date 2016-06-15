/*
 * robotBlockWorld.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include <string>
#include <stdlib.h>
#include "robotBlocksWorld.h"
#include "robotBlocksBlock.h"
#include "robotBlocksScheduler.h"
#include "trace.h"
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

using namespace std;

namespace RobotBlocks {

RobotBlocksWorld::RobotBlocksWorld(int slx,int sly,int slz, int argc, char *argv[]):World() {
    OUTPUT << "\033[1;31mRobotBlocksWorld constructor\033[0m" << endl;
    gridSize[0]=slx;
    gridSize[1]=sly;
    gridSize[2]=slz;
    gridPtrBlocks = new RobotBlocksBlock*[slx*sly*slz];

    // initialise grid of blocks
    int i=slx*sly*slz;
    RobotBlocksBlock **ptr = gridPtrBlocks;
    while (i--) {
	*ptr=NULL;
	ptr++;
    }
    targetGrid=NULL;

    GlutContext::init(argc,argv);
    idTextureWall=0;
    blockSize[0]=39.0;
    blockSize[1]=39.0;
    blockSize[2]=40.0;
    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/robotBlocksTextures","robotBlock.obj");
    objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/robotBlocksTextures","robotBlockPicking.obj");
    objRepere = new ObjLoader::ObjLoader("../../simulatorCore/smartBlocksTextures","repere25.obj");
    camera = new Camera(-M_PI/2.0,M_PI/3.0,750.0);
    camera->setLightParameters(Vector3D(0,0,0),45.0,80.0,800.0,45.0,10.0,1500.0);
    camera->setTarget(Vector3D(0,0,1.0));

    menuId=0;
    numSelectedFace=0;
    numSelectedBlock=0;

}

RobotBlocksWorld::~RobotBlocksWorld() {
    OUTPUT << "RobotBlocksWorld destructor" << endl;
    /*	block linked are deleted by world::~world() */
    delete [] gridPtrBlocks;
    delete [] targetGrid;
    delete objBlock;
    delete objBlockForPicking;
    delete objRepere;
    delete camera;
}

void RobotBlocksWorld::createWorld(int slx,int sly,int slz, int argc, char *argv[]) {
    world = new RobotBlocksWorld(slx,sly,slz,argc,argv);
}

void RobotBlocksWorld::deleteWorld() {
    delete((RobotBlocksWorld*)world);
}

void RobotBlocksWorld::addBlock(int blockId, RobotBlocksBlockCode *(*robotBlockCodeBuildingFunction)(RobotBlocksBlock*),const Cell3DPosition &pos,const Color &color,bool master) {

    if (blockId == -1) {
	map<int, BaseSimulator::BuildingBlock*>::iterator it;
	for(it = buildingBlocksMap.begin();
	    it != buildingBlocksMap.end(); it++) {
	    RobotBlocksBlock* bb = (RobotBlocksBlock*) it->second;
	    if (it->second->blockId > blockId) blockId = bb->blockId;
	}
	blockId++;
    }
    RobotBlocksBlock *robotBlock = new RobotBlocksBlock(blockId,robotBlockCodeBuildingFunction);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>(robotBlock->blockId, (BaseSimulator::BuildingBlock*)robotBlock));

    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), robotBlock));

    RobotBlocksGlBlock *glBlock = new RobotBlocksGlBlock(blockId);
    tabGlBlocks.push_back(glBlock);
    robotBlock->setGlBlock(glBlock);
    robotBlock->setPosition(pos);
    robotBlock->setColor(color);
    robotBlock->isMaster=master;

    short ix,iy,iz;
    ix = robotBlock->position.pt[0];
    iy = robotBlock->position.pt[1];
    iz = robotBlock->position.pt[2];
    if (ix>=0 && ix<gridSize[0] &&
	iy>=0 && iy<gridSize[1] &&
	iz>=0 && iz<gridSize[2]) {
	setGridPtr(ix,iy,iz,robotBlock);
    } else {
	ERRPUT << "ERROR : BLOCK #" << blockId << " out of the grid !!!!!" << endl;
	exit(1);
    }
}

void RobotBlocksWorld::connectBlock(RobotBlocksBlock *block) {
    int ix,iy,iz;
    ix = int(block->position.pt[0]);
    iy = int(block->position.pt[1]);
    iz = int(block->position.pt[2]);
    setGridPtr(ix,iy,iz,block);
    OUTPUT << "Reconnection " << block->blockId << " pos ="<< ix << "," << iy << "," << iz << endl;
    linkBlock(ix,iy,iz);
    if (ix<gridSize[0]-1) linkBlock(ix+1,iy,iz);
    if (ix>0) linkBlock(ix-1,iy,iz);
    if (iy<gridSize[1]-1) linkBlock(ix,iy+1,iz);
    if (iy>0) linkBlock(ix,iy-1,iz);
    if (iz<gridSize[2]-1) linkBlock(ix,iy,iz+1);
    if (iz>0) linkBlock(ix,iy,iz-1);
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
    int ix,iy,iz;
    ix = int(block->position.pt[0]);
    iy = int(block->position.pt[1]);
    iz = int(block->position.pt[2]);
    setGridPtr(ix,iy,iz,NULL);
    OUTPUT << getScheduler()->now() << " : Disconnection " << block->blockId << " pos ="<< ix << "," << iy << "," << iz << endl;
}

void RobotBlocksWorld::linkBlock(int ix, int iy, int iz) {
    RobotBlocksBlock *ptrBlock = getGridPtr(ix,iy,iz);
    if (ptrBlock) {
	if (iz<gridSize[2]-1 && getGridPtr(ix,iy,iz+1)) {
	    (ptrBlock)->getInterface(NeighborDirection::Top)->connect(getGridPtr(ix,iy,iz+1)->getInterface(NeighborDirection::Bottom));
	    OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix,iy,iz+1)->blockId << endl;
	} else {
	    (ptrBlock)->getInterface(NeighborDirection::Top)->connect(NULL);
	}
	if (iy<gridSize[1]-1 && getGridPtr(ix,iy+1,iz)) {
	    (ptrBlock)->getInterface(NeighborDirection::Right)->connect(getGridPtr(ix,iy+1,iz)->getInterface(NeighborDirection::Left));
	    OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix,iy+1,iz)->blockId << endl;
	} else {
	    (ptrBlock)->getInterface(NeighborDirection::Right)->connect(NULL);
	}
	if (ix<gridSize[0]-1 && getGridPtr(ix+1,iy,iz)) {
	    (ptrBlock)->getInterface(NeighborDirection::Front)->connect(getGridPtr(ix+1,iy,iz)->getInterface(NeighborDirection::Back));
	    OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix+1,iy,iz)->blockId << endl;
	} else {
            (ptrBlock)->getInterface(NeighborDirection::Front)->connect(NULL);
        }
	if (iy>0 && getGridPtr(ix,iy-1,iz)) {
	    (ptrBlock)->getInterface(NeighborDirection::Left)->connect(getGridPtr(ix,iy-1,iz)->getInterface(NeighborDirection::Right));
	    OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix,iy-1,iz)->blockId << endl;
	} else {
            (ptrBlock)->getInterface(NeighborDirection::Left)->connect(NULL);
	}
	if (iz>0 && getGridPtr(ix,iy,iz-1)) {
	    (ptrBlock)->getInterface(NeighborDirection::Bottom)->connect(getGridPtr(ix,iy,iz-1)->getInterface(NeighborDirection::Top));
	    OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix,iy,iz-1)->blockId << endl;
	} else {
	    (ptrBlock)->getInterface(NeighborDirection::Bottom)->connect(NULL);
	}
	if (ix>0 && getGridPtr(ix-1,iy,iz)) {
	    (ptrBlock)->getInterface(NeighborDirection::Back)->connect(getGridPtr(ix-1,iy,iz)->getInterface(NeighborDirection::Front));
	    OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix-1,iy,iz)->blockId << endl;
	} else {
	    (ptrBlock)->getInterface(NeighborDirection::Back)->connect(NULL);
	}
    }
}

void RobotBlocksWorld::linkBlocks() {
    int ix,iy,iz;
    for (iz=0; iz<gridSize[2]; iz++) {
	for (iy=0; iy<gridSize[1]; iy++) {
	    for(ix=0; ix<gridSize[0]; ix++) {
		linkBlock(ix,iy,iz);
	    }
	}
    }
}

void RobotBlocksWorld::deleteBlock(RobotBlocksBlock *bb) {
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
	// free grid cell
	int ix,iy,iz;
	ix = int(bb->position.pt[0]);
	iy = int(bb->position.pt[1]);
	iz = int(bb->position.pt[2]);
	setGridPtr(ix,iy,iz,NULL);

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
	glTranslatef(0.5*blockSize[0],0.5*blockSize[1],0.5*blockSize[2]);
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
	glScalef(gridSize[0]*blockSize[0],gridSize[1]*blockSize[1],gridSize[2]*blockSize[2]);
	glBegin(GL_QUADS);
	// bottom
	glNormal3f(0,0,1.0f);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,0.0f,0.0f);
	glTexCoord2f(gridSize[0],0);
	glVertex3f(1.0f,0.0f,0.0f);
	glTexCoord2f(gridSize[0],gridSize[1]);
	glVertex3f(1.0,1.0,0.0f);
	glTexCoord2f(0,gridSize[1]);
	glVertex3f(0.0,1.0,0.0f);
	// top
	glNormal3f(0,0,-1.0f);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,0.0f,1.0f);
	glTexCoord2f(0,gridSize[1]);
	glVertex3f(0.0,1.0,1.0f);
	glTexCoord2f(gridSize[0],gridSize[1]);
	glVertex3f(1.0,1.0,1.0f);
	glTexCoord2f(gridSize[0],0);
	glVertex3f(1.0f,0.0f,1.0f);
	// left
	glNormal3f(1.0,0,0);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,0.0f,0.0f);
	glTexCoord2f(gridSize[1],0);
	glVertex3f(0.0f,1.0f,0.0f);
	glTexCoord2f(gridSize[1],gridSize[2]);
	glVertex3f(0.0,1.0,1.0f);
	glTexCoord2f(0,gridSize[2]);
	glVertex3f(0.0,0.0,1.0f);
	// right
	glNormal3f(-1.0,0,0);
	glTexCoord2f(0,0);
	glVertex3f(1.0f,0.0f,0.0f);
	glTexCoord2f(0,gridSize[2]);
	glVertex3f(1.0,0.0,1.0f);
	glTexCoord2f(gridSize[1],gridSize[2]);
	glVertex3f(1.0,1.0,1.0f);
	glTexCoord2f(gridSize[1],0);
	glVertex3f(1.0f,1.0f,0.0f);
	// back
	glNormal3f(0,-1.0,0);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,1.0f,0.0f);
	glTexCoord2f(gridSize[0],0);
	glVertex3f(1.0f,1.0f,0.0f);
	glTexCoord2f(gridSize[0],gridSize[2]);
	glVertex3f(1.0f,1.0,1.0f);
	glTexCoord2f(0,gridSize[2]);
	glVertex3f(0.0,1.0,1.0f);
	// front
	glNormal3f(0,1.0,0);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,0.0f,0.0f);
	glTexCoord2f(0,gridSize[2]);
	glVertex3f(0.0,0.0,1.0f);
	glTexCoord2f(gridSize[0],gridSize[2]);
	glVertex3f(1.0f,0.0,1.0f);
	glTexCoord2f(gridSize[0],0);
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
    glTranslatef(0.5*blockSize[0],0.5*blockSize[1],0.5*blockSize[2]);
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
    glTranslatef(0.5*blockSize[0],0.5*blockSize[1],0.5*blockSize[2]);

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
    switch (n) {
    case 1 : {
	RobotBlocksBlock *bb = (RobotBlocksBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
	OUTPUT << "ADD block link to : " << bb->blockId << "     num Face : " << numSelectedFace << endl;
        Cell3DPosition pos=bb->position;
	switch (numSelectedFace) {
	case NeighborDirection::Left :
	    pos.pt[0]--;
	    break;
	case NeighborDirection::Right :
	    pos.pt[0]++;
	    break;
	case NeighborDirection::Front :
	    pos.pt[1]--;
	    break;
	case NeighborDirection::Back :
	    pos.pt[1]++;
	    break;
	case NeighborDirection::Bottom :
	    pos.pt[2]--;
	    break;
	case NeighborDirection::Top :
	    pos.pt[2]++;
	    break;
	}
	addBlock(-1, bb->buildNewBlockCode,pos,bb->color);
	linkBlocks();
    } break;
    case 2 : {
	OUTPUT << "DEL num block : " << tabGlBlocks[numSelectedBlock]->blockId << endl;
	RobotBlocksBlock *bb = (RobotBlocksBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
	deleteBlock(bb);
    } break;
    case 3 : {
	RobotBlocksBlock *bb = (RobotBlocksBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
	tapBlock(getScheduler()->now(), bb->blockId);
    } break;
    case 4:                 // Save current configuration
	exportConfiguration();
	break;
    }
}

bool RobotBlocksWorld::canAddBlockToFace(int numSelectedBlock, int numSelectedFace) {
    RobotBlocksBlock *bb = (RobotBlocksBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
    switch (numSelectedFace) {
    case NeighborDirection::Left :
        return (bb->position[0]>0
                && getGridPtr(int(bb->position[0])-1,
                              int(bb->position[1]),
                              int(bb->position[2])) == NULL);
        break;
    case NeighborDirection::Right :
        return (bb->position[0]<gridSize[0]-1
                && getGridPtr(int(bb->position[0])+1,
                              int(bb->position[1]),
                              int(bb->position[2])) == NULL);
        break;
    case NeighborDirection::Front :
        return (bb->position[1]>0
                && getGridPtr(int(bb->position[0]),
                              int(bb->position[1])-1,
                              int(bb->position[2])) == NULL);
        break;
    case NeighborDirection::Back :
        return (bb->position[1]<gridSize[1]-1
                && getGridPtr(int(bb->position[0]),
                              int(bb->position[1])+1,
                              int(bb->position[2])) == NULL);
        break;
    case NeighborDirection::Bottom :
        return (bb->position[2]>0
                && getGridPtr(int(bb->position[0]),
                              int(bb->position[1]),
                              int(bb->position[2])-1) == NULL);
        break;
    case NeighborDirection::Top :
        return (bb->position[2]<gridSize[2]-1
                && getGridPtr(int(bb->position[0]),
                              int(bb->position[1]),
                              int(bb->position[2])+1) == NULL);
        break;
    }

    return false;
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

void RobotBlocksWorld::createHelpWindow() {
    if (GlutContext::helpWindow)
	delete GlutContext::helpWindow;
    GlutContext::helpWindow = new GlutHelpWindow(NULL,10,40,540,500,"../../simulatorCore/genericHelp.txt");
}

void RobotBlocksWorld::getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm) {
    presence *gpm=pm.grid;
    RobotBlocksBlock **grb;

    //memset(pm.grid,wall,27*sizeof(presence));

    for (int i=0; i<27; i++) { *gpm++ = wallCell; };

    int ix0 = (pos.x<1)?1-pos.x:0,
        ix1 = (pos.x>gridSize[0]-2)?gridSize[0]-pos.x+1:3,
        iy0 = (pos.y<1)?1-pos.y:0,
        iy1 = (pos.y>gridSize[1]-2)?gridSize[1]-pos.y+1:3,
        iz0 = (pos.z<1)?1-pos.z:0,
        iz1 = (pos.z>gridSize[2]-2)?gridSize[2]-pos.z+1:3,
        ix,iy,iz;
    for (iz=iz0; iz<iz1; iz++) {
        for (iy=iy0; iy<iy1; iy++) {
            gpm = pm.grid+((iz*3+iy)*3+ix0);
            grb = gridPtrBlocks+(ix0+pos.x-1+(iy+pos.y-1+(iz+pos.z-1)*gridSize[1])*gridSize[0]);
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
    // 					    GlutContext::screenHeight) << endl;
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
    int sz = gridSize[0]*gridSize[1]*gridSize[2];
    targetGrid = new presence[gridSize[0]*gridSize[1]*gridSize[2]];
    memset(targetGrid,0,sz*sizeof(presence));
}

} // RobotBlock namespace

