/*
 * smartBlockWorld.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include <stdlib.h>
#include <string>
#include "smartBlocksWorld.h"
#include "smartBlocksScheduler.h"
#include "smartBlocksBlock.h"
#include "scheduler.h"

using namespace std;

namespace SmartBlocks {

SmartBlocksWorld::SmartBlocksWorld(int gw,int gh,int argc, char *argv[]):World() {
	cout << "\033[1;31mSmartBlocksWorld constructor\033[0m" << endl;
	gridSize[0] = gw;
	gridSize[1] = gh;
	tabPtrBlocks = new SmartBlocksBlock*[gw*gh];
	// initialise grid of blocks
	int i=gw*gh;
	SmartBlocksBlock **ptr = tabPtrBlocks;
	while (i--) {
		*ptr=NULL;
		ptr++;
	}

	blockSize[0]=25.0;
	blockSize[1]=25.0;
	blockSize[2]=11.0;
	targetGrid=new presence[gw*gh];
	memset(targetGrid,0,gw*gh*sizeof(presence));
	GlutContext::init(argc,argv);
	idTextureFloor=0;
	idTextureDigits=0;
#ifdef GLUT
	objBlock = new ObjLoader::ObjLoader("../../simulatorCore/smartBlocksTextures","smartBlockSimple.obj");
	objRepere = new ObjLoader::ObjLoader("../../simulatorCore/smartBlocksTextures","repere25.obj");
#else
    objBlock=NULL;
    objRepere=NULL;
#endif
	camera = new Camera(-M_PI/2.0,M_PI/3.0,750.0);
	camera->setLightParameters(Vecteur(0,0,0),45.0,80.0,800.0,45.0,10.0,1500.0);
	camera->setTarget(Vecteur(0,0,1.0));

	nbreStats=0;
	for (i=0; i<10; i++) {
        tabStatsData[i]=0;
    }
}

SmartBlocksWorld::~SmartBlocksWorld() {
	cout << "\033[1;31mSmartBlocksWorld destructor" << endl;

/*	block linked are deleted by world::~world() */
	delete [] tabPtrBlocks;
	delete objBlock;
	delete objRepere;
	delete camera;
	delete [] targetGrid;
	delete capabilities;
}

void SmartBlocksWorld::createWorld(int gw,int gh,int argc, char *argv[]) {
	world = new SmartBlocksWorld(gw,gh,argc,argv);
}

void SmartBlocksWorld::deleteWorld() {
	delete((SmartBlocksWorld*)world);
	world=NULL;
}

void SmartBlocksWorld::addBlock(int blockId, SmartBlocksBlockCode *(*smartBlockCodeBuildingFunction)(SmartBlocksBlock*),const Vecteur &pos,const Color &col) {
	SmartBlocksBlock *smartBlock = new SmartBlocksBlock(blockId,smartBlockCodeBuildingFunction);
	buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>(smartBlock->blockId, (BaseSimulator::BuildingBlock*)smartBlock) );
	getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), smartBlock));

	SmartBlocksGlBlock *glBlock = new SmartBlocksGlBlock(blockId);
	tabGlBlocks.push_back(glBlock);
	smartBlock->setGlBlock(glBlock);
	smartBlock->setPosition(pos);
	smartBlock->setColor(col);

	int x,y;
	smartBlock->getGridPosition(x,y);
	if (x>=0 && x<gridSize[0] && y>=0 && y<gridSize[1]) {
		setGridPtr(x,y,smartBlock);
	} else {
		cerr << "ERROR : BLOCK #" << blockId << " out of the grid !!!!!" << endl;
		exit(1);
	}
}

void SmartBlocksWorld::linkBlocks() {
	int ix,iy;
	for (iy=0; iy<gridSize[1]; iy++) {
		for(ix=0; ix<gridSize[0]; ix++) {
			linkBlock(ix,iy);
		}
	}
}

void SmartBlocksWorld::linkBlock(int ix, int iy) {
    SmartBlocksBlock *ptrBlock = getGridPtr(ix,iy);
	if (ptrBlock) {
		if (iy<gridSize[1]-1 && getGridPtr(ix,iy+1)) {
			(ptrBlock)->getInterface(North)->connect(getGridPtr(ix,iy+1)->getInterface(South));
//			OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix,iy+1)->blockId << endl;
		} else {
			(ptrBlock)->getInterface(North)->connect(NULL);
		}
		if (ix<gridSize[0]-1 && getGridPtr(ix+1,iy)) {
			(ptrBlock)->getInterface(East)->connect(getGridPtr(ix+1,iy)->getInterface(West));
//			OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix+1,iy)->blockId << endl;
		} else {
			(ptrBlock)->getInterface(East)->connect(NULL);
		}
		if (iy>0 && getGridPtr(ix,iy-1)) {
			(ptrBlock)->getInterface(South)->connect(getGridPtr(ix,iy-1)->getInterface(North));
//			OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix,iy-1)->blockId << endl;
		} else {
			(ptrBlock)->getInterface(South)->connect(NULL);
		}
		if (ix>0 && getGridPtr(ix-1,iy)) {
			(ptrBlock)->getInterface(West)->connect(getGridPtr(ix-1,iy)->getInterface(East));
//			OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix-1,iy)->blockId << endl;
		} else {
			(ptrBlock)->getInterface(West)->connect(NULL);
		}
	}
}

void SmartBlocksWorld::getPresenceMatrix(const PointCel &pos,PresenceMatrix &pm) {
    presence *gpm=pm.grid;
    SmartBlocksBlock **grb;

    for (int i=0; i<9; i++) { *gpm++ = wallCell; };
    int ix0 = (pos.x<1)?1-pos.x:0,
        ix1 = (pos.x>gridSize[0]-2)?gridSize[0]-pos.x+1:3,
        iy0 = (pos.y<1)?1-pos.y:0,
        iy1 = (pos.y>gridSize[1]-2)?gridSize[1]-pos.y+1:3,
        ix,iy;
    for (iy=iy0; iy<iy1; iy++) {
        gpm = pm.grid+(iy*3+ix0);
        grb = tabPtrBlocks+(ix0+pos.x-1+(iy+pos.y-1)*gridSize[0]);
        for (ix=ix0; ix<ix1; ix++) {
//            *gpm++ = (*grb)?((isBorder(ix+pos.x-1,iy+pos.y-1))?(((*grb)->wellPlaced||!isTrain(ix+pos.x-1,iy+pos.y-1))?wellPlacedBorderCell:borderCell):fullCell):emptyCell;
//            *gpm++ = (*grb)?((isBorder(ix+pos.x-1,iy+pos.y-1))?((*grb)->wellPlaced?wellPlacedBorderCell:borderCell):fullCell):emptyCell;
            *gpm++ = (*grb)?((isBorder(ix+pos.x-1,iy+pos.y-1))?(isSingle(ix+pos.x-1,iy+pos.y-1)?singleCell:borderCell):fullCell):emptyCell;
            grb++;
        }
    }
}

void SmartBlocksWorld::getPresenceMatrix0(const PointCel &pos,PresenceMatrix &pm) {
    presence *gpm=pm.grid;
    SmartBlocksBlock **grb;

    for (int i=0; i<9; i++) { *gpm++ = wallCell; };

    int ix0 = (pos.x<1)?1-pos.x:0,
        ix1 = (pos.x>gridSize[0]-2)?gridSize[0]-pos.x+1:3,
        iy0 = (pos.y<1)?1-pos.y:0,
        iy1 = (pos.y>gridSize[1]-2)?gridSize[1]-pos.y+1:3,
        ix,iy;
    for (iy=iy0; iy<iy1; iy++) {
        gpm = pm.grid+(iy*3+ix0);
        grb = tabPtrBlocks+(ix0+pos.x-1+(iy+pos.y-1)*gridSize[0]);
        for (ix=ix0; ix<ix1; ix++) {
            *gpm++ = (*grb)?fullCell:emptyCell;
            grb++;
        }
    }
}

bool SmartBlocksWorld::isBorder(int x,int y) {
    SmartBlocksBlock **grb=tabPtrBlocks+x+y*gridSize[0];
    //if ((*grb)->_isBorder) return true;
    int ix0 = (x<1)?1-x:0,
        ix1 = (x>gridSize[0]-2)?gridSize[0]-x+1:3,
        iy0 = (y<1)?1-y:0,
        iy1 = (y>gridSize[1]-2)?gridSize[1]-y+1:3,
        ix,iy;
    for (iy=iy0; iy<iy1; iy++) {
        grb = tabPtrBlocks+(ix0+x-1+(iy+y-1)*gridSize[0]);
        for (ix=ix0; ix<ix1; ix++) {
            //if (*grb==NULL || (*grb)->wellPlaced) return true;
            if (*grb==NULL) return true;
            grb++;
        }
    }
    return false;
}

bool SmartBlocksWorld::isSingle(int x,int y) {
    SmartBlocksBlock **grb=tabPtrBlocks+x+y*gridSize[0];
    return (*grb)->_isSingle;
}

void SmartBlocksWorld::glDraw() {
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
		glScalef(gridSize[0]*blockSize[0],gridSize[1]*blockSize[1],1.0f);
		glBegin(GL_QUADS);
			glTexCoord2f(0,0);
			glVertex3f(0.0f,0.0f,0.0f);
			glTexCoord2f(gridSize[0],0);
			glVertex3f(1.0f,0.0f,0.0f);
			glTexCoord2f(gridSize[0],gridSize[1]);
			glVertex3f(1.0,1.0,0.0f);
			glTexCoord2f(0,gridSize[1]);
			glVertex3f(0.0,1.0,0.0f);
		glEnd();
	glPopMatrix();
	// draw the axes
	objRepere->glDraw();

	glPushMatrix();
	//glTranslatef(-gridSize[0]/2.0f*blockSize[0],-gridSize[1]/2.0f*blockSize[1],0);
	glDisable(GL_TEXTURE_2D);
	vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
	lock();
	while (ic!=tabGlBlocks.end()) {
		((SmartBlocksGlBlock*)(*ic))->glDraw(objBlock);
		ic++;
	}
	unlock();

	/*// drawing the mobiles
	Physics::glDraw();
*/
	glPopMatrix();

}

void SmartBlocksWorld::glDrawId() {
	glPushMatrix();
	glDisable(GL_TEXTURE_2D);
	//glTranslatef(-gridSize[0]/2.0f*blockSize[0],-gridSize[1]/2.0f*blockSize[1],0);
	vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
	int n=1;
	lock();
	while (ic!=tabGlBlocks.end()) {
		glLoadName(n++);
		((SmartBlocksGlBlock*)(*ic))->glDrawId(objBlock);
	   	ic++;
	}
	unlock();
	glPopMatrix();
}

void SmartBlocksWorld::loadTextures(const string &str) {
	//string path = str+"/circuit.tga";
	string path = str+"/bois.tga";
	int lx,ly;
	idTextureFloor = GlutWindow::loadTexture(path.c_str(),lx,ly);

	path=str+"/digits.tga";
	idTextureDigits = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void SmartBlocksWorld::updateGlData(SmartBlocksBlock*blc) {
	SmartBlocksGlBlock *glblc = blc->getGlBlock();
	if (glblc) {
		lock();

		Vecteur pos(blockSize[0]*blc->position[0],blockSize[1]*blc->position[1],0.0);
		glblc->setPosition(pos);
		glblc->setColor(blc->color);
		unlock();
		//GlutContext::mustSaveImage=true;
	}
}

void SmartBlocksWorld::connectBlock(SmartBlocksBlock *block) {
    int ix,iy;
    ix = int(block->position.pt[0]);
	iy = int(block->position.pt[1]);

	setGridPtr(ix,iy,block);
//	OUTPUT << "Reconnection " << block->blockId << " pos ="<< ix << "," << iy << endl;
	linkBlock(ix,iy);
	if (ix<gridSize[0]-1) linkBlock(ix+1,iy);
	if (ix>0) linkBlock(ix-1,iy);
	if (iy<gridSize[1]-1) linkBlock(ix,iy+1);
	if (iy>0) linkBlock(ix,iy-1);
}

void SmartBlocksWorld::disconnectBlock(SmartBlocksBlock *block) {
    P2PNetworkInterface *fromBlock,*toBlock;

    for(int i=0; i<4; i++) {
        fromBlock = block->getInterface(NeighborDirection(i));
        if (fromBlock && fromBlock->connectedInterface) {
            toBlock = fromBlock->connectedInterface;
            fromBlock->connectedInterface=NULL;
            toBlock->connectedInterface=NULL;
        }
    }
    int ix,iy;
    ix = int(block->position.pt[0]);
	iy = int(block->position.pt[1]);
	setGridPtr(ix,iy,NULL);
//	OUTPUT << getScheduler()->now() << " : Disconnection " << block->blockId << " pos ="<< ix << "," << iy << endl;
}

int SmartBlocksWorld::nbreWellPlacedBlock() {
    std::map<int, BuildingBlock*>::iterator it;
    int n=0;
    SmartBlocksBlock *sb;
	for( it = buildingBlocksMap.begin() ; it != buildingBlocksMap.end() ; ++it) {
		sb = (SmartBlocksBlock *)(it->second);
		if (sb->wellPlaced) n++;
	}
	return n;
}

void SmartBlocksWorld::addStat(int n,int v) {
    tabStatsData[n]+=v;
    if (nbreStats<=n) nbreStats=n+1;
}

void SmartBlocksWorld::printStats() {
    OUTPUT << "stats: \t" << nbreWellPlacedBlock();
    for (int i=0;i<nbreStats; i++) {
        OUTPUT << "\t"<< tabStatsData[i] ;
    }
    OUTPUT << "\t" << getScheduler()->getNbreMessages() << endl;
}

} // SmartBlocks namespace
