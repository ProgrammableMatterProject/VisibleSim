/*!
 * \file catoms3DWorld.cpp
 * \brief catoms world
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#include <iostream>
#include <string>
#include <stdlib.h>
#include "catoms3DWorld.h"
#include "catoms3DBlock.h"
#include "catoms3DScheduler.h"
#include "trace.h"
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

using namespace std;

//! \namespace Catoms3D
namespace Catoms3D {

const double M_SQRT3_2 = sqrt(3.0)/2.0;
const double M_SQRT2_2 = sqrt(2.0)/2.0;

/**
    \brief Constructor
    \param slx : grid size along x axis
    \param sly : grid size along x axis
    \param slz : grid size along x axis
    \param argc : number of execution parameters
    \param argv : string array of parameters
*/
Catoms3DWorld::Catoms3DWorld(int slx,int sly,int slz, int argc, char *argv[]):World() {
	OUTPUT << "\033[1;31mCatoms3DWorld constructor\033[0m" << endl;
	gridSize[0]=slx;
	gridSize[1]=sly;
	gridSize[2]=slz;
	gridPtrBlocks = new Catoms3DBlock*[slx*sly*slz];

	// initialise grid of blocks
	int i=slx*sly*slz;
	Catoms3DBlock **ptr = gridPtrBlocks;
	while (i--) {
		*ptr=NULL;
		ptr++;
	}
	targetGrid=NULL;

	GlutContext::init(argc,argv);
	idTextureHexa=0;
	idTextureLines=0;
	blockSize[0]=1.0;
	blockSize[1]=5.0;
	blockSize[2]=1.0;
	objBlock = new ObjLoader::ObjLoader("../../simulatorCore/catoms3DTextures","catom3D.obj");
	objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/catoms3DTextures","catom3D_picking.obj");
	objRepere = new ObjLoader::ObjLoader("../../simulatorCore/catoms3DTextures","repere25.obj");
	camera = new Camera(-M_PI/2.0,M_PI/3.0,750.0);
	camera->setLightParameters(Vecteur(0,0,0),45.0,80.0,800.0,45.0,10.0,1500.0);
	camera->setTarget(Vecteur(0,0,1.0));

	menuId=0;
	numSelectedFace=0;
	numSelectedBlock=0;
}

Catoms3DWorld::~Catoms3DWorld() {
	OUTPUT << "Catoms3DWorld destructor" << endl;
	/*	block linked are deleted by world::~world() */
	delete [] gridPtrBlocks;
	delete [] targetGrid;
	delete objBlock;
	delete objBlockForPicking;
	delete objRepere;
	delete camera;
}

void Catoms3DWorld::createWorld(int slx,int sly,int slz, int argc, char *argv[]) {
	world = new Catoms3DWorld(slx,sly,slz,argc,argv);
}

void Catoms3DWorld::deleteWorld() {
	delete((Catoms3DWorld*)world);
}

void Catoms3DWorld::addBlock(int blockId, Catoms3DBlockCode *(*catomCodeBuildingFunction)(Catoms3DBlock*),const Cell3DPosition &pos,const Color &color,bool master) {

	if (blockId == -1) {
		map<int, BaseSimulator::BuildingBlock*>::iterator it;
			for(it = buildingBlocksMap.begin();
					it != buildingBlocksMap.end(); it++) {
				Catoms3DBlock* bb = (Catoms3DBlock*) it->second;
				if (it->second->blockId > blockId) blockId = bb->blockId;
			}
		blockId++;
	}
	Catoms3DBlock *catom = new Catoms3DBlock(blockId,catomCodeBuildingFunction);
	buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>(catom->blockId, (BaseSimulator::BuildingBlock*)catom));

	getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), catom));

	Catoms3DGlBlock *glBlock = new Catoms3DGlBlock(blockId);
	tabGlBlocks.push_back(glBlock);
	catom->setGlBlock(glBlock);
	catom->setPosition(pos);
	catom->setColor(color);
/*
********************************************************************
A ECRIRE AVEC LE MAILLAGE HEXAGONAL
*/
	int ix,iy,iz;
	ix = int(catom->position.pt[0]);
	iy = int(catom->position.pt[1]);
	iz = int(catom->position.pt[2]);
	if (ix>=0 && ix<gridSize[0] &&
		iy>=0 && iy<gridSize[1] &&
		iz>=0 && iz<gridSize[2]) {
		setGridPtr(ix,iy,iz,catom);
	} else {
		ERRPUT << "ERROR : BLOCK #" << blockId << " out of the grid !!!!!" << endl;
		exit(1);
	}
}

void Catoms3DWorld::connectBlock(Catoms3DBlock *block) {
    int ix,iy,iz;

    ix = int(block->position.pt[0]);
	iy = int(block->position.pt[1]);
	iz = int(block->position.pt[2]);
	setGridPtr(ix,iy,iz,block);
	OUTPUT << "Reconnection " << block->blockId << " pos ="<< ix << "," << iy << "," << iz << endl;
	linkBlock(ix,iy,iz);

	if (ix<gridSize[0]-1) linkBlock(ix+1,iy,iz);
	if (ix>0) linkBlock(ix-1,iy,iz);

	if (iz<gridSize[2]-1) linkBlock(ix,iy,iz+1);
	if (iz>0) linkBlock(ix,iy,iz-1);

	if (iz%2 == 1) {
		if (ix<gridSize[0]-1) {
			if (iz<gridSize[2]-1) linkBlock(ix+1,iy,iz+1);
			if (iz>0) linkBlock(ix+1,iy,iz-1);
		}
	} else {
		if (ix>0) {
			// x-1
			if (iz<gridSize[2]-1) linkBlock(ix-1,iy,iz+1);
			if (iz>0) linkBlock(ix-1,iy,iz-1);
		}
	}

}

void Catoms3DWorld::disconnectBlock(Catoms3DBlock *block) {
    P2PNetworkInterface *fromBlock,*toBlock;

    for(int i=0; i<12; i++) {
        fromBlock = block->getInterface(i);
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

void Catoms3DWorld::linkBlock(int ix, int iy, int iz) {
    Catoms3DBlock *ptrBlock = getGridPtr(ix,iy,iz);
/*
	if (ptrBlock) {
		OUTPUT << "link block " << ptrBlock->blockId << endl;

		OUTPUT << "X AXIS" << endl;
		if (ix<gridSize[0]-1 && getGridPtr(ix+1,iy,iz)) {
			(ptrBlock)->getInterface(NeighborDirection::Right)->connect(getGridPtr(ix+1,iy,iz)->getInterface(NeighborDirection::Left));
			OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix+1,iy,iz)->blockId << endl;
		} else {
            (ptrBlock)->getInterface(NeighborDirection::Right)->connect(NULL);
        }
		if (ix>0 && getGridPtr(ix-1,iy,iz)) {
			(ptrBlock)->getInterface(NeighborDirection::Left)->connect(getGridPtr(ix-1,iy,iz)->getInterface(NeighborDirection::Right));
			OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix-1,iy,iz)->blockId << endl;
		} else {
			(ptrBlock)->getInterface(NeighborDirection::Left)->connect(NULL);
		}

		OUTPUT << "Y AXIS - 1" << endl;
		if (iz%2 == 1) {
			if (iz<gridSize[2]-1 && getGridPtr(ix,iy,iz+1)) {
				(ptrBlock)->getInterface(NeighborDirection::TopLeft)->connect(getGridPtr(ix,iy,iz+1)->getInterface(NeighborDirection::BottomRight));
				OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix,iy,iz+1)->blockId << endl;
			} else {
				(ptrBlock)->getInterface(NeighborDirection::TopLeft)->connect(NULL);
			}
			if (iz>0 && getGridPtr(ix,iy,iz-1)) {
				(ptrBlock)->getInterface(NeighborDirection::BottomLeft)->connect(getGridPtr(ix,iy,iz-1)->getInterface(NeighborDirection::TopRight));
				OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix,iy,iz-1)->blockId << endl;
			} else {
				(ptrBlock)->getInterface(NeighborDirection::BottomLeft)->connect(NULL);
			}
		} else {
			if (iz<gridSize[2]-1 && getGridPtr(ix,iy,iz+1)) {
				(ptrBlock)->getInterface(NeighborDirection::TopRight)->connect(getGridPtr(ix,iy,iz+1)->getInterface(NeighborDirection::BottomLeft));
				OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix,iy,iz+1)->blockId << endl;
			} else {
				(ptrBlock)->getInterface(NeighborDirection::TopRight)->connect(NULL);
			}
			if (iz>0 && getGridPtr(ix,iy,iz-1)) {
				(ptrBlock)->getInterface(NeighborDirection::BottomRight)->connect(getGridPtr(ix,iy,iz-1)->getInterface(NeighborDirection::TopLeft));
				OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix,iy,iz-1)->blockId << endl;
			} else {
				(ptrBlock)->getInterface(NeighborDirection::BottomRight)->connect(NULL);
			}
		}


		OUTPUT << "Y AXIS - 2" << endl;
		if (iz%2 == 1) {
			if (ix<gridSize[0]-1) {
				// x+1
				if (iz<gridSize[2]-1 && getGridPtr(ix+1,iy,iz+1)) {
					(ptrBlock)->getInterface(NeighborDirection::TopRight)->connect(getGridPtr(ix+1,iy,iz+1)->getInterface(NeighborDirection::BottomLeft));
					OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix+1,iy,iz+1)->blockId << endl;
				} else {
					(ptrBlock)->getInterface(NeighborDirection::TopRight)->connect(NULL);
				}

				if (iz>0 && getGridPtr(ix+1,iy,iz-1)) {
					(ptrBlock)->getInterface(NeighborDirection::BottomRight)->connect(getGridPtr(ix+1,iy,iz-1)->getInterface(NeighborDirection::TopLeft));
					OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix+1,iy,iz-1)->blockId << endl;
				} else {
					(ptrBlock)->getInterface(NeighborDirection::BottomRight)->connect(NULL);
				}
			}
		} else {
				if (ix>0) {
				// x-1
				if (iz<gridSize[2]-1 && getGridPtr(ix-1,iy,iz+1)) {
					(ptrBlock)->getInterface(NeighborDirection::TopLeft)->connect(getGridPtr(ix-1,iy,iz+1)->getInterface(NeighborDirection::BottomRight));
					OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix-1,iy,iz+1)->blockId << endl;
				} else {
					(ptrBlock)->getInterface(NeighborDirection::TopLeft)->connect(NULL);
				}

				if (iz>0 && getGridPtr(ix-1,iy,iz-1)) {
					(ptrBlock)->getInterface(NeighborDirection::BottomLeft)->connect(getGridPtr(ix-1,iy,iz-1)->getInterface(NeighborDirection::TopRight));
					OUTPUT << "connection #" << (ptrBlock)->blockId << " to #" << getGridPtr(ix-1,iy,iz-1)->blockId << endl;
				} else {
					(ptrBlock)->getInterface(NeighborDirection::BottomLeft)->connect(NULL);
				}
			}
		}
	}*/
}

void Catoms3DWorld::linkBlocks() {
	int ix,iy,iz;
	for (iz=0; iz<gridSize[2]; iz++) {
		for (iy=0; iy<gridSize[1]; iy++) {
			for(ix=0; ix<gridSize[0]; ix++) {
				linkBlock(ix,iy,iz);
			}
		}
	}
}

void Catoms3DWorld::deleteBlock(Catoms3DBlock *bb) {
	if (bb->getState() >= Catoms3DBlock::ALIVE ) {
		// cut links between bb and others
		for(int i=0; i<12; i++) {
			P2PNetworkInterface *bbi = bb->getInterface(i);
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

void Catoms3DWorld::glDraw() {

	glPushMatrix();
	glTranslatef(0.5*blockSize[0],0,0.5*blockSize[2]);
	glDisable(GL_TEXTURE_2D);
	vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
	lock();
	while (ic!=tabGlBlocks.end()) {
		((Catoms3DGlBlock*)(*ic))->glDraw(objBlock);
	   	ic++;
	}
	unlock();
	glPopMatrix();


	static const GLfloat white[]={1.0,1.0,1.0,1.0},
    gray[]={0.6,0.6,0.6,1.0};
    glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
    glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
    glMaterialfv(GL_FRONT,GL_SPECULAR,gray);
    glMaterialf(GL_FRONT,GL_SHININESS,40.0);
    glPushMatrix();
    enableTexture(true);
    glBindTexture(GL_TEXTURE_2D,idTextureLines);
    glScalef(gridSize[0]*blockSize[0],gridSize[1]*blockSize[1],gridSize[2]*blockSize[2]*M_SQRT3_2);
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
        glNormal3f(1.0f,0,0);
        glTexCoord2f(0,0);
        glVertex3f(0.0f,0.0f,0.0f);
        glTexCoord2f(0,1.0f);
        glVertex3f(0.0f,1.0f,0.0f);
        glTexCoord2f(1.0f,1.0f);
        glVertex3f(0.0,1.0,1.0f);
        glTexCoord2f(1.0f,0.0);
        glVertex3f(0.0,0.0,1.0f);
    // right
        glNormal3f(-1.0f,0,0);
        glTexCoord2f(0,0);
        glVertex3f(1.0f,0.0f,0.0f);
        glTexCoord2f(1.0f,0.0f);
        glVertex3f(1.0,0.0,1.0f);
        glTexCoord2f(1.0f,1.0f);
        glVertex3f(1.0,1.0,1.0f);
        glTexCoord2f(0,1.0f);
        glVertex3f(1.0f,1.0f,0.0f);
    glEnd();
    glPopMatrix();
	// draw hexa
	glPushMatrix();
    glBindTexture(GL_TEXTURE_2D,idTextureHexa);
    glScalef(gridSize[0]*blockSize[0],gridSize[1]*blockSize[1],gridSize[2]*blockSize[2]*M_SQRT3_2);
    glBegin(GL_QUADS);

    // back
        glNormal3f(0,-1.0,0);
        glTexCoord2f(0,0);
        glVertex3f(0.0f,1.0f,0.0f);
        glTexCoord2f(gridSize[0]/3.0f,0);
        glVertex3f(1.0f,1.0f,0.0f);
        glTexCoord2f(gridSize[0]/3.0f,gridSize[2]/2.0f);
        glVertex3f(1.0f,1.0,1.0f);
        glTexCoord2f(0,gridSize[2]/2.0f);
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
		glScalef(0.5f,0.5f,0.5f);
		objRepere->glDraw();
	glPopMatrix();
}

void Catoms3DWorld::glDrawId() {
	glPushMatrix();
	glTranslatef(0.5*blockSize[0],0.5*blockSize[1],0.5*blockSize[2]);
	glDisable(GL_TEXTURE_2D);
	vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
	int n=1;
	lock();
	while (ic!=tabGlBlocks.end()) {
		((Catoms3DGlBlock*)(*ic))->glDrawId(objBlock,n);
	   	ic++;
	}
	unlock();
	glPopMatrix();
}

void Catoms3DWorld::glDrawIdByMaterial() {
	glPushMatrix();
	glTranslatef(0.5*blockSize[0],0.5*blockSize[1],0.5*blockSize[2]);

	glDisable(GL_TEXTURE_2D);
	vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
	int n=1;
	lock();
	while (ic!=tabGlBlocks.end()) {
		((Catoms3DGlBlock*)(*ic))->glDrawIdByMaterial(objBlockForPicking,n);
	   	ic++;
	}
	unlock();
	glPopMatrix();
}


void Catoms3DWorld::loadTextures(const string &str) {
	string path = str+"//hexa.tga";
	int lx,ly;
	idTextureHexa = GlutWindow::loadTexture(path.c_str(),lx,ly);
	path = str+"//lignes.tga";
	idTextureLines = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void Catoms3DWorld::updateGlData(Catoms3DBlock*blc) {
	//cout << "update posgrid:" << blc->position << endl;
	updateGlData(blc,gridToWorldPosition(blc->position));
}

void Catoms3DWorld::updateGlData(Catoms3DBlock*blc, const Vecteur &position) {
	Catoms3DGlBlock *glblc = blc->getGlBlock();
	if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setPosition(position);
		glblc->setColor(blc->color);
		unlock();
	}
}

void Catoms3DWorld::updateGlData(Catoms3DBlock*blc, const Vecteur &position, double angle) {
	Catoms3DGlBlock *glblc = blc->getGlBlock();
	if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setAngle(angle);
		glblc->setPosition(position);
		glblc->setColor(blc->color);
		unlock();
	}
}

Cell3DPosition Catoms3DWorld::worldToGridPosition(Vecteur &pos) {
	Cell3DPosition res;

/*
	res.pt[2] = pos[2] / (M_SQRT3_2 * blockSize[2]);
	res.pt[1] = 0;
	res.pt[0] = (int) (pos[0]/blockSize[0] - ((int)res.pt[2]%2)*0.5);
*/
	return res;
}

Vecteur Catoms3DWorld::gridToWorldPosition(Cell3DPosition &pos) {
	Vecteur res;

    res.pt[2] = M_SQRT2_2*pos[2]*blockSize[2];
    if (pos[2]%2) {
        res.pt[1] = (pos[1]+0.5)*blockSize[1];
        res.pt[0] = (pos[0]+0.5)*blockSize[0];
    } else {
        res.pt[1] = pos[1]*blockSize[1];
        res.pt[0] = pos[0]*blockSize[0];
    }
    return res;
}

void Catoms3DWorld::createPopupMenu(int ix, int iy) {
	if (!GlutContext::popupMenu) {
		GlutContext::popupMenu = new GlutPopupMenuWindow(NULL,0,0,200,180);
		GlutContext::popupMenu->addButton(1,"../../simulatorCore/catoms3DTextures/menu_add.tga");
		GlutContext::popupMenu->addButton(2,"../../simulatorCore/catoms3DTextures/menu_del.tga");
		GlutContext::popupMenu->addButton(3,"../../simulatorCore/catoms3DTextures/menu_stop.tga");
		GlutContext::popupMenu->addButton(4,"../../simulatorCore/catoms3DTextures/menu_save.tga");
		GlutContext::popupMenu->addButton(5,"../../simulatorCore/catoms3DTextures/menu_cancel.tga");
	}
	if (iy<GlutContext::popupMenu->h) iy=GlutContext::popupMenu->h;
	// verify if add is possible for this face
	Catoms3DBlock *bb = (Catoms3DBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
	bool valid=true;
/*	switch (numSelectedFace) {
		//BottomLeft = 0, Left = 1, TopLeft = 2, BottomRight = 3, Right = 4, TopRight =5}
		case NeighborDirection::Left :
			valid=(bb->position[0]>0 && getGridPtr(int(bb->position[0])-1,int(bb->position[1]),int(bb->position[2]))==NULL);
			break;
		case NeighborDirection::Right :
			valid=(bb->position[0]<gridSize[0]-1 && getGridPtr(int(bb->position[0])+1,int(bb->position[1]),int(bb->position[2]))==NULL);
		break;
		case NeighborDirection::BottomLeft :
			valid=(bb->position[2]>0 && getGridPtr(int(bb->position[0]),int(bb->position[1]),int(bb->position[2])-1)==NULL);
			break;
		case NeighborDirection::TopLeft :
			valid=(bb->position[2]<gridSize[2]-1 && getGridPtr(int(bb->position[0]),int(bb->position[1]),int(bb->position[2])+1)==NULL);
		break;
		case NeighborDirection::BottomRight :
			valid=(bb->position[0]<gridSize[0]-1 && bb->position[2]>0 && getGridPtr(int(bb->position[0]+1),int(bb->position[1]),int(bb->position[2])-1)==NULL);
			break;
		case NeighborDirection::TopRight :
			valid=(bb->position[0]<gridSize[0]-1 && bb->position[2]<gridSize[2]-1 && getGridPtr(int(bb->position[0])+1,int(bb->position[1]),int(bb->position[2])+1)==NULL);
		break;
	}
	GlutContext::popupMenu->activate(1,valid);*/
	GlutContext::popupMenu->setCenterPosition(ix,GlutContext::screenHeight-iy);
	GlutContext::popupMenu->show(true);
}

void Catoms3DWorld::menuChoice(int n) {
	switch (n) {
		case 1 : {
			Catoms3DBlock *bb = (Catoms3DBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
			OUTPUT << "ADD block link to : " << bb->blockId << "     num Face : " << numSelectedFace << endl;
			/*Vecteur pos=bb->position;
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
			linkBlocks();*/
		} break;
		case 2 : {
			OUTPUT << "DEL num block : " << tabGlBlocks[numSelectedBlock]->blockId << endl;
			Catoms3DBlock *bb = (Catoms3DBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
			deleteBlock(bb);
		} break;
	}
}

void Catoms3DWorld::setSelectedFace(int n) {
	numSelectedBlock=n/6;
	string name = objBlockForPicking->getObjMtlName(n%6);
	numSelectedFace=0;
	/*
    if (name=="face_top") numSelectedFace=NeighborDirection::Top;
    else if (name=="face_bottom") numSelectedFace=NeighborDirection::Bottom;
    else if (name=="face_right") numSelectedFace=NeighborDirection::Right;
    else if (name=="face_left") numSelectedFace=NeighborDirection::Left;
    else if (name=="face_front") numSelectedFace=NeighborDirection::Front;
    else if (name=="face_back") numSelectedFace=NeighborDirection::Back;
	*/
}

void Catoms3DWorld::createHelpWindow() {
	if (GlutContext::helpWindow)
		delete GlutContext::helpWindow;
	GlutContext::helpWindow = new GlutHelpWindow(NULL,10,40,540,500,"../../simulatorCore/catoms3DHelp.txt");
}

void Catoms3DWorld::getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm) {
    presence *gpm=pm.grid;
    Catoms3DBlock **grb;

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

void Catoms3DWorld::initTargetGrid() {
    if (targetGrid) delete [] targetGrid;
    int sz = gridSize[0]*gridSize[1]*gridSize[2];
    targetGrid = new presence[gridSize[0]*gridSize[1]*gridSize[2]];
    memset(targetGrid,emptyCell,sz*sizeof(presence));
}

} // RobotBlock namespace

