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
#include "trace.h"
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace Catoms3D
namespace Catoms3D {

/**
   \brief Constructor
   \param gridSize : size of the grid
   \param gridScale : size of a block
   \param argc : number of execution parameters
   \param argv : string array of parameters
*/
Catoms3DWorld::Catoms3DWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
			     int argc, char *argv[]):World(argc, argv) {
    OUTPUT << "\033[1;31mCatoms3DWorld constructor\033[0m" << endl;

    idTextureHexa=0;
    idTextureGrid=0;
    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/catoms3DTextures","catom3DV2connectorID.obj");
    objBlockForPicking =
		new ObjLoader::ObjLoader("../../simulatorCore/catoms3DTextures","catom3D_picking.obj");
    objRepere = new ObjLoader::ObjLoader("../../simulatorCore/catoms3DTextures","repereCatom3D.obj");

    skeleton=NULL;

    lattice = new FCCLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
}

Catoms3DWorld::~Catoms3DWorld() {
    OUTPUT << "Catoms3DWorld destructor" << endl;
    /*	block linked are deleted by world::~world() */
    //delete [] targetGrid;
    delete objBlock;
    delete objBlockForPicking;
    delete objRepere;
}

void Catoms3DWorld::deleteWorld() {
    delete((Catoms3DWorld*)world);
}

void Catoms3DWorld::addBlock(int blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
							 short orientation, bool master) {
    // if blockID==-1, search the maximum id
    if (blockId == -1) {
	map<int, BaseSimulator::BuildingBlock*>::iterator it;
	for(it = buildingBlocksMap.begin();	it != buildingBlocksMap.end(); it++) {
	    Catoms3DBlock* bb = (Catoms3DBlock*) it->second;
	    if (it->second->blockId > blockId) blockId = bb->blockId;
	}
	blockId++;
    }

    Catoms3DBlock *catom = new Catoms3DBlock(blockId,bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
			     (catom->blockId, (BaseSimulator::BuildingBlock*)catom));

    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), catom));

    Catoms3DGlBlock *glBlock = new Catoms3DGlBlock(blockId);
    tabGlBlocks.push_back(glBlock);

    catom->setGlBlock(glBlock);
    catom->setPositionAndOrientation(pos,orientation);
    catom->setColor(col);
    lattice->insert(catom, pos);
    glBlock->setPosition(lattice->gridToWorldPosition(pos));
}

/**
   \brief Connect a block to its neighbors after a motion
*/
void Catoms3DWorld::connectBlock(Catoms3DBlock *block) {
    Cell3DPosition pos = block->position;
    lattice->insert(block, pos);
    OUTPUT << "Reconnection " << block->blockId << " pos ="<< pos << endl;
    linkBlock(pos);
    linkNeighbors(pos);
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

    lattice->remove(block->position);
    OUTPUT << getScheduler()->now() << " : Disconnection " << block->blockId << " pos = "
	   << block->position << endl;
}

/**
 * \brief Connect the block placed on the cell at position pos
 */
void Catoms3DWorld::linkBlock(const Cell3DPosition& pos) {
    Catoms3DBlock *catom = (Catoms3DBlock *)lattice->getBlock(pos);

    if (catom) {
	OUTPUT << "link catom " << catom->blockId << endl;

	Cell3DPosition neighborPos;
	Catoms3DBlock* neighborBlock;

	for (int i=0; i<12; i++) {
	    if (catom->getNeighborPos(i,neighborPos)
		&& (neighborBlock = (Catoms3DBlock *)lattice->getBlock(neighborPos))!=NULL) {
		catom->getInterface(i)->connect(neighborBlock->getInterface(pos));
		OUTPUT << "connection #" << catom->blockId << "(" << i << ") to #"
		       << neighborBlock->blockId << endl;
	    }
	}
    }
}

void Catoms3DWorld::deleteBlock(BuildingBlock *blc) {
	Catoms3DBlock *bb = (Catoms3DBlock *)blc;

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
	lattice->remove(bb->position);

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

/**
 * \brief Draw catoms and axes
 */
void Catoms3DWorld::glDraw() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
// draw catoms
    vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
    lock();
    while (ic!=tabGlBlocks.end()) {
		((Catoms3DGlBlock*)(*ic))->glDraw(objBlock);
		ic++;
    }
    unlock();
    glPopMatrix();

// material for the grid walls
	static const GLfloat white[]={0.8f,0.8f,0.8f,1.0f},
	gray[]={0.2f,0.2f,0.2f,1.0f};
	glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
	glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
	glMaterialfv(GL_FRONT,GL_SPECULAR,white);
	glMaterialf(GL_FRONT,GL_SHININESS,40.0);
	glPushMatrix();
	enableTexture(true);
	glBindTexture(GL_TEXTURE_2D,idTextureGrid);
	glTranslatef(0,0,lattice->gridScale[2]*(0.5-M_SQRT2_2));
	glScalef(lattice->gridSize[0]*lattice->gridScale[0],lattice->gridSize[1]*lattice->gridScale[1],lattice->gridSize[2]*lattice->gridScale[2]*M_SQRT2_2);
	glBegin(GL_QUADS);
	// bottom
	glNormal3f(0,0,1.0f);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,0.0f,-0.0f);
	glTexCoord2f(0.5f*lattice->gridSize[0],0);
	glVertex3f(1.0f,0.0f,0.0f);
	glTexCoord2f(0.5f*lattice->gridSize[0],0.5f*lattice->gridSize[1]);
	glVertex3f(1.0,1.0,0.0f);
	glTexCoord2f(0,0.5f*lattice->gridSize[1]);
	glVertex3f(0.0,1.0,0.0f);
	// top
	glNormal3f(0,0,-1.0f);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,0.0f,1.0f);
	glTexCoord2f(0.5f*lattice->gridSize[0],0);
	glVertex3f(0.0,1.0,1.0f);
	glTexCoord2f(0.5f*lattice->gridSize[0],0.5f*lattice->gridSize[1]);
	glVertex3f(1.0,1.0,1.0f);
	glTexCoord2f(0,0.5f*lattice->gridSize[1]);
	glVertex3f(1.0f,0.0f,1.0f);
	glEnd();
	// draw hexa
	glBindTexture(GL_TEXTURE_2D,idTextureHexa);
	glBegin(GL_QUADS);
	// left
	glNormal3f(1.0f,0,0);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,0.0f,0.0f);
	glTexCoord2f(lattice->gridSize[1]/3.0f,0);
	glVertex3f(0.0f,1.0f,0.0f);
	glTexCoord2f(lattice->gridSize[1]/3.0f,lattice->gridSize[2]/1.5f);
	glVertex3f(0.0,1.0,1.0f);
	glTexCoord2f(0,lattice->gridSize[2]/1.5f);
	glVertex3f(0.0,0.0,1.0f);
	// right
	glNormal3f(-1.0f,0,0);
	glTexCoord2f(0,0);
	glVertex3f(1.0f,0.0f,0.0f);
	glTexCoord2f(0,lattice->gridSize[2]/1.5f);
	glVertex3f(1.0,0.0,1.0f);
	glTexCoord2f(lattice->gridSize[1]/3.0f,lattice->gridSize[2]/1.5f);
	glVertex3f(1.0,1.0,1.0f);
	glTexCoord2f(lattice->gridSize[1]/3.0f,0);
	glVertex3f(1.0f,1.0f,0.0f);
	// back
	glNormal3f(0,-1.0f,0);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,1.0f,0.0f);
	glTexCoord2f(lattice->gridSize[0]/3.0f,0);
	glVertex3f(1.0f,1.0f,0.0f);
	glTexCoord2f(lattice->gridSize[0]/3.0f,lattice->gridSize[2]/1.5f);
	glVertex3f(1.0f,1.0,1.0f);
	glTexCoord2f(0,lattice->gridSize[2]/1.5f);
	glVertex3f(0.0,1.0,1.0f);
	// front
	glNormal3f(0,1.0,0);
	glTexCoord2f(0,0);
	glVertex3f(0.0f,0.0f,0.0f);
	glTexCoord2f(0,lattice->gridSize[2]/1.5f);
	glVertex3f(0.0,0.0,1.0f);
	glTexCoord2f(lattice->gridSize[0]/3.0f,lattice->gridSize[2]/1.5f);
	glVertex3f(1.0f,0.0,1.0f);
	glTexCoord2f(lattice->gridSize[0]/3.0f,0);
	glVertex3f(1.0f,0.0f,0.0f);
	glEnd();
	glPopMatrix();
	// draw the axes
	glPushMatrix();
	objRepere->glDraw();
	glPopMatrix();
}

void Catoms3DWorld::glDrawId() {
    glPushMatrix();
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
    path = str+"//textureCarre.tga";
    idTextureGrid = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void Catoms3DWorld::updateGlData(BuildingBlock *bb) {
    Catoms3DGlBlock *glblc = (Catoms3DGlBlock*)bb->getGlBlock();
    if (glblc) {
	lock();
	//cout << "update pos:" << position << endl;
	glblc->setPosition(lattice->gridToWorldPosition(bb->position));
	glblc->setColor(bb->color);
	unlock();
    }
}

void Catoms3DWorld::updateGlData(Catoms3DBlock*blc, const Color &color) {
    Catoms3DGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
	lock();
	//cout << "update pos:" << position << endl;
	glblc->setColor(color);
	unlock();
    }
}

void Catoms3DWorld::updateGlData(Catoms3DBlock*blc, bool visible) {
    Catoms3DGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
	lock();
	//cout << "update pos:" << position << endl;
	glblc->setVisible(visible);
	unlock();
    }
}

void Catoms3DWorld::updateGlData(Catoms3DBlock*blc, const Vector3D &position) {
    Catoms3DGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
	lock();
	//cout << "update pos:" << position << endl;
	glblc->setPosition(position);
	unlock();
    }
}

void Catoms3DWorld::updateGlData(Catoms3DBlock*blc, const Cell3DPosition &position) {
    Catoms3DGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
	lock();
	//cout << "update pos:" << position << endl;
	glblc->setPosition(lattice->gridToWorldPosition(position));
	unlock();
    }
}

void Catoms3DWorld::updateGlData(Catoms3DBlock*blc, const Matrix &mat) {
    Catoms3DGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
	lock();
	glblc->mat = mat;
	unlock();
    }
}


void Catoms3DWorld::menuChoice(int n) {
    switch (n) {
    case 1 : {
	Catoms3DBlock *bb = (Catoms3DBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
	OUTPUT << "ADD block link to : " << bb->blockId << "     num Face : " << numSelectedFace << endl;
	/*Vector3D pos=bb->position;
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
	linkBlock(bb->position);
	linkNeighbors(bb->position);
    } break;
    case 3 : {
	Catoms3DBlock *bb = (Catoms3DBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
	tapBlock(getScheduler()->now(), bb->blockId);
    } break;
    case 4:                 // Save current configuration
	exportConfiguration();
	break;
    }
}

void Catoms3DWorld::setSelectedFace(int n) {
    numSelectedBlock=n/12;
    string name = objBlockForPicking->getObjMtlName(n%12);
    numSelectedFace=0;
    
	// if (name=="face_top") numSelectedFace=;
	// else if (name=="face_bottom") numSelectedFace=;
	// else if (name=="face_right") numSelectedFace=;
	// else if (name=="face_left") numSelectedFace=;
	// else if (name=="face_front") numSelectedFace=;
	// else if (name=="face_back") numSelectedFace=;
}

/*
  void Catoms3DWorld::getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm) {
  presence *gpm=pm.grid;
  Catoms3DBlock **grb;

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
  grb = gridPtrBlocks+(ix0+pos.x-1+(iy+pos.y-1+(iz+pos.z-1)*lattice->gridSize[1])*lattice->gridSize[0]);
  for (ix=ix0; ix<ix1; ix++) {
  *gpm++ = (*grb++)?fullCell:emptyCell;
  }
  }
  }
  }

  void Catoms3DWorld::initTargetGrid() {
  if (targetGrid) delete [] targetGrid;
  int sz = lattice->gridSize[0]*lattice->gridSize[1]*lattice->gridSize[2];
  targetGrid = new presence[lattice->gridSize[0]*lattice->gridSize[1]*lattice->gridSize[2]];
  memset(targetGrid,emptyCell,sz*sizeof(presence));
  }
*/
} // RobotBlock namespace
