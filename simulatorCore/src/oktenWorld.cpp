/*!
 * \file oktenWorld.cpp
 * \brief okten world
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#include <iostream>
#include <string>
#include <stdlib.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

#include "oktenWorld.h"
#include "oktenBlock.h"
#include "trace.h"
#include "configExporter.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace Okten
namespace Okten {

/**
   \brief Constructor
   \param gridSize : size of the grid
   \param gridScale : size of a block
   \param argc : number of execution parameters
   \param argv : string array of parameters
*/
OktenWorld::OktenWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
							 int argc, char *argv[]):World(argc, argv) {
    OUTPUT << "\033[1;31mOktenWorld constructor\033[0m" << endl;

    if (GlutContext::GUIisEnabled) {
		objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/oktenTextures","oktenModule.obj");
		objConnector = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/oktenTextures","oktenConnector.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/oktenTextures","oktenModule.obj");
		objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures","repere25.obj");
	}

    lattice = new SCLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
}

OktenWorld::~OktenWorld() {
    OUTPUT << "OktenWorld destructor" << endl;
    /*	block linked are deleted by world::~world() */
}

void OktenWorld::deleteWorld() {
    delete((OktenWorld*)world);
}

void OktenWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
							 short orientation, bool master) {
	if (blockId > maxBlockId)
		maxBlockId = blockId;
	else if (blockId == 0)
		blockId = incrementBlockId();

    OktenBlock *module = new OktenBlock(blockId,bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
							 (module->blockId, (BaseSimulator::BuildingBlock*)module));

    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), module));

    OktenGlBlock *glBlock = new OktenGlBlock(blockId);
    tabGlBlocks.push_back(glBlock);

    module->setGlBlock(glBlock);
    module->setPositionAndOrientation(pos,orientation);
    module->setColor(col);
    lattice->insert(module, pos);
    glBlock->setPosition(lattice->gridToWorldPosition(pos));
    OUTPUT << "ADD #" << blockId << ", "<< pos << endl;
}

/**
 * \brief Connect the block placed on the cell at position pos
 */

 /* revoir avec robotblock*/
void OktenWorld::linkBlock(const Cell3DPosition& pos) {
    OktenBlock *module = (OktenBlock *)lattice->getBlock(pos);
	OktenBlock* neighborBlock;
	vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
	Cell3DPosition nPos;

	OUTPUT << "pos:" << pos << "  #" << module->blockId << endl;

	// Check neighbors for each interface
	for (int i = 0; i < 6; i++) {
		nPos = pos + nRelCells[i];
		OUTPUT << "npos:" << nPos << "  i=" << i << endl;
		neighborBlock = (OktenBlock*)lattice->getBlock(nPos);
		if (neighborBlock) {
			module->getInterface(SCLattice::Direction(i))->connect(neighborBlock->getInterface(SCLattice::Direction(lattice->getOppositeDirection(i))));

			OUTPUT << "connection #" << module->blockId << ":" << lattice->getDirectionString(i) <<
				" to #" << neighborBlock->blockId << ":"
				   << lattice->getDirectionString(lattice->getOppositeDirection(i)) << endl;

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
void OktenWorld::glDraw() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
	glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
// draw modules
    vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
    OktenGlBlock *ptr;
    lock();
    while (ic!=tabGlBlocks.end()) {
        ptr = (OktenGlBlock*)(*ic);
		ptr->glDraw(objBlock);
		ptr->glDrawConnectors(objConnector);
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

void OktenWorld::glDrawId() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
   	glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
    vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
    int n=1;
    lock();
    while (ic!=tabGlBlocks.end()) {
		((OktenGlBlock*)(*ic))->glDrawId(objBlock,n);
		ic++;
    }
    unlock();
    glPopMatrix();
}

void OktenWorld::glDrawIdByMaterial() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
	glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
    vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
    int n=1;
    lock();
    while (ic!=tabGlBlocks.end()) {
		((OktenGlBlock*)(*ic))->glDrawIdByMaterial(objBlockForPicking,n);
		ic++;
    }
    unlock();
    glPopMatrix();
}


void OktenWorld::loadTextures(const string &str) {
	string path = str+"/texture_plane.tga";
	int lx,ly;
	idTextureWall = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void OktenWorld::updateGlData(BuildingBlock *bb) {
    OktenGlBlock *glblc = (OktenGlBlock*)bb->getGlBlock();
    if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setPosition(lattice->gridToWorldPosition(bb->position));
		glblc->setColor(bb->color);
		unlock();
    }
}

void OktenWorld::updateGlData(OktenBlock*blc, const Color &color) {
    OktenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setColor(color);
		unlock();
    }
}

void OktenWorld::updateGlData(OktenBlock*blc, bool visible) {
    OktenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setVisible(visible);
		unlock();
    }
}

void OktenWorld::updateGlData(OktenBlock*blc, const Vector3D &position) {
    OktenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setPosition(position);
		unlock();
    }
}

void OktenWorld::updateGlData(OktenBlock*blc, const Cell3DPosition &position) {
    OktenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setPosition(lattice->gridToWorldPosition(position));
		unlock();
    }
}

void OktenWorld::updateGlData(OktenBlock*blc, const Matrix &mat) {
    OktenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
		lock();
		glblc->mat = mat;
		unlock();
    }
}

void OktenWorld::updateGlData(OktenBlock*blc, short id, float length) {
    OktenGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
		lock();
		glblc->tabPosConnectors[id] = (uint8_t)(length*255.0);
		OUTPUT << "#" << blc->blockId << ":" << id << "=" << (int)glblc->tabPosConnectors[id] << endl;
		unlock();
    }
}

void OktenWorld::setSelectedFace(int n) {
    numSelectedGlBlock=n/6;
	string name = objBlockForPicking->getObjMtlName(n%6);
	if (name=="face_top") numSelectedFace=SCLattice::Top;
	else if (name=="face_bottom") numSelectedFace=SCLattice::Bottom;
	else if (name=="face_right") numSelectedFace=SCLattice::Right;
	else if (name=="face_left") numSelectedFace=SCLattice::Left;
	else if (name=="face_front") numSelectedFace=SCLattice::Front;
	else if (name=="face_back") numSelectedFace=SCLattice::Back;
	cerr << name << " = " << numSelectedFace << " = " << endl;
}

void OktenWorld::exportConfiguration() {
	OktenConfigExporter exporter = OktenConfigExporter(this);
	exporter.exportConfiguration();
}

/*
  void OktenWorld::getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm) {
  presence *gpm=pm.grid;
  OktenBlock **grb;

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

  void OktenWorld::initTargetGrid() {
  if (targetGrid) delete [] targetGrid;
  int sz = lattice->gridSize[0]*lattice->gridSize[1]*lattice->gridSize[2];
  targetGrid = new presence[lattice->gridSize[0]*lattice->gridSize[1]*lattice->gridSize[2]];
  memset(targetGrid,emptyCell,sz*sizeof(presence));
  }
*/
} // RobotBlock namespace
