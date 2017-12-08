/*!
 * \file catoms3DWorld.cpp
 * \brief catoms world
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#include <iostream>
#include <string>
#include <stdlib.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

#include "catoms3DWorld.h"
#include "catoms3DBlock.h"
#include "trace.h"
#include "configExporter.h"

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

    if (GlutContext::GUIisEnabled) {
		objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures",
											"catom3DV2connectorID.obj");
        /*objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures",
											"catom3Drepere3.obj");*/
		objBlockForPicking =
			new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures",
									 "catom3D_picking.obj");
		objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures","repereCatom3D.obj");
	}

    lattice = new FCCLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
    // motionRules = new Catoms3DMotionRules();
}

Catoms3DWorld::~Catoms3DWorld() {
    OUTPUT << "Catoms3DWorld destructor" << endl;
    /*	block linked are deleted by world::~world() */
    // delete motionRules;
}

void Catoms3DWorld::deleteWorld() {
    delete((Catoms3DWorld*)world);
}

void Catoms3DWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
							 short orientation, bool master) {
	if (blockId > maxBlockId)
		maxBlockId = blockId;
	else if (blockId == 0)
		blockId = incrementBlockId();

    Catoms3DBlock *catom = new Catoms3DBlock(blockId,bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
							 (catom->blockId, (BaseSimulator::BuildingBlock*)catom));

    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), catom));


    Catoms3DGlBlock *glBlock = new Catoms3DGlBlock(blockId);
    glBlock->setPosition(lattice->gridToWorldPosition(pos));

    catom->setGlBlock(glBlock);
    catom->setPositionAndOrientation(pos,orientation);
    catom->setColor(col);
    lattice->insert(catom, pos);

    tabGlBlocks.push_back(glBlock);
}

/**
 * \brief Connect the block placed on the cell at position pos
 */
void Catoms3DWorld::linkBlock(const Cell3DPosition& pos) {
    Catoms3DBlock *catom = (Catoms3DBlock *)lattice->getBlock(pos);

    OUTPUT << "linkBlock(" << pos << "):" << endl;
    if (catom) {
		OUTPUT << "link catom " << catom->blockId << endl;
        if (catom->blockId == 1)
            OUTPUT << "C'est ici! " << catom->blockId << endl;
        
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
    } else {
        OUTPUT << "ERROR : try to link a block in empty cell :" << pos << endl;
    }
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

        lattice->glDraw();

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

    glDrawBackground();
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

void Catoms3DWorld::glDrawSpecificBg() {
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

void Catoms3DWorld::setSelectedFace(int n) {
    numSelectedGlBlock = n / 13;
    string name = objBlockForPicking->getObjMtlName(n%13);

	if (name == "Material__66") numSelectedFace = 0;
	else if (name == "Material__68") numSelectedFace = 1;
	else if (name == "Material__71") numSelectedFace = 2;
	else if (name == "Material__72") numSelectedFace = 3;
	else if (name == "Material__73") numSelectedFace = 4;
	else if (name == "Material__74") numSelectedFace = 5;
	else if (name == "Material__75") numSelectedFace = 6;
	else if (name == "Material__76") numSelectedFace = 7;
	else if (name == "Material__77") numSelectedFace = 8;
	else if (name == "Material__78") numSelectedFace = 9;
	else if (name == "Material__69") numSelectedFace = 10;
	else if (name == "Material__70") numSelectedFace = 11;
	else {
		cerr << "warning: Unrecognized picking face" << endl;
		numSelectedFace = 13;	// UNDEFINED
	}

	cerr << name << " = " << numSelectedFace << " = " << endl;
}

void Catoms3DWorld::exportConfiguration() {
	Catoms3DConfigExporter exporter = Catoms3DConfigExporter(this);
	exporter.exportConfiguration();
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
