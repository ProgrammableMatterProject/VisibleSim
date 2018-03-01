/*!
 * \file datomsWorld.cpp
 * \brief datoms world
 * \date 28/01/2018
 * \author Benoît Piranda
 */

#include <iostream>
#include <string>
#include <stdlib.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

#include "datomsWorld.h"
#include "datomsBlock.h"
#include "trace.h"
#include "configExporter.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace Datoms
namespace Datoms {

/**
   \brief Constructor
   \param gridSize : size of the grid
   \param gridScale : size of a block
   \param argc : number of execution parameters
   \param argv : string array of parameters
*/
DatomsWorld::DatomsWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
							 int argc, char *argv[]):World(argc, argv) {
    OUTPUT << "\033[1;31mDatomsWorld constructor\033[0m" << endl;

    if (GlutContext::GUIisEnabled) {
		objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/datomsTextures",
											"datoms.obj");
		objBlockForPicking =
			new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/datomsTextures",
									 "datoms.obj");
		objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/datomsTextures","repereDatom.obj");
	}

    lattice = new FCCLattice2(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
	motionRules = new DatomsMotionRules();
}

DatomsWorld::~DatomsWorld() {
    OUTPUT << "DatomsWorld destructor" << endl;
    /*	block linked are deleted by world::~world() */
}

void DatomsWorld::deleteWorld() {
    delete((DatomsWorld*)world);
}

void DatomsWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
							 short orientation, bool master) {
	if (blockId > maxBlockId)
		maxBlockId = blockId;
	else if (blockId == 0)
		blockId = incrementBlockId();

    DatomsBlock *catom = new DatomsBlock(blockId,bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
							 (catom->blockId, (BaseSimulator::BuildingBlock*)catom));

    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), catom));

    DatomsGlBlock *glBlock = new DatomsGlBlock(blockId);
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
void DatomsWorld::linkBlock(const Cell3DPosition& pos) {
    DatomsBlock *datom = (DatomsBlock *)lattice->getBlock(pos);

    if (datom) {
		OUTPUT << "link datom " << datom->blockId << endl;

		Cell3DPosition neighborPos;
		DatomsBlock* neighborBlock;

		for (int i=0; i<12; i++) {
			if (datom->getNeighborPos(i,neighborPos)
				&& (neighborBlock = (DatomsBlock *)lattice->getBlock(neighborPos))!=NULL) {
				datom->getInterface(i)->connect(neighborBlock->getInterface(pos));
				OUTPUT << "connection #" << datom->blockId << "(" << i << ") to #"
					   << neighborBlock->blockId << endl;
			}
		}
    }
}

/**
 * \brief Draw catoms and axes
 */
void DatomsWorld::glDraw() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
// draw catoms
    vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
    lock();
    while (ic!=tabGlBlocks.end()) {
        ((DatomsGlBlock*)(*ic))->glDraw(objBlock);
        ic++;
    }
    unlock();
    glPopMatrix();

// material for the grid walls
    glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
    if (background) {
        glDrawBackground();
    }
}

void DatomsWorld::glDrawId() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
    int n=1;
    lock();
    while (ic!=tabGlBlocks.end()) {
		((DatomsGlBlock*)(*ic))->glDrawId(objBlock,n);
		ic++;
    }
    unlock();
    glPopMatrix();

    glDrawBackground();
}

void DatomsWorld::glDrawIdByMaterial() {
    glPushMatrix();

    glDisable(GL_TEXTURE_2D);
    vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
    int n=1;
    lock();
    while (ic!=tabGlBlocks.end()) {
		((DatomsGlBlock*)(*ic))->glDrawIdByMaterial(objBlockForPicking,n);
		ic++;
    }
    unlock();
    glPopMatrix();
}

void DatomsWorld::glDrawSpecificBg() {
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

void DatomsWorld::loadTextures(const string &str) {
    string path = str+"//hexa.tga";
    int lx,ly;
    idTextureHexa = GlutWindow::loadTexture(path.c_str(),lx,ly);
    path = str+"//textureCarre.tga";
    idTextureGrid = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void DatomsWorld::updateGlData(BuildingBlock *bb) {
    DatomsGlBlock *glblc = (DatomsGlBlock*)bb->getGlBlock();
    if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setPosition(lattice->gridToWorldPosition(bb->position));
		glblc->setColor(bb->color);
		unlock();
    }
}

void DatomsWorld::updateGlData(DatomsBlock*blc, const Color &color) {
    DatomsGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setColor(color);
		unlock();
    }
}

void DatomsWorld::updateGlData(DatomsBlock*blc, bool visible) {
    DatomsGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setVisible(visible);
		unlock();
    }
}

void DatomsWorld::updateGlData(DatomsBlock*blc, const Vector3D &position) {
    DatomsGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setPosition(position);
		unlock();
    }
}

void DatomsWorld::updateGlData(DatomsBlock*blc, const Cell3DPosition &position) {
    DatomsGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
		lock();
		//cout << "update pos:" << position << endl;
		glblc->setPosition(lattice->gridToWorldPosition(position));
		unlock();
    }
}

void DatomsWorld::updateGlData(DatomsBlock*blc, const Matrix &mat) {
    DatomsGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
		lock();
		glblc->mat = mat;
		unlock();
    }
}

void DatomsWorld::setSelectedFace(int n) {
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

void DatomsWorld::exportConfiguration() {
	DatomsConfigExporter exporter = DatomsConfigExporter(this);
	exporter.exportConfiguration();
}

} // Datom namespace
