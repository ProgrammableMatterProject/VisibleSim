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
#include "smartBlocksBlock.h"
#include "scheduler.h"
#include "configExporter.h"

using namespace std;

namespace SmartBlocks {

SmartBlocksWorld::SmartBlocksWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                   int argc, char *argv[]):World(argc, argv) {
    cout << "\033[1;31mSmartBlocksWorld constructor\033[0m" << endl;

    if (GlutContext::GUIisEnabled) {
        objBlock = new ObjLoader::ObjLoader("../../simulatorCore/smartBlocksTextures","smartBlockSimple.obj");
        objRepere = new ObjLoader::ObjLoader("../../simulatorCore/smartBlocksTextures","repere25.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/smartBlocksTextures",
                                                      "smartBlockPicking.obj");
    }
       
    lattice = new SLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
}

SmartBlocksWorld::~SmartBlocksWorld() {
    cout << "\033[1;31mSmartBlocksWorld destructor" << endl;
}

void SmartBlocksWorld::deleteWorld() {
    delete((SmartBlocksWorld*)world);
    world=NULL;
}

void SmartBlocksWorld::addBlock(int blockId, BlockCodeBuilder bcb,
                                const Cell3DPosition &pos, const Color &col,
                                short orientation, bool master) {
	if (blockId > maxBlockId)
		maxBlockId = blockId;
	else if (blockId == -1)
		blockId = incrementBlockId();
        
    SmartBlocksBlock *smartBlock = new SmartBlocksBlock(blockId, bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                             (smartBlock->blockId, (BaseSimulator::BuildingBlock*)smartBlock) );
    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), smartBlock));

    SmartBlocksGlBlock *glBlock = new SmartBlocksGlBlock(blockId);
    tabGlBlocks.push_back(glBlock);
    smartBlock->setGlBlock(glBlock);
    smartBlock->setPosition(pos);
    smartBlock->setColor(col);

    if (lattice->isInGrid(pos)) {
        lattice->insert(smartBlock, pos);
    } else {
        cerr << "ERROR : BLOCK #" << blockId << " out of the grid !!!!!" << endl;
        exit(1);
    }
}

void SmartBlocksWorld::linkBlock(const Cell3DPosition &pos) {
    SmartBlocksBlock *ptrNeighbor;
    SmartBlocksBlock *ptrBlock = (SmartBlocksBlock*)lattice->getBlock(pos);
    vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
    Cell3DPosition nPos;

    // Check neighbors for each interface
    for (int i = 0; i < 4; i++) {
        nPos = pos + nRelCells[i];
        ptrNeighbor = (SmartBlocksBlock*)lattice->getBlock(nPos);
        if (ptrNeighbor) {
            (ptrBlock)->getInterface(SLattice::Direction(i))->
                connect(ptrNeighbor->getInterface(SLattice::Direction(
                                                      SLattice::getOppositeDirection(i))));

            OUTPUT << "connection #" << (ptrBlock)->blockId <<
                " to #" << ptrNeighbor->blockId << endl;
        } else {
            (ptrBlock)->getInterface(SLattice::Direction(i))->connect(NULL);
        }
    }
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
        glScalef(lattice->gridSize[0]*lattice->gridScale[0],
                 lattice->gridSize[1]*lattice->gridScale[1],1.0f);
        glBegin(GL_QUADS);
        glTexCoord2f(0,0);
        glVertex3f(0.0f,0.0f,0.0f);
        glTexCoord2f(lattice->gridSize[0],0);
        glVertex3f(1.0f,0.0f,0.0f);
        glTexCoord2f(lattice->gridSize[0],lattice->gridSize[1]);
        glVertex3f(1.0,1.0,0.0f);
        glTexCoord2f(0,lattice->gridSize[1]);
        glVertex3f(0.0,1.0,0.0f);
        glEnd();
        glPopMatrix();
        // draw the axes
        objRepere->glDraw();

        glPushMatrix();
        /*glTranslatef(-lattice->gridSize[0]/2.0f*lattice->gridScale[0],
          -lattice->gridSize[1]/2.0f*lattice->gridScale[1],0); */
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

void SmartBlocksWorld::glDrawIdByMaterial() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    /*glTranslatef(-lattice->gridSize[0]/2.0f*lattice->gridScale[0],
      -lattice->gridSize[1]/2.0f*lattice->gridScale[1],0);*/
    vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
    int n=1;
    lock();
    while (ic!=tabGlBlocks.end()) {
        glLoadName(n++);
        ((SmartBlocksGlBlock*)(*ic))->glDrawIdByMaterial(objBlockForPicking, n);
        ic++;
    }
    unlock();
    glPopMatrix();
}

void SmartBlocksWorld::glDrawId() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    /*glTranslatef(-lattice->gridSize[0]/2.0f*lattice->gridScale[0],
      -lattice->gridSize[1]/2.0f*lattice->gridScale[1],0);*/
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
    if (GlutContext::GUIisEnabled) {
        //string path = str+"/circuit.tga";
        string path = str+"/bois.tga";
        int lx,ly;
        idTextureFloor = GlutWindow::loadTexture(path.c_str(),lx,ly);

        path=str+"/digits.tga";
        idTextureDigits = GlutWindow::loadTexture(path.c_str(),lx,ly);
    }
}

void SmartBlocksWorld::setSelectedFace(int n) {
    numSelectedGlBlock = n / 5;
    string name = objBlockForPicking->getObjMtlName(n % 5);

    if (name == "Material__73") numSelectedFace = SLattice::South;
    else if (name == "Material__68") numSelectedFace = SLattice::East;
    else if (name == "Material__72") numSelectedFace = SLattice::West;
    else if (name == "Material__71") numSelectedFace = SLattice::North;
    else {
		cerr << "warning: Unrecognized picking face" << endl;
		numSelectedFace = 4;	// UNDEFINED
        return;
    }

    cerr << name << " = " << numSelectedFace << " = "
         << SLattice::getDirectionString(numSelectedFace) << endl;       
}

void SmartBlocksWorld::exportConfiguration() {
	SmartBlocksConfigExporter *exporter = new SmartBlocksConfigExporter(this);
	exporter->exportConfiguration();
	delete exporter;
}


} // SmartBlocks namespace
