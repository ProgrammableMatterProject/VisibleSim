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

#include "catoms2DSimulator.h"
#include "catoms2DWorld.h"
#include "catoms2DBlock.h"
#include "rotation2DEvents.h"

#include "trace.h"
#include "utils.h"
#include "configExporter.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Catoms2D {

Catoms2DWorld::Catoms2DWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                             int argc, char *argv[]):World(argc, argv) {
    OUTPUT << "\033[1;31mCatoms2DWorld constructor\033[0m" << endl;

    if (GlutContext::GUIisEnabled) {
        objBlock = new ObjLoader::ObjLoader("../../simulatorCore/catoms2DTextures","catom2D.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/catoms2DTextures","catom2Dpicking.obj");
        objRepere = new ObjLoader::ObjLoader("../../simulatorCore/smartBlocksTextures","repere25.obj");
    }

    lattice = new HLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
}

Catoms2DWorld::~Catoms2DWorld() {
    OUTPUT << "Catoms2DWorld destructor" << endl;
    /*	block linked are deleted by world::~world() */
}

void Catoms2DWorld::deleteWorld() {
    delete((Catoms2DWorld*)world);
}
void Catoms2DWorld::addBlock(int blockId, BlockCodeBuilder bcb,
                             const Cell3DPosition &pos, const Color &col,
                             short orientation, bool master) {
	if (blockId > maxBlockId)
		maxBlockId = blockId;
	else if (blockId == -1)
		blockId = incrementBlockId();

    Catoms2DBlock *catom2D = new Catoms2DBlock(blockId,bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                             (catom2D->blockId, (BaseSimulator::BuildingBlock*)catom2D));

    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), catom2D));

    Catoms2DGlBlock *glBlock = new Catoms2DGlBlock(blockId);
    tabGlBlocks.push_back(glBlock);
    catom2D->setGlBlock(glBlock);

    catom2D->setPosition(pos);
    catom2D->setColor(col);
    catom2D->isMaster=master;

    cerr << "ADDING BLOCK #" << blockId << " pos:" << pos << " color:" << col << endl;

    if (lattice->isInGrid(pos)) {
        lattice->insert(catom2D, pos);
    } else {
        ERRPUT << "ERROR : BLOCK #" << blockId << " out of the grid !!!!!" << endl;
        exit(1);
    }
}

void Catoms2DWorld::linkBlock(const Cell3DPosition &pos) {
    Catoms2DBlock *ptrNeighbor;
    Catoms2DBlock *ptrBlock = (Catoms2DBlock*)lattice->getBlock(pos);
    vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
    Cell3DPosition nPos;

    // Check neighbors for each interface
    for (int i = 0; i < 6; i++) {
        nPos = pos + nRelCells[i];
        ptrNeighbor = (Catoms2DBlock*)lattice->getBlock(nPos);
        if (ptrNeighbor) {
            (ptrBlock)->getInterface(HLattice::Direction(i))->
                connect(ptrNeighbor->getInterface(HLattice::Direction(
                                                      lattice->getOppositeDirection(i))));

            OUTPUT << "connection #" << (ptrBlock)->blockId <<
                " to #" << ptrNeighbor->blockId << endl;
        } else {
            (ptrBlock)->getInterface(HLattice::Direction(i))->connect(NULL);
        }
    }
}

void Catoms2DWorld::glDraw() {
    glPushMatrix();
    glTranslatef(0.5*lattice->gridScale[0],0,0.5*lattice->gridScale[2]);
    glDisable(GL_TEXTURE_2D);
    vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
    lock();
    while (ic!=tabGlBlocks.end()) {
        ((Catoms2DGlBlock*)(*ic))->glDraw(objBlock);
        ic++;
    }
    unlock();
    glPopMatrix();

    static const GLfloat white[]={0.8f,0.8f,0.8f,1.0f},gray[]={0.2f,0.2f,0.2f,1.0f};

    glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
    glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
    glMaterialfv(GL_FRONT,GL_SPECULAR,gray);
    glMaterialf(GL_FRONT,GL_SHININESS,40.0);
    glPushMatrix();
    enableTexture(true);
    glBindTexture(GL_TEXTURE_2D,idTextureLines);
    glScalef(lattice->gridSize[0]*lattice->gridScale[0],
             lattice->gridSize[1]*lattice->gridScale[1],
             lattice->gridScale[2]+(lattice->gridSize[2]-1)*lattice->gridScale[2]*M_SQRT3_2);
    glBegin(GL_QUADS);
    // bottom
    glNormal3f(0,0,1.0f);
    glTexCoord2f(1.0f,0.25f);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(1.0f,lattice->gridSize[0]+0.25f);
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(0,lattice->gridSize[0]+0.25f);
    glVertex3f(1.0,1.0,0.0f);
    glTexCoord2f(0,0.25f);
    glVertex3f(0.0,1.0,0.0f);
    // top
    glNormal3f(0,0,-1.0f);
    glTexCoord2f(1.0f,0.25f);
    glVertex3f(0.0f,0.0f,1.0f);
    glTexCoord2f(0,0.25f);
    glVertex3f(0.0,1.0,1.0f);
    glTexCoord2f(0,lattice->gridSize[0]+0.25f);
    glVertex3f(1.0,1.0,1.0f);
    glTexCoord2f(1.0f,lattice->gridSize[0]+0.25f);
    glVertex3f(1.0f,0.0f,1.0f);
    // left
    glNormal3f(1.0f,0,0);
    glTexCoord2f(0,0.25f*M_SQRT3_2);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(1.0f,0.25f*M_SQRT3_2);
    glVertex3f(0.0f,1.0f,0.0f);
    glTexCoord2f(1.0f,(lattice->gridSize[2]+0.25f)*M_SQRT3_2);
    glVertex3f(0.0,1.0,1.0f);
    glTexCoord2f(0.0f,(lattice->gridSize[2]+0.25f)*M_SQRT3_2);
    glVertex3f(0.0,0.0,1.0f);
    // right
    glNormal3f(-1.0f,0,0);
    glTexCoord2f(0,0.25f*M_SQRT3_2);
    glVertex3f(1.0f,0.0f,0.0f);
    glTexCoord2f(0.0f,(lattice->gridSize[2]+0.25f)*M_SQRT3_2);
    glVertex3f(1.0,0.0,1.0f);
    glTexCoord2f(1.0f,(lattice->gridSize[2]+0.25f)*M_SQRT3_2);
    glVertex3f(1.0,1.0,1.0f);
    glTexCoord2f(1.0f,0.25f*M_SQRT3_2);
    glVertex3f(1.0f,1.0f,0.0f);
    glEnd();
    glPopMatrix();
    // draw hexa
    glPushMatrix();
    glBindTexture(GL_TEXTURE_2D,idTextureHexa);
    glScalef(lattice->gridSize[0]*lattice->gridScale[0],
             lattice->gridSize[1]*lattice->gridScale[1],
             lattice->gridScale[2]+(lattice->gridSize[2]-1)*lattice->gridScale[2]*M_SQRT3_2);
    float h=((lattice->gridSize[2]-1)+1.0/M_SQRT3_2)/2.0;
    glBegin(GL_QUADS);
    // back
    glNormal3f(0,-1.0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,1.0f,0.0f);
    glTexCoord2f(lattice->gridSize[0]/3.0f,0);
    glVertex3f(1.0f,1.0f,0.0f);
    glTexCoord2f(lattice->gridSize[0]/3.0f,h);
    glVertex3f(1.0f,1.0,1.0f);
    glTexCoord2f(0,h);
    glVertex3f(0.0,1.0,1.0f);
    // front
    glNormal3f(0,1.0,0);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(0,h);
    glVertex3f(0.0,0.0,1.0f);
    glTexCoord2f(lattice->gridSize[0]/3.0f,h);
    glVertex3f(1.0f,0.0,1.0f);
    glTexCoord2f(lattice->gridSize[0]/3.0f,0);
    glVertex3f(1.0f,0.0f,0.0f);
    glEnd();
    glPopMatrix();
    // draw the axes
    glPushMatrix();
    glScalef(0.05f,0.05f,0.05f);
    objRepere->glDraw();
    glPopMatrix();
}

void Catoms2DWorld::glDrawId() {
    glPushMatrix();
    glTranslatef(0.5*lattice->gridScale[0],0,0.5*lattice->gridScale[2]);
    glDisable(GL_TEXTURE_2D);
    vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
    int n=1;
    lock();
    while (ic!=tabGlBlocks.end()) {
        ((Catoms2DGlBlock*)(*ic))->glDrawId(objBlock,n);
        ic++;
    }
    unlock();
    glPopMatrix();
}

void Catoms2DWorld::glDrawIdByMaterial() {
    glPushMatrix();
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);

    glDisable(GL_TEXTURE_2D);
    vector <GlBlock*>::iterator ic=tabGlBlocks.begin();
    int n=1;
    lock();
    while (ic!=tabGlBlocks.end()) {
        ((Catoms2DGlBlock*)(*ic))->glDrawIdByMaterial(objBlockForPicking,n);
        ic++;
    }
    unlock();
    glPopMatrix();
}


void Catoms2DWorld::loadTextures(const string &str) {
    string path = str+"//hexa.tga";
    int lx,ly;
    idTextureHexa = GlutWindow::loadTexture(path.c_str(),lx,ly);
    path = str+"//lignes.tga";
    idTextureLines = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void Catoms2DWorld::updateGlData(BuildingBlock*blc) {
    updateGlData((Catoms2DBlock*)blc,lattice->gridToWorldPosition(blc->position));
}

void Catoms2DWorld::updateGlData(Catoms2DBlock*blc, const Vector3D &position) {
    Catoms2DGlBlock *glblc = (Catoms2DGlBlock*)blc->getGlBlock();
    if (glblc) {
        lock();
        glblc->setPosition(position);
        glblc->setColor(blc->color);
        unlock();
    }
}

void Catoms2DWorld::updateGlData(Catoms2DBlock*blc, const Vector3D &position, double angle) {
    Catoms2DGlBlock *glblc = (Catoms2DGlBlock*)blc->getGlBlock();
    if (glblc) {
        lock();
        glblc->setAngle(angle);
        glblc->setPosition(position);
        glblc->setColor(blc->color);
        unlock();
    }
}

bool Catoms2DWorld::areNeighborsWorldPos(Vector3D &pos1, Vector3D &pos2) {
    float distance = 0;
    for (int i = 0; i < 3; i++) {
        distance += powf(pos2[i] - pos1[i],2);
    }
    distance = sqrt(distance);
    return (ceil(distance) == lattice->gridScale[0]);
}

bool Catoms2DWorld::areNeighborsGridPos(Cell3DPosition &pos1, Cell3DPosition &pos2) {
    Vector3D wpos1 = lattice->gridToWorldPosition(pos1);
    Vector3D wpos2 = lattice->gridToWorldPosition(pos2);

    return areNeighborsWorldPos(wpos1,wpos2);
}

void Catoms2DWorld::menuChoice(int n) {
    Catoms2DBlock *bb = (Catoms2DBlock *)getBlockById(tabGlBlocks[numSelectedGlBlock]->blockId);

    switch (n) {
    case 5: {                 // Move Left
        // Identify pivot
        int pivotId = bb->getCCWMovePivotId();
        Catoms2DBlock *pivot = (Catoms2DBlock *)getBlockById(pivotId);
        Rotation2DMove move = Rotation2DMove(pivot, RelativeDirection::CCW);
        bb->startMove(move);
    } break;
    case 6: {                // Move Right
        // Identify pivot
        int pivotId = bb->getCWMovePivotId();
        Catoms2DBlock *pivot = (Catoms2DBlock *)getBlockById(pivotId);
        Rotation2DMove move = Rotation2DMove(pivot, RelativeDirection::CW);
        bb->startMove(move);
    } break;
    default: World::menuChoice(n); break; // For all non-catoms2D-specific cases
    }
}

void Catoms2DWorld::setSelectedFace(int n) {
    numSelectedGlBlock = n / numPickingTextures;
    string name = objBlockForPicking->getObjMtlName(n % numPickingTextures);
    numSelectedFace = numPickingTextures;   // Undefined NeighborDirection

    if (name == "face_0") numSelectedFace = HLattice::Right;
    else if (name == "face_1") numSelectedFace = HLattice::TopRight;
    else if (name == "face_2") numSelectedFace = HLattice::TopLeft;
    else if (name == "face_3") numSelectedFace = HLattice::Left;
    else if (name == "face_4") numSelectedFace = HLattice::BottomLeft;
    else if (name == "face_5") numSelectedFace = HLattice::BottomRight;
}

/**
 * Displays a popup menu at coordinates (ix, iy)
 *  Overriden for catoms2D to allow for c2d movements
 *
 * @param ix
 * @param iy
 * @return
 */
void Catoms2DWorld::createPopupMenu(int ix, int iy) {
    if (!GlutContext::popupMenu) {
        GlutContext::popupMenu = new GlutPopupMenuWindow(NULL,0,0,200,180);
        GlutContext::popupMenu->addButton(1,"../../simulatorCore/menuTextures/menu_add.tga");
        GlutContext::popupMenu->addButton(2,"../../simulatorCore/menuTextures/menu_del.tga");
        GlutContext::popupMenu->addButton(3,"../../simulatorCore/menuTextures/menu_tap.tga");
        GlutContext::popupMenu->addButton(4,"../../simulatorCore/menuTextures/menu_save.tga");
        GlutContext::popupMenu->addButton(5,"../../simulatorCore/menuTextures/menu_tap.tga");
        GlutContext::popupMenu->addButton(6,"../../simulatorCore/menuTextures/menu_tap.tga");
        GlutContext::popupMenu->addButton(7,"../../simulatorCore/menuTextures/menu_cancel.tga");
    }

    if (iy < GlutContext::popupMenu->h) iy = GlutContext::popupMenu->h;

    cerr << "Block " << numSelectedGlBlock << ":" << lattice->getDirectionString(numSelectedFace)
         << " selected" << endl;

    Catoms2DBlock *bb = (Catoms2DBlock *)getBlockById(tabGlBlocks[numSelectedGlBlock]->blockId);

    GlutContext::popupMenu->activate(1, canAddBlockToFace((int)numSelectedGlBlock, (int)numSelectedFace));
    GlutContext::popupMenu->activate(5, bb->getCCWMovePivotId() != -1);
    GlutContext::popupMenu->activate(6, bb->getCWMovePivotId() != -1);
    GlutContext::popupMenu->setCenterPosition(ix,GlutContext::screenHeight-iy);
    GlutContext::popupMenu->show(true);
}

void Catoms2DWorld::exportConfiguration() {
	Catoms2DConfigExporter *exporter = new Catoms2DConfigExporter(this);
	exporter->exportConfiguration();
	delete exporter;
}

} // Catoms2D namespace
