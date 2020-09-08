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

#include "slidingCubesWorld.h"
#include "slidingCubesBlock.h"
#include "../../utils/trace.h"
#include "../../utils/configExporter.h"
#include "../../replay/replayExporter.h"

using namespace std;

namespace SlidingCubes {

SlidingCubesWorld::SlidingCubesWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                   int argc, char *argv[]):World(argc, argv) {
    OUTPUT << TermColor::LifecycleColor << "SlidingCubesWorld constructor" << TermColor::Reset << endl;

    if (GlutContext::GUIisEnabled) {
        objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/slidingCubesTextures","slidingCube.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/slidingCubesTextures","slidingCubePicking.obj");
        objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures","repere25.obj");
    }

    lattice = new SCLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);

    motionRules = new SlidingCubesMotionRules();
}

SlidingCubesWorld::~SlidingCubesWorld() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "SlidingCubesWorld destructor" << endl;
#endif
    /*	block linked are deleted by world::~world() */
        delete motionRules;
}

void SlidingCubesWorld::deleteWorld() {
    delete((SlidingCubesWorld*)world);
}

void SlidingCubesWorld::createPopupMenu(int ix, int iy) {
    if (!GlutContext::popupMenu) {
        GlutContext::popupMenu = new GlutPopupMenuWindow(NULL,0,0,202,215);
        // create submenu "Add"
        GlutContext::popupMenu->addButton(1,"../../simulatorCore/resources/textures/menuTextures/menu_add.tga");
        // create submenu "Rotate"
        GlutPopupMenuWindow *rotateBlockSubMenu = new GlutPopupMenuWindow(NULL,0,0,116,40);
        rotateBlockSubMenu->id=51;

        GlutContext::popupMenu->addButton(2,"../../simulatorCore/resources/textures/menuTextures/menu_del.tga");

        GlutContext::popupMenu->addButton(6,"../../simulatorCore/resources/textures/menuTextures/menu_rotate_sub.tga",rotateBlockSubMenu);
        GlutContext::popupMenu->addButton(3,"../../simulatorCore/resources/textures/menuTextures/menu_tap.tga");
        GlutContext::popupMenu->addButton(4,"../../simulatorCore/resources/textures/menuTextures/menu_save.tga");
        GlutContext::popupMenu->addButton(5,"../../simulatorCore/resources/textures/menuTextures/menu_cancel.tga");
    }

    // update rotateSubMenu depending on rotation catoms3DCapabilities
    SlidingCubesBlock *rb = (SlidingCubesBlock*)getSelectedBuildingBlock();
    vector<SlidingCubesMotionRule*> tab = motionRules->getValidMotionList(rb);
    // remove element pointing in the same motion
    /*if (tab.size()>1) {
        auto ci=tab.begin()+1;
        auto ci2=ci;
        Cell3DPosition finalPos;
        while (ci<tab.end()) {
            // search for previous motion with the same goal finalPosition
            finalPos = (*ci)->getFinalPosition(rb);
            cout << (*ci)->isRotation() << ":" << (*ci)->getToID() << "," << finalPos << endl;
            ci2 = tab.begin();
            while (ci2!=ci && (*ci2)->getFinalPosition(rb)!=finalPos) {
                ci2++;
            }
            if (ci2==ci) {
                ci2=ci-1gridToUnscaledWorldPosition;
                tab.erase(ci);
                ci=ci2;
            }
            ci++;
        }
    }*/

    int nbreMenus=tab.size();
    if (nbreMenus==0) {
        ((GlutButton*)GlutContext::popupMenu->getButton(6))->activate(false);
    } else {
        ((GlutButton*)GlutContext::popupMenu->getButton(6))->activate(true);
        GlutPopupMenuWindow *rotateBlockSubMenu = (GlutPopupMenuWindow*)GlutContext::popupMenu->getButton(6)->getChild(0);
        rotateBlockSubMenu->h = nbreMenus*35+10;
        rotateBlockSubMenu->clearChildren();
        int i=100;
        Cell3DPosition finalPos;
        auto ci=tab.begin();
        while (ci<tab.end()) {
            finalPos = (*ci)->getFinalPosition(rb);
            //cout << "printed: " << (*ci)->isRotation() << ":" << (*ci)->getToID() << "," << finalPos << endl;
            rotateBlockSubMenu->addButton(new GlutRBMotionButton(NULL,i++,0,0,0,0,"../../simulatorCore/resources/textures/menuTextures/menu_move_rb.tga", (*ci)->isRotation(),(*ci)->getToID(),finalPos));
            ci++;
        }
    }

    if (iy < GlutContext::popupMenu->h) iy = GlutContext::popupMenu->h;
    cout << "Block " << numSelectedGlBlock << ":" << lattice->getDirectionString(numSelectedFace)
    << " selected" << endl;
    GlutContext::popupMenu->activate(1, canAddBlockToFace((int)numSelectedGlBlock, (int)numSelectedFace));
    GlutContext::popupMenu->setCenterPosition(ix,GlutContext::screenHeight-iy);
    GlutContext::popupMenu->show(true);
    if (GlutContext::popupSubMenu) GlutContext::popupSubMenu->show(false);
}

void SlidingCubesWorld::menuChoice(int n) {
    SlidingCubesBlock *rb = (SlidingCubesBlock *)getSelectedBuildingBlock();
    Cell3DPosition nPos;
    switch (n) {
        case 6:
            GlutContext::popupMenu->show(true);
            GlutContext::popupSubMenu = (GlutPopupMenuWindow*)GlutContext::popupMenu->getButton(n)->getChild(0);
            GlutContext::popupSubMenu->show(true);
            GlutContext::popupSubMenu->x=GlutContext::popupMenu->x+GlutContext::popupMenu->w+5;
            GlutContext::popupSubMenu->y=GlutContext::popupMenu->y+GlutContext::popupMenu->getButton(n)->y-GlutContext::popupSubMenu->h/2;
            // avoid placing submenu over the top of the window
            if (GlutContext::popupSubMenu->y+GlutContext::popupSubMenu->h > GlutContext::screenHeight) {
                GlutContext::popupSubMenu->y = GlutContext::screenHeight-GlutContext::popupSubMenu->h;
            }
            break;
        default:
            if (n>=100) {
                GlutContext::popupSubMenu->show(false);
                GlutContext::popupMenu->show(false);

                Cell3DPosition pos = ((GlutRotationButton*)GlutContext::popupSubMenu->getButton(n))->finalPosition;
                SlidingCubesWorld *wrld = getWorld();
                wrld->disconnectBlock(rb);
                rb->setPosition(pos);
                wrld->connectBlock(rb);
                //}
            } else {
                cout << "menu world:" << n << endl;
                World::menuChoice(n); // For all non-catoms2D-specific cases
            }
            break;
    }
}

void SlidingCubesWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos,
                                const Color &col, short orientation, bool master) {
    if (blockId > maxBlockId)
        maxBlockId = blockId;
    else if (blockId == 0)
        blockId = incrementBlockId();

    SlidingCubesBlock *robotBlock = new SlidingCubesBlock(blockId, bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                             (robotBlock->blockId, (BaseSimulator::BuildingBlock*)robotBlock));

    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), robotBlock));

    SlidingCubesGlBlock *glBlock = new SlidingCubesGlBlock(blockId);
    mapGlBlocks.insert(make_pair(blockId, glBlock));
    robotBlock->setGlBlock(glBlock);
    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeAddModule(getScheduler()->now(), blockId);
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

void SlidingCubesWorld::linkBlock(const Cell3DPosition &pos) {
    SlidingCubesBlock *ptrNeighbor;
    SlidingCubesBlock *ptrBlock = (SlidingCubesBlock*)lattice->getBlock(pos);
    vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
    Cell3DPosition nPos;

    // Check neighbors for each interface
    for (int i = 0; i < 6; i++) {
        nPos = pos + nRelCells[i];
        ptrNeighbor = (SlidingCubesBlock*)lattice->getBlock(nPos);
        if (ptrNeighbor) {
            (ptrBlock)->getInterface(SCLattice::Direction(i))->
                connect(ptrNeighbor->getInterface(SCLattice::Direction(
                                                      lattice->getOppositeDirection(i))));

#ifdef DEBUG_NEIGHBORHOOD
            OUTPUT << "connection #" << (ptrBlock)->blockId << ":" << lattice->getDirectionString(i) <<
                " to #" << ptrNeighbor->blockId << ":"
                   << lattice->getDirectionString(lattice->getOppositeDirection(i)) << endl;
#endif
        } else {
            (ptrBlock)->getInterface(SCLattice::Direction(i))->connect(NULL);
        }
    }
}

void SlidingCubesWorld::glDraw() {
    glPushMatrix();
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
    glDisable(GL_TEXTURE_2D);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((SlidingCubesGlBlock*)pair.second)->glDraw(objBlock);
    }
    unlock();

    BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
    if (bb) bb->blockCode->onGlDraw();
    lattice->glDraw();
    glPopMatrix();
}

void SlidingCubesWorld::glDrawId() {
    glPushMatrix();
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);
    glDisable(GL_TEXTURE_2D);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((SlidingCubesGlBlock*)pair.second)->glDrawId(objBlock, pair.first);
    }
    unlock();
    glPopMatrix();
}

void SlidingCubesWorld::glDrawIdByMaterial() {
    glPushMatrix();
    glTranslatef(0.5*lattice->gridScale[0],0.5*lattice->gridScale[1],0.5*lattice->gridScale[2]);

    glDisable(GL_TEXTURE_2D);
    int n;
    lock();
    for (const auto& pair : mapGlBlocks) {
        n = pair.first * numPickingTextures;
        ((SlidingCubesGlBlock*)pair.second)->glDrawIdByMaterial(objBlockForPicking,n);
    }
    unlock();
    glPopMatrix();
}

void SlidingCubesWorld::glDrawBackground() {
    static const GLfloat white[]={0.8f,0.8f,0.8f,1.0f},
        gray[]={0.2f,0.2f,0.2f,1.0f};

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
void SlidingCubesWorld::loadTextures(const string &str) {
    string path = str+"/texture_plane.tga";
    int lx,ly;
    idTextureWall = loadTexture(path.c_str(),lx,ly);
}

void SlidingCubesWorld::setSelectedFace(int n) {
    numSelectedGlBlock=n/numPickingTextures;
    string name = objBlockForPicking->getObjMtlName(n%numPickingTextures);
        if (name=="face_top") numSelectedFace=SCLattice::Top;
    else if (name=="face_bottom") numSelectedFace=SCLattice::Bottom;
    else if (name=="face_right") numSelectedFace=SCLattice::Right;
    else if (name=="face_left") numSelectedFace=SCLattice::Left;
    else if (name=="face_front") numSelectedFace=SCLattice::Front;
    else if (name=="face_back") numSelectedFace=SCLattice::Back;
}

void SlidingCubesWorld::exportConfiguration() {
    SlidingCubesConfigExporter exporter = SlidingCubesConfigExporter(this);
    exporter.exportConfiguration();
}

void saveStlRect(ofstream &fout,const Vector3D &O,const Vector3D &u,const Vector3D &v,const Vector3D &N) {
    char buf[25];
    snprintf(buf,25,"%5.3f %5.3f %5.3f", N[0], N[1], N[2]);
    fout << "          facet normal " << buf << endl;
    fout << "            outer loop" << endl;
    snprintf(buf,25,"%5.3f %5.3f %5.3f", O[0], O[1], O[2]);
    fout << "              vertex " << buf << endl;
    snprintf(buf,25,"%5.3f %5.3f %5.3f", O[0]+u[0], O[1]+u[1], O[2]+u[2]);
    fout << "              vertex " << buf << endl;
    snprintf(buf,25,"%5.3f %5.3f %5.3f", O[0]+v[0], O[1]+v[1], O[2]+v[2]);
    fout << "              vertex " << buf << endl;
    fout << "            endloop" << endl;
    fout << "          endfacet" << endl;

    snprintf(buf,25,"%5.3f %5.3f %5.3f", N[0], N[1], N[2]);
    fout << "          facet normal " << buf << endl;
    fout << "            outer loop" << endl;
    snprintf(buf,25,"%5.3f %5.3f %5.3f", O[0]+u[0], O[1]+u[1], O[2]+u[2]);
    fout << "              vertex " << buf << endl;
    snprintf(buf,25,"%5.3f %5.3f %5.3f", O[0]+u[0]+v[0], O[1]+u[1]+v[1], O[2]+u[2]+v[2]);
    fout << "              vertex " << buf << endl;
    snprintf(buf,25,"%5.3f %5.3f %5.3f", O[0]+v[0], O[1]+v[1], O[2]+v[2]);
    fout << "              vertex " << buf << endl;
    fout << "            endloop" << endl;
    fout << "          endfacet" << endl;
}

bool SlidingCubesWorld::exportSTLModel(string title) {
    cout << "Writing STL output file..." << endl;
    Matrix mt;
    Vector3D pos,v1,v2,N;
    Cell3DPosition cell,neighborCell;


    // select robotBlock in the border
    vector <SlidingCubesBlock*> borderBlocks;
    cout << "step #1: " << endl;
    for (const auto& pair : buildingBlocksMap) {
        if (pair.second->getState() != BuildingBlock::REMOVED
            and (pair.second->ptrGlBlock and pair.second->ptrGlBlock->isVisible())) {
            SlidingCubesBlock *rb = (SlidingCubesBlock *)pair.second;
            if (rb->getNbNeighbors()<6) { // moins de 6 voisins
                rb->setColor(RED);
                borderBlocks.push_back(rb);
            }
        }
    }

    cout << "border blocks: " << borderBlocks.size() << "/" << buildingBlocksMap.size() << endl;

    cout << "step #2: " << endl;
    // int loop=0,nbreLoop=borderBlocks.size();
    ofstream file(title);
    if (!file.is_open()) return false;

    lock();
    for (auto block: borderBlocks) {
        GlBlock *glblock = block->getGlBlock();
        file << "solid rb#" << glblock->blockId << endl;
        pos.set(glblock->position[0],glblock->position[1],glblock->position[2]);
        cell = lattice->worldToGridPosition(pos);

        // top connector
        if (!block->getInterface(SCLattice::Direction::Top)->isConnected() ||
            block->getInterface(SCLattice::Direction::Top)->connectedInterface->hostBlock==NULL) {
            pos.set(glblock->position[0]-5.0,glblock->position[1]-5.0,glblock->position[2]+5.0);
            v1.set(10.0,0,0);
            v2.set(0,10.0,0);
            N.set(0,0,1.0);
            saveStlRect(file,pos,v1,v2,N);
        }
        // bottom connector
        if (!block->getInterface(SCLattice::Direction::Bottom)->isConnected() ||
            block->getInterface(SCLattice::Direction::Bottom)->connectedInterface->hostBlock==NULL) {
            pos.set(glblock->position[0]+5.0,glblock->position[1]-5.0,glblock->position[2]-5.0);
            v1.set(-10.0,0,0);
            v2.set(0,10.0,0);
            N.set(0,0,-1.0);
            saveStlRect(file,pos,v1,v2,N);
        }
        // left connector
        if (!block->getInterface(SCLattice::Direction::Left)->isConnected() ||
            block->getInterface(SCLattice::Direction::Left)->connectedInterface->hostBlock==NULL) {
            pos.set(glblock->position[0]-5.0,glblock->position[1]+5.0,glblock->position[2]-5.0);
        v1.set(0,-10.0,0);
        v2.set(0,0,10.0);
        N.set(-1.0,0,0);
        saveStlRect(file,pos,v1,v2,N);
            }
        // right connector
        if (!block->getInterface(SCLattice::Direction::Right)->isConnected() ||
            block->getInterface(SCLattice::Direction::Right)->connectedInterface->hostBlock==NULL) {
            pos.set(glblock->position[0]+5.0,glblock->position[1]-5.0,glblock->position[2]-5.0);
            v1.set(0,10.0,0);
            v2.set(0,0,10.0);
            N.set(1.0,0,0);
            saveStlRect(file,pos,v1,v2,N);
        }
        // back connector
        if (!block->getInterface(SCLattice::Direction::Back)->isConnected() ||
            block->getInterface(SCLattice::Direction::Back)->connectedInterface->hostBlock==NULL) {
            pos.set(glblock->position[0]+5.0,glblock->position[1]+5.0,glblock->position[2]-5.0);
            v1.set(-10.0,0,0);
            v2.set(0,0,10.0);
            N.set(0,-1.0,0);
            saveStlRect(file,pos,v1,v2,N);
        }
        // front connector
        if (!block->getInterface(SCLattice::Direction::Front)->isConnected() ||
            block->getInterface(SCLattice::Direction::Front)->connectedInterface->hostBlock==NULL) {
            pos.set(glblock->position[0]-5.0,glblock->position[1]-5.0,glblock->position[2]-5.0);
            v1.set(10.0,0,0);
            v2.set(0,0,10.0);
            N.set(0,1.0,0);
            saveStlRect(file,pos,v1,v2,N);
        }


        file << "        endsolid rb#" << glblock->blockId << endl;
    }
    unlock();
    file.close();
    cout << "...done." << endl;

    return true;
}


} // RobotBlock namespace
