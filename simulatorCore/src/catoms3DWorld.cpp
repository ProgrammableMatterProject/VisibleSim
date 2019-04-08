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
#include "catoms3DMotionEngine.h"
#include "trace.h"
#include "configExporter.h"
#include "rotation3DEvents.h"
#include "simulator.h"

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
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "Catoms3DWorld constructor" << TermColor::Reset << endl;
#endif

    if (GlutContext::GUIisEnabled) {
/* Toggle to use catoms3D with max connector size (no rotation) but very simple models*/
#define CATOMS3D_TEXTURE_ID 0
#define UseC3DSkewFCC 1

#if CATOMS3D_TEXTURE_ID == 1 // Standard, no conID
        objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures","catom3DV2.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures","catom3D_picking.obj");
#elif CATOMS3D_TEXTURE_ID == 2 // w/ coordinates
        objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures", "catom3Drepere3.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures","catom_max_vs.obj");
#elif CATOMS3D_TEXTURE_ID == 3 // filled catoms
        objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures","catom_max_vs.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures","catom_max_vs.obj");
#else // 0 standard, with conIDs
        objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures","catom3DV2connectorID.obj");
        objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures","catom3D_picking.obj");
#endif

#if UseC3DSkewFCC == 1
        objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures","repereCatom3D_Zinc.obj");
#else
        objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/catoms3DTextures","repereCatom3D.obj");
#endif
    }

#if UseC3DSkewFCC == 1
    lattice = new SkewFCCLattice(gridSize,gridScale.hasZero() ? defaultBlockSize : gridScale);
#else
    lattice = new FCCLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
#endif

    motionRules = new Catoms3DMotionRules();
}

Catoms3DWorld::~Catoms3DWorld() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "Catoms3DWorld destructor" << endl;
#endif
    /*	block linked are deleted by world::~world() */
    delete motionRules;
}

void Catoms3DWorld::deleteWorld() {
    delete((Catoms3DWorld*)world);
}

void Catoms3DWorld::createPopupMenu(int ix, int iy) {
    if (!GlutContext::popupMenu) {
        GlutContext::popupMenu = new GlutPopupMenuWindow(NULL,0,0,202,215);
        // create submenu "Add"
        GlutPopupMenuWindow *addBlockSubMenu = new GlutPopupMenuWindow(NULL,0,0,202,112);
        addBlockSubMenu->id=50;
        addBlockSubMenu->addButton(11,"../../simulatorCore/resources/textures/menuTextures/menu_add_normal.tga");
        addBlockSubMenu->addButton(12,"../../simulatorCore/resources/textures/menuTextures/menu_add_same.tga");
        addBlockSubMenu->addButton(13,"../../simulatorCore/resources/textures/menuTextures/menu_add_random.tga");
    // create submenu "Rotate"
        GlutPopupMenuWindow *rotateBlockSubMenu = new GlutPopupMenuWindow(NULL,0,0,116,40);
        rotateBlockSubMenu->id=51;

        GlutContext::popupMenu->addButton(1,"../../simulatorCore/resources/textures/menuTextures/menu_add_sub.tga",addBlockSubMenu);
        GlutContext::popupMenu->addButton(2,"../../simulatorCore/resources/textures/menuTextures/menu_del.tga");

        GlutContext::popupMenu->addButton(6,"../../simulatorCore/resources/textures/menuTextures/menu_rotate_sub.tga",rotateBlockSubMenu);
        GlutContext::popupMenu->addButton(3,"../../simulatorCore/resources/textures/menuTextures/menu_tap.tga");
        GlutContext::popupMenu->addButton(4,"../../simulatorCore/resources/textures/menuTextures/menu_save.tga");
        GlutContext::popupMenu->addButton(5,"../../simulatorCore/resources/textures/menuTextures/menu_cancel.tga");
    }

  // update rotateSubMenu depending on rotation catoms3DCapabilities
    Catoms3DBlock *bb = (Catoms3DBlock *)getSelectedBuildingBlock();
    vector<std::pair<const Catoms3DMotionRulesLink*, Rotations3D>> tab = Catoms3DMotionEngine::getAllRotationsForModule(bb);
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
        short finalOrient;
        for(auto &elem:tab) {
            elem.second.init(((Catoms3DGlBlock*)bb->ptrGlBlock)->mat);
            elem.second.getFinalPositionAndOrientation(finalPos,finalOrient);
            if (lattice->isInGrid(finalPos) && lattice->isFree(finalPos)) {
                rotateBlockSubMenu->addButton(new GlutRotationButton(NULL,i++,0,0,0,0,"../../simulatorCore/resources/textures/menuTextures/menu_link.tga",
                elem.first->isOctaFace(),elem.first->getConFromID(),elem.first->getConToID(),finalPos,finalOrient));
            }
        }
    }

    if (iy < GlutContext::popupMenu->h) iy = GlutContext::popupMenu->h;
    cerr << "Block " << numSelectedGlBlock << ":" << lattice->getDirectionString(numSelectedFace)
                 << " selected" << endl;
    GlutContext::popupMenu->activate(1, canAddBlockToFace((int)numSelectedGlBlock, (int)numSelectedFace));
    GlutContext::popupMenu->setCenterPosition(ix,GlutContext::screenHeight-iy);
    GlutContext::popupMenu->show(true);
    if (GlutContext::popupSubMenu) GlutContext::popupSubMenu->show(false);
}

void Catoms3DWorld::menuChoice(int n) {
    Catoms3DBlock *bb = (Catoms3DBlock *)getSelectedBuildingBlock();
    Cell3DPosition nPos;
    switch (n) {
        case 1: case 6:
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
        case 11:
            GlutContext::popupSubMenu->show(false);
            GlutContext::popupMenu->show(false);
            if (bb->getNeighborPos(numSelectedFace,nPos)) {
                addBlock(0, bb->buildNewBlockCode, nPos,bb->color,0,false);
                linkBlock(nPos);
                linkNeighbors(nPos);
            } else {
                cerr << "Position out of the grid" << endl;
            }
        break;
        case 12:
            GlutContext::popupSubMenu->show(false);
            GlutContext::popupMenu->show(false);
            if (bb->getNeighborPos(numSelectedFace,nPos)) {
                addBlock(0, bb->buildNewBlockCode,nPos,bb->color,bb->orientationCode,false);
                linkBlock(nPos);
                linkNeighbors(nPos);
            } else {
                cerr << "Position out of the grid" << endl;
            }
        break;
        case 13:
            GlutContext::popupSubMenu->show(false);
            GlutContext::popupMenu->show(false);
            if (bb->getNeighborPos(numSelectedFace,nPos)) {
                int orient = rand()%24;
                addBlock(0, bb->buildNewBlockCode,nPos,bb->color,orient,false);
                linkBlock(nPos);
                linkNeighbors(nPos);
            } else {
                cerr << "Position out of the grid" << endl;
            }
        break;
    default:
          if (n>=100) {
                GlutContext::popupSubMenu->show(false);
                GlutContext::popupMenu->show(false);
                // if (getScheduler()->state == RUNNING) {
                // scheduler->schedule(new Rotation3DStartEvent(getScheduler()->now(), bb, Rotations3D r));
                // } else {
                Cell3DPosition pos = ((GlutRotationButton*)GlutContext::popupSubMenu->getButton(n))->finalPosition;
                short orient = ((GlutRotationButton*)GlutContext::popupSubMenu->getButton(n))->finalOrientation;
                Catoms3DWorld *wrld = getWorld();
                wrld->disconnectBlock(bb);
                bb->setPositionAndOrientation(pos,orient);
                wrld->connectBlock(bb);
                //}
            } else World::menuChoice(n); // For all non-catoms2D-specific cases
        break;
    }
}


void Catoms3DWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos,
                             const Color &col, short orientation, bool master) {
    if (blockId > maxBlockId)
        maxBlockId = blockId;
    else if (blockId == 0)
        blockId = incrementBlockId();

    Catoms3DBlock *catom = new Catoms3DBlock(blockId,bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                             (catom->blockId, (BaseSimulator::BuildingBlock*)catom));

    // FIXME: Adversarial start, randomly initiate start event
    std::mt19937 rng;
    rng.seed(Simulator::getSimulator()->getCmdLine().getSimulationSeed());
    std::uniform_int_distribution<std::mt19937::result_type> u500(0,500);
    // getScheduler()->schedule(new CodeStartEvent(getScheduler()->now() + u500(rng), catom));
    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), catom));

    Catoms3DGlBlock *glBlock = new Catoms3DGlBlock(blockId);
    glBlock->setPosition(lattice->gridToWorldPosition(pos));

    catom->setGlBlock(glBlock);
    catom->setPositionAndOrientation(pos,orientation);
    catom->setColor(col);
    lattice->insert(catom, pos);

    lock();
    mapGlBlocks.insert(make_pair(blockId, glBlock));
    unlock();
    linkBlock(pos);
}

/**
 * \brief Connect the block placed on the cell at position pos
 */
void Catoms3DWorld::linkBlock(const Cell3DPosition& pos) {
    Catoms3DBlock *catom = (Catoms3DBlock *)lattice->getBlock(pos);

    if (catom) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "link catom " << catom->blockId << endl;
#endif

        Cell3DPosition neighborPos;
        Catoms3DBlock* neighborBlock;
        for (int i=0; i<12; i++) {
            if (catom->getNeighborPos(i,neighborPos)
                && (neighborBlock = (Catoms3DBlock *)lattice->getBlock(neighborPos))!=NULL) {
                catom->getInterface(i)->connect(neighborBlock->getInterface(pos));
#ifdef DEBUG_NEIGHBORHOOD
                OUTPUT << "connection #" << catom->blockId << "(" << i << ") to #"
                       << neighborBlock->blockId << endl;
#endif
            }
        }
    } else {
        OUTPUT << "ERROR: trying to link a block in an empty cell!" << endl;
    }
}

/**
 * \brief Draw catoms and axes
 */
void Catoms3DWorld::glDraw() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    // draw catoms
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((Catoms3DGlBlock*)pair.second)->glDraw(objBlock);
    }
    unlock();
    glPopMatrix();

    // draw the goal surface
    /*if (buildingBlocksMap.begin()!=buildingBlocksMap.end()) {
        map<bID, BuildingBlock*>::iterator it = buildingBlocksMap.begin();
        it->second->blockCode->target->glDraw();
    }*/
/*	GLfloat mat_ambient[] = { 0.2, 0.2, 0.2, 1.0 };
    GLfloat mat_diffuse[] = { 0.8, 0.2, 0.8, 1.0 };
    GLfloat mat_specular[] = { 0.8, 0.8, 0.8, 1.0 };
    GLfloat mat_shininess[] = { 50.0 };

    glMaterialfv(GL_FRONT, GL_AMBIENT, mat_ambient);
    glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_diffuse);
    glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
    glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess);
    enableTexture(false);
    glPushMatrix();
        // Nurbs surface
    //glScalef(45,45,45);
        //Nurbs car
        //glScalef(33.33,33.33,33.33);
        //Nurbs mirror
        glScalef(10,10,10);
    gluBeginSurface(theNurb);
    gluNurbsSurface(theNurb,
        S_NUMKNOTS, sknots,
        T_NUMKNOTS, tknots,
        4 * T_NUMPOINTS,
        4,
        &ctlpoints[0][0][0],
        S_ORDER, T_ORDER,
        GL_MAP2_VERTEX_4);
    gluEndSurface(theNurb);

    glPopMatrix();
*/

    BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
    if (bb) bb->blockCode->onGlDraw();

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

    lattice->glDraw();

    // if (BlockCode::target and dynamic_cast<TargetCSG*>(BlockCode::target)) {
    //     glScalef(lattice->gridScale[0], lattice->gridScale[1], lattice->gridScale[2]);
    //     static_cast<TargetCSG*>(BlockCode::target)->highlight();
    // }
}

void Catoms3DWorld::glDrawId() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((Catoms3DGlBlock*)pair.second)->glDrawId(objBlock, pair.first);
    }
    unlock();
    glPopMatrix();
}

void Catoms3DWorld::glDrawIdByMaterial() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    lock();
    int n;
    for (const auto& pair : mapGlBlocks) {
        n = pair.first*13;
        ((Catoms3DGlBlock*)pair.second)->glDrawIdByMaterial(objBlockForPicking,n);
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
    numSelectedGlBlock = n/13;
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
    cerr << name << " => " << numSelectedFace << endl;
}

void Catoms3DWorld::exportConfiguration() {
    Catoms3DConfigExporter exporter = Catoms3DConfigExporter(this);
    exporter.exportConfiguration();
}

bool Catoms3DWorld::exportSTLModel(string title) {
    ofstream file(title);
    if (!file.is_open()) return false;
    cout << "Writing STL output file..." << endl;
    ObjLoader::ObjLoader stl("../../simulatorCore/resources/textures/catoms3DTextures","catoms3D_STL.obj");
    Matrix mt;
    Vector3D pos;
    Cell3DPosition cell,neighborCell;

    // détermine la distance de chaque module au bord
    /*int n=0;
    vector <Catoms3DBlock*> tabBorders;
    cout << "step #1: " << endl;;
    for (const std::pair<bID, BuildingBlock*>& pair : buildingBlocksMap) {
        Catoms3DBlock *bb = (Catoms3DBlock *)pair.second;
        if (bb->getNbNeighbors()<12) {
            bb->setColor(RED);
            bb->distanceToBorder=1;
            tabBorders.push_back(bb);
            n++;
        } else {
            bb->setColor(WHITE);
            bb->distanceToBorder=0;
        }
    }
    cout << "#1borders =" << n << endl;
    cout << "step #2: " << endl;
    n=0;
    P2PNetworkInterface *p2p;
    Catoms3DBlock *bbConnected;
    for (const Catoms3DBlock* bb : tabBorders) {

        for (int i=0; i<12; i++) {
            p2p = bb->getInterface(i);
            if (p2p && p2p->connectedInterface) {
                bbConnected = (Catoms3DBlock *)p2p->connectedInterface->hostBlock;
                if (bbConnected->distanceToBorder==0) {
                    bbConnected->distanceToBorder=2;
                    bbConnected->setColor(GREEN);
                    n++;
                }
            }
        }
    }
    cout << "#2borders =" << n << endl;
    */

    lock();
    for (const std::pair<bID, GlBlock*>& pair : mapGlBlocks) {
        GlBlock *glblock = pair.second;
        file << "solid catom#" << glblock->blockId << endl;
        pos.set(glblock->position[0],glblock->position[1],glblock->position[2]);
        cell = lattice->worldToGridPosition(pos);
        for (int i=0; i<12; i++) {
            neighborCell = lattice->getCellInDirection(cell,i);
            if ((!lattice->isInGrid(neighborCell) || lattice->isFree(neighborCell))) {
                stl.saveSTLfacets(file,pos,i*2,(i+1)*2); // draw connector
            }
        }
        stl.saveSTLfacets(file,pos,22,-1); // draw other faces
        file << "        endsolid catom#" << glblock->blockId << endl;
    }
    unlock();
    file.close();
    cout << "...done." << endl;
    return true;
}


} // Catoms3DBlock namespace
