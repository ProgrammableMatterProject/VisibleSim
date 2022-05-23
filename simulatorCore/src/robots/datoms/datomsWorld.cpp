/*!
 * \file datomsWorld.cpp
 * \brief datoms world
 * \date 28/01/2018
 * \author Benoît Piranda
 */

#include <iostream>
#include <string>
#include <stdlib.h>
#include <sys/types.h>
#include <signal.h>

#include "datomsWorld.h"
#include "datomsBlock.h"
#include "datomsGlBlock.h"
#include "datomsMotionEngine.h"
#include "deformationEvents.h"
#include "../../utils/trace.h"
#include "../../utils/configExporter.h"
#include "../../replay/replayExporter.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace Datoms
namespace Datoms {

#ifdef WIN32
    string directory = string(ROOT_DIR) + "/simulatorCore/resources/textures/datomsTextures";
#else
    string directory = "../../simulatorCore/resources/textures/datomsTextures";
#endif
/**
   \brief Constructor
   \param gridSize : size of the grid
   \param gridScale : size of a block
   \param argc : number of execution parameters
   \param argv : string array of parameters
*/
DatomsWorld::DatomsWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                             int argc, char *argv[]):World(argc, argv) {
    OUTPUT << TermColor::LifecycleColor << "DatomsWorld constructor" << TermColor::Reset << endl;

    if (GlutContext::GUIisEnabled) {
        //objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/datomsTextures","datomVS_piston.obj");
        //objBlock = new ObjLoader::ObjLoader(directory.c_str(),"datomsThick.obj");
        objBlock = new ObjLoader::ObjLoader(directory.c_str(),"datomVS_piston.obj");
        objBlockForPicking = new ObjLoader::ObjLoader(directory.c_str(), "datoms_picking.obj");
#ifdef WIN32
        string directory = string(ROOT_DIR) + "/simulatorCore/resources/textures/catoms3DTextures";
#else
        string directory = "../../simulatorCore/resources/textures/catoms3DTextures";
#endif
        objRepere = new ObjLoader::ObjLoader(directory.c_str(),"repereCatom3D.obj");
    }

    lattice = new SkewFCCLattice(gridSize, gridScale.isZero() ? defaultBlockSize : gridScale);
    motionRules = new DatomsMotionRules();
}

DatomsWorld::~DatomsWorld() {
    OUTPUT << "DatomsWorld destructor" << endl;
    /*	block linked are deleted by world::~world() */
}

void DatomsWorld::deleteWorld() {
    delete((DatomsWorld*)world);
}

void DatomsWorld::createPopupMenu(int ix, int iy) {
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

  // update rotateSubMenu depending on rotation datomsCapabilities
    DatomsBlock *bb = (DatomsBlock *)getSelectedBuildingBlock();
    vector<std::pair<const DatomsMotionRulesLink*, Deformation>> tab = DatomsMotionEngine::getAllDeformationsForModule(bb);
    int nbreMenus=tab.size();
    if (nbreMenus==0) {
        ((GlutButton*)GlutContext::popupMenu->getButton(6))->activate(false);
    } else {
        ((GlutButton*)GlutContext::popupMenu->getButton(6))->activate(true);
        GlutPopupMenuWindow *rotateBlockSubMenu = (GlutPopupMenuWindow*)GlutContext::popupMenu->getButton(6)->getChild(0);
        rotateBlockSubMenu->h = nbreMenus*35+10;
        rotateBlockSubMenu->clearChildren();
        rotateBlockSubMenu->show(false);
        int i=100;
        Cell3DPosition finalPos;
        short finalOrient;
        for(auto &elem:tab) {
            elem.second.init();
            elem.second.getFinalPositionAndOrientation(finalPos,finalOrient);
            cout << elem.first->getConFromID() << "=>" << elem.first->getConToID() << " " << finalPos << "," << finalOrient << endl;
            if (lattice->isInGrid(finalPos) && lattice->isFree(finalPos)) {
                    rotateBlockSubMenu->addButton(new GlutRotationButton(NULL,i++,0,0,0,0,"../../simulatorCore/resources/textures/menuTextures/menu_link.tga",
                true,elem.first->getConFromID(),elem.first->getConToID(),finalPos,finalOrient));
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

void DatomsWorld::menuChoice(int n) {
    DatomsBlock *bb = (DatomsBlock *)getSelectedBuildingBlock();
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
            addBlock(0, bb->buildNewBlockCode, nPos,bb->color,0);
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
            addBlock(0, bb->buildNewBlockCode,nPos,bb->color,bb->orientationCode);
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
            addBlock(0, bb->buildNewBlockCode,nPos,bb->color,orient);
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
                DatomsWorld *wrld = getWorld();
                wrld->disconnectBlock(bb, false);
                bb->setPositionAndOrientation(pos,orient);
                wrld->connectBlock(bb, false);
                //}
            } else World::menuChoice(n); // For all non-catoms2D-specific cases
        break;
    }
}

void DatomsWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                           uint8_t orient) {
    if (blockId > maxBlockId) {
        maxBlockId = blockId;
    } else if (blockId == 0) {
        blockId = incrementBlockId();
    }

    DatomsBlock *datom = new DatomsBlock(blockId,bcb);

    PistonId pId = (PistonId)(orient/64);
    orient = orient % 64;
    if (pId==0) pId=AllPistonsOff;
    cout << "ID=" << blockId << "  piston=" << pId << "  pos=" << pos << "  orient=" << orient << endl;

    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                            (datom->blockId, (BaseSimulator::BuildingBlock*)datom));



    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), datom));

    DatomsGlBlock *glBlock = new DatomsGlBlock(blockId);
    glBlock->setPosition(lattice->gridToWorldPosition(pos));
    glBlock->currentModel=pId;
    //datom->setModel(pId);
    datom->setGlBlock(glBlock);
    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeAddModule(getScheduler()->now(), blockId);
    datom->setPositionAndOrientation(pos,orient);
    datom->setColor(col);
    lattice->insert(datom, pos);

    mapGlBlocks.insert(make_pair(blockId,glBlock));
}

/**
 * \brief Connect the block placed on the cell at position pos
 */
void DatomsWorld::linkBlock(const Cell3DPosition& pos) {
    DatomsBlock *datom = (DatomsBlock *)lattice->getBlock(pos);

    if (datom) {
#ifdef verbose
        OUTPUT << "link datom " << datom->blockId << endl;
#endif
        Cell3DPosition neighborPos;
        DatomsBlock* neighborBlock;

        for (int i=0; i<12; i++) {
            if (datom->getNeighborPos(i,neighborPos)
                && (neighborBlock = (DatomsBlock *)lattice->getBlock(neighborPos))!=NULL) {
                datom->getInterface(i)->connect(neighborBlock->getInterface(pos));
#ifdef verbose
                OUTPUT << "connection #" << datom->blockId << "(" << i << ") to #"
                       << neighborBlock->blockId << endl;
#endif
            }
        }
    }
}

/**
 * \brief Draw catoms and axes
 */
void DatomsWorld::glDraw() {
// material for the grid walls
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);

// draw datoms
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((DatomsGlBlock*)pair.second)->glDraw(objBlock);
    }
    unlock();
    glPopMatrix();

    enableTexture(false);
    lattice->glDraw();

    BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
    if (bb) bb->blockCode->onGlDraw();
}

void DatomsWorld::glDrawId() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((DatomsGlBlock*)pair.second)->glDrawId(objBlock, pair.first);
    }
    unlock();
    glPopMatrix();
}

void DatomsWorld::glDrawIdByMaterial() {
    glPushMatrix();

    glDisable(GL_TEXTURE_2D);
    int n;
    lock();
    for (const auto& pair : mapGlBlocks) {
        n = pair.first*numPickingTextures;
        ((DatomsGlBlock*)pair.second)->glDrawIdByMaterial(objBlockForPicking,n);
    }
    unlock();
    glPopMatrix();
}

void DatomsWorld::glDrawBackground() {
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
        glScalef(0.2f,0.2f,0.2f);
        objRepere->glDraw();
        glPopMatrix();
}

void DatomsWorld::loadTextures(const string &str) {
    string path = str+"/hexa.tga";
    int lx,ly;
    idTextureHexa = loadTexture(path.c_str(),lx,ly);
    path = str+"/textureCarre.tga";
    idTextureGrid = loadTexture(path.c_str(),lx,ly);
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

void DatomsWorld::updateGlData(const DatomsBlock*blc, const Color &color) {
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

void DatomsWorld::updateGlData(const DatomsBlock*blc, PistonId id) {
    DatomsGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        glblc->currentModel = id;
        unlock();
    }
}


void DatomsWorld::setSelectedFace(int n) {
    numSelectedGlBlock = n / numPickingTextures;
    string name = objBlockForPicking->getObjMtlName(n%numPickingTextures);

    if (name == "Material__186") numSelectedFace = 0;
    else if (name == "connector1") numSelectedFace = 1;
    else if (name == "connector2") numSelectedFace = 2;
    else if (name == "connector3") numSelectedFace = 3;
    else if (name == "connector4") numSelectedFace = 4;
    else if (name == "connector5") numSelectedFace = 5;
    else if (name == "connector6") numSelectedFace = 6;
    else if (name == "connector7") numSelectedFace = 7;
    else if (name == "connector8") numSelectedFace = 8;
    else if (name == "connector9") numSelectedFace = 9;
    else if (name == "connector10") numSelectedFace = 10;
    else if (name == "connector11") numSelectedFace = 11;
    else {
        cerr << "warning: Unrecognized picking face" << endl;
        numSelectedFace = numPickingTextures;	// UNDEFINED
    }

    cerr << name << " = " << numSelectedFace << " = " << endl;
}

void DatomsWorld::exportConfiguration() {
    DatomsConfigExporter exporter = DatomsConfigExporter(this);
    exporter.exportConfiguration();
}

} // Datom namespace
