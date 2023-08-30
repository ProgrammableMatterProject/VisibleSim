/**
 * @file   nodeWorld.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:59:44 2019
 *
 * @brief
 *
 *
 */

#include <iostream>
#include <string>
#include <stdlib.h>
//#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

#include "../../utils/trace.h"
#include "../../utils/configExporter.h"
#include "../../replay/replayExporter.h"
#include "../../events/events.h"
#include "hexanodesWorld.h"
#include "hexanodesBlock.h"
#include "hexanodesGlBlock.h"
#include "hexanodesMotionEngine.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace Hexanodes
namespace Hexanodes {
#ifdef WIN32
    string directory = string(ROOT_DIR) + "/simulatorCore/resources/textures/hexanodesTextures";
#else
    string directory = "../../simulatorCore/resources/textures/hexanodesTextures";
#endif
/**
   \brief Constructor
   \param gridSize : size of the grid
   \param gridScale : size of a block
   \param argc : number of execution parameters
   \param argv : string array of parameters
*/
HexanodesWorld::HexanodesWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                             int argc, char *argv[]):World(argc, argv) {
    OUTPUT << TermColor::LifecycleColor << "HexanodesWorld constructor" << TermColor::Reset << endl;

    if (GlutContext::GUIisEnabled) {
            objBlock = new ObjLoader::ObjLoader(directory.c_str(),"hexanodes.obj");
        objBlockForPicking = new ObjLoader::ObjLoader(directory.c_str(),"hexanodes_picking.obj");
        //objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures","repere25.obj");
    }
    lattice = new HHLattice(gridSize, gridScale.isZero() ? defaultBlockSize : gridScale);
        nodeMotionEngine = new HexanodesMotionEngine();
}

HexanodesWorld::~HexanodesWorld() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "HexanodesWorld destructor" << endl;
#endif
    /*	block linked are deleted by world::~world() */
        delete nodeMotionEngine;
}

void HexanodesWorld::deleteWorld() {
    delete((HexanodesWorld*)world);
}

void HexanodesWorld::createPopupMenu(int ix, int iy) {
    if (!GlutContext::popupMenu) {
        GlutContext::popupMenu = new GlutPopupMenuWindow(nullptr,0,0,202,215);
        // create submenu "Add"
        GlutPopupMenuWindow *addBlockSubMenu = new GlutPopupMenuWindow(nullptr,0,0,202,112);
        addBlockSubMenu->id=50;
        addBlockSubMenu->addButton(11,menuTextureDirectory + "menu_add_normal.tga");
        addBlockSubMenu->addButton(12,menuTextureDirectory + "menu_add_same.tga");
        addBlockSubMenu->addButton(13,menuTextureDirectory + "menu_add_random.tga");
        // create submenu "Rotate"
        GlutPopupMenuWindow *rotateBlockSubMenu = new GlutPopupMenuWindow(nullptr,0,0,116,40);
        rotateBlockSubMenu->id=51;

        GlutContext::popupMenu->addButton(1,menuTextureDirectory + "menu_add_sub.tga",addBlockSubMenu);
        GlutContext::popupMenu->addButton(2,menuTextureDirectory + "menu_del.tga");
        GlutContext::popupMenu->addButton(6,menuTextureDirectory + "menu_rotate_sub.tga",rotateBlockSubMenu);
        GlutContext::popupMenu->addButton(3,menuTextureDirectory + "menu_tap.tga");
        GlutContext::popupMenu->addButton(4,menuTextureDirectory + "menu_save.tga");
        GlutContext::popupMenu->addButton(5,menuTextureDirectory + "menu_cancel.tga");
    }

    // update rotateSubMenu depending on rotation/translation HexanodesCapabilities
    HexanodesBlock *bb = (HexanodesBlock *)getSelectedBuildingBlock();
    vector<HexanodesMotion*> tab = getAllMotionsForModule(bb);
    int nbreMenus=tab.size();
    if (nbreMenus==0) {
        ((GlutButton*)GlutContext::popupMenu->getButton(6))->activate(false);
    } else {
        ((GlutButton*)GlutContext::popupMenu->getButton(6))->activate(true);
        GlutPopupMenuWindow *rotateBlockSubMenu = (GlutPopupMenuWindow*)GlutContext::popupMenu->getButton(6)->getChild(0);
        rotateBlockSubMenu->h = nbreMenus*35+10;
        rotateBlockSubMenu->clearChildren();
        int i=100;
        short orient;
        Cell3DPosition finalPos;
        for(auto &motion:tab) {
            orient=bb->orientationCode+(motion->direction==motionDirection::CW?2:4)%6;
            finalPos = motion->getFinalPos(bb->position);
            lattice->getCellInDirection(bb->position,(motion->direction==motionDirection::CW?(motion->fromConId+1)%6:(motion->fromConId+5)%6));
            uint8_t idDest = (motion->fromConId+(motion->direction==motionDirection::CW?5:1))%6;
            //cout << motion->direction << "/" << motion->fromConId << "finalPos="<< finalPos << "," << orient << "(" << (motion->direction==motionDirection::CCW?(motion->fromConId+1)%6:(motion->fromConId+5)%6) << ")" << endl;
            if (lattice->isInGrid(finalPos) && lattice->isFree(finalPos)) {
                rotateBlockSubMenu->addButton(
                        new GlutRotation2DButton(nullptr,i++,0,0,0,0,menuTextureDirectory + "menu_link_hexanodes.tga",
                                                 motion->direction==CCW,motion->direction==CCW,2,finalPos,orient,0.06)
                        );
            }
        }
    }

    if (iy < GlutContext::popupMenu->h) iy = GlutContext::popupMenu->h;
    //cerr << "Block " << numSelectedGlBlock << ":" << lattice->getDirectionString(numSelectedFace) << " selected" << endl;
    GlutContext::popupMenu->activate(1, canAddBlockToFace((int)numSelectedGlBlock, (int)numSelectedFace));
    GlutContext::popupMenu->setCenterPosition(ix,GlutContext::screenHeight-iy);
    GlutContext::popupMenu->show(true);
    if (GlutContext::popupSubMenu) GlutContext::popupSubMenu->show(false);
}

void HexanodesWorld::menuChoice(int n) {
    HexanodesBlock *bb = (HexanodesBlock *)getSelectedBuildingBlock();
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
            if (bb->getNeighborPos(HHLattice::Direction(numSelectedFace),nPos)) {
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
            if (bb->getNeighborPos(HHLattice::Direction(numSelectedFace),nPos)) {
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
            if (bb->getNeighborPos(HHLattice::Direction(numSelectedFace),nPos)) {
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
                HexanodesWorld *wrld = getWorld();
                wrld->disconnectBlock(bb);
                bb->setPositionAndOrientation(pos,orient);
                wrld->connectBlock(bb);
                //}
            } else World::menuChoice(n); // For all non-specific cases
            break;
    }
}

void HexanodesWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                              uint8_t orient) {
    if (blockId > maxBlockId)
        maxBlockId = blockId;
    else if (blockId == 0)
        blockId = incrementBlockId();

    HexanodesBlock *module = new HexanodesBlock(blockId,bcb);
    buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>
                             (module->blockId, (BaseSimulator::BuildingBlock*)module));

    getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), module));

    HexanodesGlBlock *glBlock = new HexanodesGlBlock(blockId);
    mapGlBlocks.insert(make_pair(blockId, glBlock));
    module->setGlBlock(glBlock);
    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeAddModule(getScheduler()->now(), blockId);
    module->setColor(col);
    module->setPositionAndOrientation(pos,orient);
    lattice->insert(module, pos);
    glBlock->setPosition(lattice->gridToWorldPosition(pos));
    linkBlock(pos);

}

/**
 * \brief Connect the block placed on the cell at position pos
 */

 /* revoir avec robotblock*/
void HexanodesWorld::linkBlock(const Cell3DPosition& pos) {
    HexanodesBlock *module = (HexanodesBlock *)lattice->getBlock(pos);
    HexanodesBlock* neighborBlock;
    vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
    Cell3DPosition nPos;

    // Check neighbors for each interface
    for (int i = 0; i < HHLattice::MAX_NB_NEIGHBORS; i++) {
        nPos = pos + nRelCells[i];
        neighborBlock = (HexanodesBlock*)lattice->getBlock(nPos);
        if (neighborBlock) {
            module->getInterface(HHLattice::Direction(i))->connect(neighborBlock->getInterface(HHLattice::Direction(lattice->getOppositeDirection(i))));

#ifdef DEBUG_NEIGHBORHOOD
            OUTPUT << "connection #" << module->blockId << ":" << lattice->getDirectionString(i) <<
                " to #" << neighborBlock->blockId << ":"
                   << lattice->getDirectionString(lattice->getOppositeDirection(i)) << endl;
#endif
        } else {
            module->getInterface(HHLattice::Direction(i))->connect(nullptr);
        }
    }
}

/**
 * \brief Draw modules and axes
 */
void HexanodesWorld::glDraw() {
        glDisable(GL_TEXTURE_2D);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((HexanodesGlBlock*)pair.second)->glDraw(objBlock);
    }
    unlock();

    glPopMatrix();

    BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
    if (bb) bb->blockCode->onGlDraw();
}

void HexanodesWorld::glDrawShadows() {
    glDisable(GL_TEXTURE_2D);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((HexanodesGlBlock*)pair.second)->glDrawShadows(objBlockForPicking);
    }
    unlock();
}

void HexanodesWorld::glDrawId() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    lock();
    for (const auto& pair : mapGlBlocks) {
        ((HexanodesGlBlock*)pair.second)->glDrawId(objBlock, pair.first);
    }
    unlock();
    glPopMatrix();
}

void HexanodesWorld::glDrawIdByMaterial() {
    glPushMatrix();
    glDisable(GL_TEXTURE_2D);
    int m;
    lock();
    for (const auto& pair : mapGlBlocks) {
        m = pair.first*6;
        ((HexanodesGlBlock*)pair.second)->glDrawIdByMaterial(objBlockForPicking,m);
    }
    unlock();
    glPopMatrix();
}

void HexanodesWorld::glDrawBackground() {
    static const GLfloat white[]={1.0,1.0,1.0,1.0},
    gray[]={0.2,0.2,0.2,1.0},black[]={0.0,0.0,0.0,1.0};
    glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
    glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
    glMaterialfv(GL_FRONT,GL_SPECULAR,black);
    glMaterialf(GL_FRONT,GL_SHININESS,40.0);
    glPushMatrix();
    enableTexture(true);
    glBindTexture(GL_TEXTURE_2D,idTextureWall);
    glNormal3f(0,0,1.0f);
    //glScalef(lattice->gridSize[0]*lattice->gridScale[0],lattice->gridSize[1]*lattice->gridScale[1]*M_SQRT3_2,1.0f);

    glBegin(GL_QUADS);
    glTexCoord2f(0,0);
    glVertex3f(0.0f,0.0f,0.0f);
    glTexCoord2f(0.5*lattice->gridSize[0],0);
    glVertex3f(lattice->gridSize[0]*lattice->gridScale[0],0.0f,0.0f);
    glTexCoord2f(0.5*(lattice->gridSize[0]+0.5*lattice->gridSize[1]),0.5*lattice->gridSize[1]);
    glVertex3f((lattice->gridSize[0]+0.5*lattice->gridSize[1])*lattice->gridScale[0],lattice->gridSize[1]*lattice->gridScale[1]*M_SQRT3_2,0.0f);
    glTexCoord2f(0.25*lattice->gridSize[1],0.5*lattice->gridSize[1]);
    glVertex3f(0.5*lattice->gridSize[1]*lattice->gridScale[0],lattice->gridSize[1]*lattice->gridScale[1]*M_SQRT3_2,0.0f);
    glEnd();
    glPopMatrix();
    // draw the axes
    //objRepere->glDraw();

    glPushMatrix();
}

void HexanodesWorld::loadTextures(const string &str) {
    string path = str+"/hexanodesgrid.tga";
    int lx,ly;
    idTextureWall = loadTexture(path.c_str(),lx,ly);
    path=str+"/../smartBlocksTextures/digits.tga";
    idTextureDigits = loadTexture(path.c_str(),lx,ly);
}

void HexanodesWorld::updateGlData(BuildingBlock *bb) {
    HexanodesGlBlock *glblc = (HexanodesGlBlock*)bb->getGlBlock();
    if (glblc) {
            lock();
            //cout << "update pos:" << position << endl;
            glblc->setPosition(lattice->gridToWorldPosition(bb->position));
            glblc->setColor(bb->color);
            unlock();
    }
}

void HexanodesWorld::updateGlData(HexanodesBlock*blc, const Color &color) {
    HexanodesGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
            lock();
            //cout << "update pos:" << position << endl;
            glblc->setColor(color);
            unlock();
    }
}

void HexanodesWorld::updateGlData(HexanodesBlock*blc, bool visible) {
    HexanodesGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
            lock();
            //cout << "update pos:" << position << endl;
            glblc->setVisible(visible);
            unlock();
    }
}

void HexanodesWorld::updateGlData(HexanodesBlock*blc, const Vector3D &position) {
    HexanodesGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setPosition(position);
        unlock();
    }
}

void HexanodesWorld::updateGlData(HexanodesBlock*blc, const Cell3DPosition &position) {
    HexanodesGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        //cout << "update pos:" << position << endl;
        glblc->setPosition(lattice->gridToWorldPosition(position));
        unlock();
    }
}

void HexanodesWorld::updateGlData(HexanodesBlock*blc, const Matrix &mat) {
    HexanodesGlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        lock();
        glblc->mat = mat;
        unlock();
    }
}

void HexanodesWorld::setSelectedFace(int n) {
    cout << "SelectedFace("<< n << ")" <<endl;

    numSelectedGlBlock = n / 6;
        string name = objBlockForPicking->getObjMtlName(n%6);

    if (name == "NORTH_EAST") numSelectedFace = HHLattice::Direction::NORTH_EAST;
    else if (name == "EAST") numSelectedFace = HHLattice::Direction::EAST;
    else if (name == "SOUTH_EAST") numSelectedFace = HHLattice::Direction::SOUTH_EAST;
    else if (name == "SOUTH_WEST") numSelectedFace = HHLattice::Direction::SOUTH_WEST;
    else if (name == "WEST") numSelectedFace = HHLattice::Direction::WEST;
    else if (name == "NORTH_WEST") numSelectedFace = HHLattice::Direction::NORTH_WEST;
    else {
        cerr << "warning: Unrecognized picking face" << endl;
        numSelectedFace = HHLattice::Direction::MAX_NB_NEIGHBORS; // UNDEFINED
    }
        cerr << name << " => " << numSelectedFace << endl;
}

void HexanodesWorld::exportConfiguration() {
    HexanodesConfigExporter exporter = HexanodesConfigExporter(this);
    exporter.exportConfiguration();
}

void HexanodesWorld::disconnectBlock(BuildingBlock *block) {
    P2PNetworkInterface *fromBlock,*toBlock;

    for(int i = 0; i < block->getNbInterfaces(); i++) {
        fromBlock = block->getInterface(i);
        if (fromBlock && fromBlock->connectedInterface) {
            toBlock = fromBlock->connectedInterface;

            // Clear message queue
            fromBlock->outgoingQueue.clear();
            toBlock->outgoingQueue.clear();

            // Notify respective codeBlocks
            block->removeNeighbor(fromBlock);
            fromBlock->connectedInterface->hostBlock->removeNeighbor(fromBlock->connectedInterface);

            // Disconnect the interfaces
            fromBlock->connectedInterface = nullptr;
            toBlock->connectedInterface = nullptr;
        }
    }
    lattice->remove(block->position);
}

vector<HexanodesMotion*> HexanodesWorld::getAllMotionsForModule(HexanodesBlock*nb) {
    return nodeMotionEngine->getAllMotionsForModule(nb,(HHLattice*)lattice);
}


} // Hexanodes namespace
