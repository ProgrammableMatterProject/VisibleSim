/**
 * @file   nodes2DWorld.cpp
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

#include "nodes2DWorld.h"
#include "nodes2DBlock.h"
#include "nodes2DGlBlock.h"
#include "nodes2DMotionEngine.h"
#include "../../utils/trace.h"
#include "../../utils/configExporter.h"
#include "../../replay/replayExporter.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace Nodes2D
namespace Nodes2D {

/**
   \brief Constructor
   \param gridSize : size of the grid
   \param gridScale : size of a block
   \param argc : number of execution parameters
   \param argv : string array of parameters
*/
    Nodes2DWorld::Nodes2DWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                               int argc, char *argv[]) : World(argc, argv) {
        OUTPUT << TermColor::LifecycleColor << "Nodes2DWorld constructor" << TermColor::Reset << endl;

        if (GlutContext::GUIisEnabled) {
            objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/nodes2DTextures",
                                                "nodes2D.obj");
            objBlockForPicking = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/nodes2DTextures",
                                                          "nodes2D_picking.obj");
            objRepere = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/latticeTextures",
                                                 "repere25.obj");
        }
        lattice = new SLattice(gridSize, gridScale.isZero() ? defaultBlockSize : gridScale);
        nodes2DMotionEngine = new Nodes2DMotionEngine();
    }

    Nodes2DWorld::~Nodes2DWorld() {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "Nodes2DWorld destructor" << endl;
#endif
        /*	block linked are deleted by world::~world() */
        delete nodes2DMotionEngine;
    }

    void Nodes2DWorld::deleteWorld() {
        delete ((Nodes2DWorld *) world);
    }

    void Nodes2DWorld::createPopupMenu(int ix, int iy) {
        if (!GlutContext::popupMenu) {
            GlutContext::popupMenu = new GlutPopupMenuWindow(NULL, 0, 0, 202, 215);
            // create submenu "Add"
            GlutPopupMenuWindow *addBlockSubMenu = new GlutPopupMenuWindow(NULL, 0, 0, 202, 112);
            addBlockSubMenu->id = 50;
            addBlockSubMenu->addButton(11, "../../simulatorCore/resources/textures/menuTextures/menu_add_normal.tga");
            addBlockSubMenu->addButton(12, "../../simulatorCore/resources/textures/menuTextures/menu_add_same.tga");
            addBlockSubMenu->addButton(13, "../../simulatorCore/resources/textures/menuTextures/menu_add_random.tga");
            // create submenu "Rotate"
            GlutPopupMenuWindow *rotateBlockSubMenu = new GlutPopupMenuWindow(NULL, 0, 0, 116, 40);
            rotateBlockSubMenu->id = 51;

            GlutContext::popupMenu->addButton(1, "../../simulatorCore/resources/textures/menuTextures/menu_add_sub.tga",
                                              addBlockSubMenu);
            GlutContext::popupMenu->addButton(2, "../../simulatorCore/resources/textures/menuTextures/menu_del.tga");
            GlutContext::popupMenu->addButton(6,
                                              "../../simulatorCore/resources/textures/menuTextures/menu_rotate_sub.tga",
                                              rotateBlockSubMenu);
            GlutContext::popupMenu->addButton(3, "../../simulatorCore/resources/textures/menuTextures/menu_tap.tga");
            GlutContext::popupMenu->addButton(4, "../../simulatorCore/resources/textures/menuTextures/menu_save.tga");
            GlutContext::popupMenu->addButton(5, "../../simulatorCore/resources/textures/menuTextures/menu_cancel.tga");
        }

        // update rotateSubMenu depending on rotation/translation Nodes2DCapabilities
        Nodes2DBlock *bb = (Nodes2DBlock *) getSelectedBuildingBlock();
        vector<Nodes2DMotion *> tab = getAllMotionsForModule(bb);
        int nbreMenus = tab.size();
        if (nbreMenus == 0) {
            ((GlutButton *) GlutContext::popupMenu->getButton(6))->activate(false);
        } else {
            ((GlutButton *) GlutContext::popupMenu->getButton(6))->activate(true);
            GlutPopupMenuWindow *rotateBlockSubMenu = (GlutPopupMenuWindow *) GlutContext::popupMenu->getButton(
                    6)->getChild(0);
            rotateBlockSubMenu->h = nbreMenus * 35 + 10;
            rotateBlockSubMenu->clearChildren();
            int i = 100;
            short orient;
            Cell3DPosition finalPos;
            for (auto &motion: tab) {
                orient = motion->isRotation ? (bb->orientationCode + (motion->direction == CW ? 1 : 3)) % 4
                                            : bb->orientationCode;
                finalPos = motion->finalPos + bb->position;
                cout << finalPos << "," << orient << endl;
                rotateBlockSubMenu->addButton(new GlutRotation2DButton(NULL, i++, 0, 0, 0, 0,
                                                                       "../../simulatorCore/resources/textures/menuTextures/menu_link_nodes2D.tga",
                                                                       motion->isRotation, motion->fromConId,
                                                                       motion->direction + 4, finalPos, orient,
                                                                       0.083333333));
            }
        }

        if (iy < GlutContext::popupMenu->h) iy = GlutContext::popupMenu->h;
        cerr << "Block " << numSelectedGlBlock << ":" << lattice->getDirectionString(numSelectedFace) << " selected"
             << endl;
        GlutContext::popupMenu->activate(1, canAddBlockToFace((int) numSelectedGlBlock, (int) numSelectedFace));
        GlutContext::popupMenu->setCenterPosition(ix, GlutContext::screenHeight - iy);
        GlutContext::popupMenu->show(true);
        if (GlutContext::popupSubMenu) GlutContext::popupSubMenu->show(false);
    }

    void Nodes2DWorld::menuChoice(int n) {
        Nodes2DBlock *bb = (Nodes2DBlock *) getSelectedBuildingBlock();
        Cell3DPosition nPos;
        switch (n) {
            case 1:
            case 6:
                GlutContext::popupMenu->show(true);
                GlutContext::popupSubMenu = (GlutPopupMenuWindow *) GlutContext::popupMenu->getButton(n)->getChild(0);
                GlutContext::popupSubMenu->show(true);
                GlutContext::popupSubMenu->x = GlutContext::popupMenu->x + GlutContext::popupMenu->w + 5;
                GlutContext::popupSubMenu->y = GlutContext::popupMenu->y + GlutContext::popupMenu->getButton(n)->y -
                                               GlutContext::popupSubMenu->h / 2;
                // avoid placing submenu over the top of the window
                if (GlutContext::popupSubMenu->y + GlutContext::popupSubMenu->h > GlutContext::screenHeight) {
                    GlutContext::popupSubMenu->y = GlutContext::screenHeight - GlutContext::popupSubMenu->h;
                }
                break;
            case 11:
                GlutContext::popupSubMenu->show(false);
                GlutContext::popupMenu->show(false);
                if (bb->getNeighborPos(SLattice::Direction(numSelectedFace), nPos)) {
                    addBlock(0, bb->buildNewBlockCode, nPos, bb->color, 0);
                    linkBlock(nPos);
                    linkNeighbors(nPos);
                } else {
                    cerr << "Position out of the grid" << endl;
                }
                break;
            case 12:
                GlutContext::popupSubMenu->show(false);
                GlutContext::popupMenu->show(false);
                if (bb->getNeighborPos(SLattice::Direction(numSelectedFace), nPos)) {
                    addBlock(0, bb->buildNewBlockCode, nPos, bb->color, bb->orientationCode);
                    linkBlock(nPos);
                    linkNeighbors(nPos);
                } else {
                    cerr << "Position out of the grid" << endl;
                }
                break;
            case 13:
                GlutContext::popupSubMenu->show(false);
                GlutContext::popupMenu->show(false);
                if (bb->getNeighborPos(SLattice::Direction(numSelectedFace), nPos)) {
                    int orient = rand() % 24;
                    addBlock(0, bb->buildNewBlockCode, nPos, bb->color, orient);
                    linkBlock(nPos);
                    linkNeighbors(nPos);
                } else {
                    cerr << "Position out of the grid" << endl;
                }
                break;
            default:
                if (n >= 100) {
                    GlutContext::popupSubMenu->show(false);
                    GlutContext::popupMenu->show(false);
                    // if (getScheduler()->state == RUNNING) {
                    // scheduler->schedule(new Rotation3DStartEvent(getScheduler()->now(), bb, Rotations3D r));
                    // } else {
                    Cell3DPosition pos = ((GlutRotationButton *) GlutContext::popupSubMenu->getButton(
                            n))->finalPosition;
                    short orient = ((GlutRotationButton *) GlutContext::popupSubMenu->getButton(n))->finalOrientation;
                    Nodes2DWorld *wrld = getWorld();
                    wrld->disconnectBlock(bb);
                    bb->setPositionAndOrientation(pos, orient);
                    wrld->connectBlock(bb);
                    //}
                } else World::menuChoice(n); // For all non-specific cases
                break;
        }
    }


    void Nodes2DWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                                uint8_t orientation) {
        if (blockId > maxBlockId)
            maxBlockId = blockId;
        else if (blockId == 0)
            blockId = incrementBlockId();

        Nodes2DBlock *module = new Nodes2DBlock(blockId, bcb);
        buildingBlocksMap.insert(std::pair<int, BaseSimulator::BuildingBlock *>
                                         (module->blockId, (BaseSimulator::BuildingBlock *) module));

        getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), module));

        Nodes2DGlBlock *glBlock = new Nodes2DGlBlock(blockId);
        mapGlBlocks.insert(make_pair(blockId, glBlock));
        module->setGlBlock(glBlock);
        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writeAddModule(getScheduler()->now(), blockId);
        module->setColor(col);
        module->setPositionAndOrientation(pos, orientation);
        lattice->insert(module, pos);
        glBlock->setPosition(lattice->gridToWorldPosition(pos));
        linkBlock(pos);

    }

/**
 * \brief Connect the block placed on the cell at position pos
 */

    /* revoir avec robotblock*/
    void Nodes2DWorld::linkBlock(const Cell3DPosition &pos) {
        Nodes2DBlock *module = (Nodes2DBlock *) lattice->getBlock(pos);
        Nodes2DBlock *neighborBlock;
        vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
        Cell3DPosition nPos;

        // Check neighbors for each interface
        for (int i = 0; i < SLattice::MAX_NB_NEIGHBORS; i++) {
            nPos = pos + nRelCells[i];
            neighborBlock = (Nodes2DBlock *) lattice->getBlock(nPos);
            if (neighborBlock) {
                module->getInterface(SLattice::Direction(i))->connect(
                        neighborBlock->getInterface(SLattice::Direction(lattice->getOppositeDirection(i))));

#ifdef DEBUG_NEIGHBORHOOD
                OUTPUT << "connection #" << module->blockId << ":" << lattice->getDirectionString(i) <<
                    " to #" << neighborBlock->blockId << ":"
                       << lattice->getDirectionString(lattice->getOppositeDirection(i)) << endl;
#endif
            } else {
                module->getInterface(SLattice::Direction(i))->connect(NULL);
            }
        }
    }

/**
 * \brief Draw modules and axes
 */
    void Nodes2DWorld::glDraw() {
        glDisable(GL_TEXTURE_2D);
        lock();
        for (const auto &pair: mapGlBlocks) {
            ((Nodes2DGlBlock *) pair.second)->glDraw(objBlock);
        }
        unlock();

        glPopMatrix();

        BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
        if (bb) bb->blockCode->onGlDraw();
    }

    void Nodes2DWorld::glDrawShadows() {
        glDisable(GL_TEXTURE_2D);
        lock();
        for (const auto &pair: mapGlBlocks) {
            ((Nodes2DGlBlock *) pair.second)->glDrawShadows(objBlockForPicking);
        }
        unlock();
    }

    void Nodes2DWorld::glDrawId() {
        glPushMatrix();
        glDisable(GL_TEXTURE_2D);
        lock();
        for (const auto &pair: mapGlBlocks) {
            ((Nodes2DGlBlock *) pair.second)->glDrawId(objBlock, pair.first);
        }
        unlock();
        glPopMatrix();
    }

    void Nodes2DWorld::glDrawIdByMaterial() {
        glPushMatrix();
        glDisable(GL_TEXTURE_2D);
        int m;
        lock();
        for (const auto &pair: mapGlBlocks) {
            m = pair.first * 5;
            ((Nodes2DGlBlock *) pair.second)->glDrawIdByMaterial(objBlockForPicking, m);
        }
        unlock();
        glPopMatrix();
    }

    void Nodes2DWorld::glDrawBackground() {
        static const GLfloat white[] = {1.0, 1.0, 1.0, 1.0},
                gray[] = {0.2, 0.2, 0.2, 1.0};

        glMaterialfv(GL_FRONT, GL_AMBIENT, gray);
        glMaterialfv(GL_FRONT, GL_DIFFUSE, white);
        glMaterialfv(GL_FRONT, GL_SPECULAR, gray);
        glMaterialf(GL_FRONT, GL_SHININESS, 40.0);
        glPushMatrix();
        enableTexture(true);
        glBindTexture(GL_TEXTURE_2D, idTextureWall);
        glNormal3f(0, 0, 1.0f);
        glScalef(lattice->gridSize[0] * lattice->gridScale[0],
                 lattice->gridSize[1] * lattice->gridScale[1], 1.0f);
        glBegin(GL_QUADS);
        glTexCoord2f(0, 0);
        glVertex3f(0.0f, 0.0f, 0.0f);
        glTexCoord2f(0.5 * lattice->gridSize[0], 0);
        glVertex3f(1.0f, 0.0f, 0.0f);
        glTexCoord2f(0.5 * lattice->gridSize[0], 0.5 * lattice->gridSize[1]);
        glVertex3f(1.0, 1.0, 0.0f);
        glTexCoord2f(0, 0.5 * lattice->gridSize[1]);
        glVertex3f(0.0, 1.0, 0.0f);
        glEnd();
        glPopMatrix();
        // draw the axes
        objRepere->glDraw();

        glPushMatrix();
    }

    void Nodes2DWorld::loadTextures(const string &str) {
        string path = str + "/texture_plane.tga";
        int lx, ly;
        idTextureWall = loadTexture(path.c_str(), lx, ly);
        path = str + "/../smartBlocksTextures/digits.tga";
        idTextureDigits = loadTexture(path.c_str(), lx, ly);
    }

    void Nodes2DWorld::updateGlData(Nodes2DBlock *blc, const Matrix &mat) {
        auto glblc = (Nodes2DGlBlock *) blc->getGlBlock();
        if (glblc) {
            lock();
            glblc->mat = mat;
            unlock();
        }
    }

    void Nodes2DWorld::setSelectedFace(int n) {
        cout << "SelectedFace(" << n << ")" << endl;

        numSelectedGlBlock = n / 5;
        string name = objBlockForPicking->getObjMtlName(n % 5);

        if (name == "NORTH") numSelectedFace = 0;
        else if (name == "EAST") numSelectedFace = 1;
        else if (name == "SOUTH") numSelectedFace = 2;
        else if (name == "WEST") numSelectedFace = 3;
        else {
            cerr << "warning: Unrecognized picking face" << endl;
            numSelectedFace = 4; // UNDEFINED
        }
        cerr << name << " => " << numSelectedFace << endl;
    }

    void Nodes2DWorld::exportConfiguration() {
        auto exporter = Nodes2DConfigExporter(this);
        exporter.exportConfiguration();
    }

    void Nodes2DWorld::disconnectBlock(BuildingBlock *block) {
        P2PNetworkInterface *fromBlock, *toBlock;

        for (int i = 0; i < block->getNbInterfaces(); i++) {
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
                fromBlock->connectedInterface = NULL;
                toBlock->connectedInterface = NULL;
            }
        }

        lattice->remove(block->position);
    }

    vector<Nodes2DMotion *> Nodes2DWorld::getAllMotionsForModule(Nodes2DBlock *nb) {
        return nodes2DMotionEngine->getAllMotionsForModule(nb, (SLattice *) lattice);
    }


} // Nodes2D namespace
