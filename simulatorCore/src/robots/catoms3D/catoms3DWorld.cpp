/*!
 * \file catoms3DWorld.cpp
 * \brief catoms world
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#include <iostream>
#include <string>
#include <cstdlib>
#include <csignal>

#include "../../base/simulator.h"
#include "../../utils/trace.h"
#include "../../utils/configExporter.h"
#include "../../replay/replayExporter.h"
#include "catoms3DWorld.h"
#include "catoms3DBlock.h"
#include "catoms3DGlBlock.h"
#include "catoms3DMotionEngine.h"
#include "catoms3DRotationEvents.h"
#include "catoms3DSimulator.h"

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
                                 int argc, char *argv[]) : World(argc, argv) {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << TermColor::LifecycleColor << "Catoms3DWorld constructor" << TermColor::Reset << endl;
#endif
        idTextureGrid = 0;
        idTextureHexa = 0;
        if (GlutContext::GUIisEnabled) {
            string modelDir = textureDirectory + "catoms3DTextures";
/* Toggle to use catoms3D with max connector size (no rotation) but very simple models*/
#define CATOMS3D_TEXTURE_ID 0

#if CATOMS3D_TEXTURE_ID == 1 // Standard, but w/o conID
            objBlock = new ObjLoader::ObjLoader(directory.c_str(),"catom3DV2.obj");
            objBlockForPicking = new ObjLoader::ObjLoader(directory.c_str(),"catom3D_picking.obj");
#elif CATOMS3D_TEXTURE_ID == 2 // w/ coordinates
            objBlock = new ObjLoader::ObjLoader(directory.c_str(), "catom3Drepere3.obj");
            objBlockForPicking = new ObjLoader::ObjLoader(directory.c_str(),"catom_max_vs.obj");
#elif CATOMS3D_TEXTURE_ID == 3 // filled catoms
            objBlock = new ObjLoader::ObjLoader(directory.c_str(),"catom_max_vs.obj");
            objBlockForPicking = new ObjLoader::ObjLoader(directory.c_str(),"catom_max_vs.obj");
#else // 0 standard, with conIDs
            objBlock = new ObjLoader::ObjLoader(modelDir.c_str(), "catom3DV2connectorID.obj");
            objBlockForPicking = new ObjLoader::ObjLoader(modelDir.c_str(), "catom3D_picking.obj");
#endif

            if (Catoms3DSimulator::getSimulator()->useSkewedFCCLattice)
                objRepere = new ObjLoader::ObjLoader(modelDir.c_str(), "repereCatom3D_Zinc.obj");
            else
                objRepere = new ObjLoader::ObjLoader(modelDir.c_str(), "repereCatom3D.obj");
        }

        cout << "LatticeMode: "
             << (Catoms3DSimulator::getSimulator()->useSkewedFCCLattice ? "SkewedFCCLattice" : "FCCLattice") << endl;
        if (Catoms3DSimulator::getSimulator()->useSkewedFCCLattice)
            lattice = new SkewFCCLattice(gridSize, gridScale.isZero() ?
                                                   defaultBlockSize : gridScale);
        else
            lattice = new FCCLattice(gridSize, gridScale.isZero() ?
                                               defaultBlockSize : gridScale);

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
        delete ((Catoms3DWorld *) world);
    }

    void Catoms3DWorld::createPopupMenu(int ix, int iy) {
        if (!GlutContext::popupMenu) {
            GlutContext::popupMenu = new GlutPopupMenuWindow(nullptr, 0, 0, 202, 215);
            // create submenu "Add"
            GlutPopupMenuWindow *addBlockSubMenu = new GlutPopupMenuWindow(nullptr, 0, 0, 202, 112);
            addBlockSubMenu->id = 50;
            addBlockSubMenu->addButton(11, menuTextureDirectory + "menu_add_normal.tga");
            addBlockSubMenu->addButton(12, menuTextureDirectory + "menu_add_same.tga");
            addBlockSubMenu->addButton(13, menuTextureDirectory + "menu_add_random.tga");
            // create submenu "Rotate"
            GlutPopupMenuWindow *rotateBlockSubMenu = new GlutPopupMenuWindow(nullptr, 0, 0, 116, 40);
            rotateBlockSubMenu->id = 51;

            GlutContext::popupMenu->addButton(1, menuTextureDirectory + "menu_add_sub.tga", addBlockSubMenu);
            GlutContext::popupMenu->addButton(2, menuTextureDirectory + "menu_del.tga");
            GlutContext::popupMenu->addButton(6, menuTextureDirectory + "menu_rotate_sub.tga", rotateBlockSubMenu);
            GlutContext::popupMenu->addButton(3, menuTextureDirectory + "menu_tap.tga");
            GlutContext::popupMenu->addButton(4, menuTextureDirectory + "menu_save.tga");
            GlutContext::popupMenu->addButton(5, menuTextureDirectory + "menu_cancel.tga");
        }

        // update rotateSubMenu depending on rotation catoms3DCapabilities
        Catoms3DBlock *bb = (Catoms3DBlock *) getSelectedBuildingBlock();
        auto tab = Catoms3DMotionEngine::getAllRotationsForModule(bb);
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
            Cell3DPosition finalPos;
            short finalOrient;
            for (auto &elem: tab) {
                elem.second.init(((Catoms3DGlBlock *) bb->ptrGlBlock)->mat);
                elem.second.getFinalPositionAndOrientation(finalPos, finalOrient);
                if (lattice->isInGrid(finalPos) && lattice->isFree(finalPos)) {
                    rotateBlockSubMenu->addButton(
                            new GlutRotationButton(nullptr, i++, 0, 0, 0, 0, menuTextureDirectory + "menu_link.tga",
                                                   elem.first->isOctaFace(), elem.first->getConFromID(),
                                                   elem.first->getConToID(), finalPos, finalOrient));
                }
            }
        }

        if (iy < GlutContext::popupMenu->h) iy = GlutContext::popupMenu->h;
        cerr << "Block " << numSelectedGlBlock << ":" << lattice->getDirectionString(numSelectedFace)
             << " selected" << endl;
        GlutContext::popupMenu->activate(1, canAddBlockToFace((int) numSelectedGlBlock, (int) numSelectedFace));
        GlutContext::popupMenu->setCenterPosition(ix, GlutContext::screenHeight - iy);
        GlutContext::popupMenu->show(true);
        if (GlutContext::popupSubMenu) GlutContext::popupSubMenu->show(false);
    }

    void Catoms3DWorld::menuChoice(int n) {
        Catoms3DBlock *bb = (Catoms3DBlock *) getSelectedBuildingBlock();
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
                if (bb->getNeighborPos(numSelectedFace, nPos)) {
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
                if (bb->getNeighborPos(numSelectedFace, nPos)) {
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
                if (bb->getNeighborPos(numSelectedFace, nPos)) {
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
                    // scheduler->schedule(new Catoms3DRotationStartEvent(getScheduler()->now(), bb, Catoms3DRotation r));
                    // } else {
                    Cell3DPosition pos = ((GlutRotationButton *) GlutContext::popupSubMenu->getButton(
                            n))->finalPosition;
                    short orient = ((GlutRotationButton *) GlutContext::popupSubMenu->getButton(n))->finalOrientation;
                    Catoms3DWorld *wrld = getWorld();
                    wrld->disconnectBlock(bb, false);
                    bb->setPositionAndOrientation(pos, orient);
                    wrld->connectBlock(bb, false);
                    //}
                } else World::menuChoice(n); // For all non-catoms2D-specific cases
                break;
        }
    }


    void Catoms3DWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos,
                                 const Color &col, uint8_t orient) {
        if (blockId > maxBlockId)
            maxBlockId = blockId;
        else if (blockId == 0)
            blockId = incrementBlockId();

        Catoms3DBlock *catom = new Catoms3DBlock(blockId, bcb);
        buildingBlocksMap.insert(std::pair<int, BaseSimulator::BuildingBlock *>
                                         (catom->blockId, (BaseSimulator::BuildingBlock *) catom));

        // // FIXME: Adversarial start, randomly initiate start event
        // std::mt19937 rng;
        // rng.seed(Simulator::getSimulator()->getSimulationSeed());
        // std::uniform_int_distribution<std::mt19937::result_type> u500(0,500);
        // getScheduler()->schedule(new CodeStartEvent(getScheduler()->now() + u500(rng), catom));
        // getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), catom));
        getScheduler()->schedule(new CodeStartEvent(getScheduler()->now() + 1000, catom));

        Catoms3DGlBlock *glBlock = new Catoms3DGlBlock(blockId);
        glBlock->setPosition(pos);

        catom->setGlBlock(glBlock);
        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writeAddModule(getScheduler()->now(), blockId);
        catom->setPositionAndOrientation(pos, orient);
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
    void Catoms3DWorld::linkBlock(const Cell3DPosition &pos) {
        Catoms3DBlock *catom = (Catoms3DBlock *) lattice->getBlock(pos);

        if (catom) {
#ifdef DEBUG_NEIGHBORHOOD
            OUTPUT << "link catom " << catom->blockId << endl;
#endif

            Cell3DPosition neighborPos;
            Catoms3DBlock *neighborBlock;
            for (int i = 0; i < 12; i++) {
                if (catom->getNeighborPos(i, neighborPos)
                    && (neighborBlock = (Catoms3DBlock *) lattice->getBlock(neighborPos)) != nullptr) {
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
        for (const auto &pair: mapGlBlocks) {
            if (pair.second->isVisible()) ((Catoms3DGlBlock *) pair.second)->glDraw(objBlock);
        }
        unlock();
        glPopMatrix();

        BuildingBlock *bb = getSelectedBuildingBlock();
        if (bb == nullptr && !getMap().empty()) bb = getMap().begin()->second;
        if (bb != nullptr) bb->blockCode->onGlDraw();
        lattice->glDraw();

        if (GlutContext::editMode && numSelectedFace != 13 && bb) {
            Cell3DPosition nPos;
            if (bb->getNeighborPos(numSelectedFace, nPos)) {
                static const GLfloat transpRed[4] = {255.0, 0, 0, 0.5};
                Matrix mat = Catoms3DBlock::getMatrixFromPositionAndOrientation(nPos, bb->orientationCode);
                glPushMatrix();
                mat.glMultMatrix();
                objBlock->setLightedColor(transpRed);
                objBlock->glDraw();
                glPopMatrix();
            } else {
                cerr << "Position out of the grid" << endl;
            }
        }
    }

    void Catoms3DWorld::glDrawId() {
        glPushMatrix();
        glDisable(GL_TEXTURE_2D);
        lock();
        for (const auto &pair: mapGlBlocks) {
            ((Catoms3DGlBlock *) pair.second)->glDrawId(objBlock, pair.first);
        }
        unlock();
        glPopMatrix();
    }

    void Catoms3DWorld::glDrawIdByMaterial() {
        glPushMatrix();
        glDisable(GL_TEXTURE_2D);
        lock();
        int n;
        for (const auto &pair: mapGlBlocks) {
            n = pair.first * numPickingTextures;
            ((Catoms3DGlBlock *) pair.second)->glDrawIdByMaterial(objBlockForPicking, n);
        }
        unlock();
        glPopMatrix();
    }

    void Catoms3DWorld::glDrawBackground() {
        static const GLfloat white[] = {0.8f, 0.8f, 0.8f, 1.0f},
                gray[] = {0.2f, 0.2f, 0.2f, 1.0f};
        glMaterialfv(GL_FRONT, GL_AMBIENT, gray);
        glMaterialfv(GL_FRONT, GL_DIFFUSE, white);
        glMaterialfv(GL_FRONT, GL_SPECULAR, white);
        glMaterialf(GL_FRONT, GL_SHININESS, 40.0);
        glPushMatrix();
        enableTexture(true);
        glBindTexture(GL_TEXTURE_2D, idTextureGrid);
        glTranslatef(0, 0, lattice->gridScale[2] * (0.5 - M_SQRT2_2));
        glScalef(lattice->gridSize[0] * lattice->gridScale[0], lattice->gridSize[1] * lattice->gridScale[1],
                 lattice->gridSize[2] * lattice->gridScale[2] * M_SQRT2_2);
        glBegin(GL_QUADS);
        // bottom
        glNormal3f(0, 0, 1.0f);
        glTexCoord2f(0, 0);
        glVertex3f(0.0f, 0.0f, -0.0f);
        glTexCoord2f(0.5f * lattice->gridSize[0], 0);
        glVertex3f(1.0f, 0.0f, 0.0f);
        glTexCoord2f(0.5f * lattice->gridSize[0], 0.5f * lattice->gridSize[1]);
        glVertex3f(1.0, 1.0, 0.0f);
        glTexCoord2f(0, 0.5f * lattice->gridSize[1]);
        glVertex3f(0.0, 1.0, 0.0f);
        // top
        glNormal3f(0, 0, -1.0f);
        glTexCoord2f(0, 0);
        glVertex3f(0.0f, 0.0f, 1.0f);
        glTexCoord2f(0.5f * lattice->gridSize[0], 0);
        glVertex3f(0.0, 1.0, 1.0f);
        glTexCoord2f(0.5f * lattice->gridSize[0], 0.5f * lattice->gridSize[1]);
        glVertex3f(1.0, 1.0, 1.0f);
        glTexCoord2f(0, 0.5f * lattice->gridSize[1]);
        glVertex3f(1.0f, 0.0f, 1.0f);
        glEnd();
        // draw hexa
        glBindTexture(GL_TEXTURE_2D, idTextureHexa);
        glBegin(GL_QUADS);
        // left
        glNormal3f(1.0f, 0, 0);
        glTexCoord2f(0, 0);
        glVertex3f(0.0f, 0.0f, 0.0f);
        glTexCoord2f(lattice->gridSize[1] / 3.0f, 0);
        glVertex3f(0.0f, 1.0f, 0.0f);
        glTexCoord2f(lattice->gridSize[1] / 3.0f, lattice->gridSize[2] / 1.5f);
        glVertex3f(0.0, 1.0, 1.0f);
        glTexCoord2f(0, lattice->gridSize[2] / 1.5f);
        glVertex3f(0.0, 0.0, 1.0f);
        // right
        glNormal3f(-1.0f, 0, 0);
        glTexCoord2f(0, 0);
        glVertex3f(1.0f, 0.0f, 0.0f);
        glTexCoord2f(0, lattice->gridSize[2] / 1.5f);
        glVertex3f(1.0, 0.0, 1.0f);
        glTexCoord2f(lattice->gridSize[1] / 3.0f, lattice->gridSize[2] / 1.5f);
        glVertex3f(1.0, 1.0, 1.0f);
        glTexCoord2f(lattice->gridSize[1] / 3.0f, 0);
        glVertex3f(1.0f, 1.0f, 0.0f);
        // back
        glNormal3f(0, -1.0f, 0);
        glTexCoord2f(0, 0);
        glVertex3f(0.0f, 1.0f, 0.0f);
        glTexCoord2f(lattice->gridSize[0] / 3.0f, 0);
        glVertex3f(1.0f, 1.0f, 0.0f);
        glTexCoord2f(lattice->gridSize[0] / 3.0f, lattice->gridSize[2] / 1.5f);
        glVertex3f(1.0f, 1.0, 1.0f);
        glTexCoord2f(0, lattice->gridSize[2] / 1.5f);
        glVertex3f(0.0, 1.0, 1.0f);
        // front
        glNormal3f(0, 1.0, 0);
        glTexCoord2f(0, 0);
        glVertex3f(0.0f, 0.0f, 0.0f);
        glTexCoord2f(0, lattice->gridSize[2] / 1.5f);
        glVertex3f(0.0, 0.0, 1.0f);
        glTexCoord2f(lattice->gridSize[0] / 3.0f, lattice->gridSize[2] / 1.5f);
        glVertex3f(1.0f, 0.0, 1.0f);
        glTexCoord2f(lattice->gridSize[0] / 3.0f, 0);
        glVertex3f(1.0f, 0.0f, 0.0f);
        glEnd();
        glPopMatrix();
    }

    void Catoms3DWorld::loadTextures(const string &str) {
        string path = str + "//hexa.tga";
        int lx, ly;
        idTextureHexa = loadTexture(path.c_str(), lx, ly);
        path = str + "//textureCarre.tga";
        idTextureGrid = loadTexture(path.c_str(), lx, ly);
    }

    void Catoms3DWorld::updateGlData(BuildingBlock *bb) {
        auto *glblc = (Catoms3DGlBlock *) bb->getGlBlock();
        if (glblc) {
            lock();
            //cout << "update pos:" << position << endl;
            glblc->setPosition(bb->position);
            glblc->setColor(bb->color);
            unlock();
        }
    }

    void Catoms3DWorld::updateGlData(Catoms3DBlock *blc, const Color &color) {
        auto *glblc = (Catoms3DGlBlock *) blc->getGlBlock();
        if (glblc) {
            lock();
            //cout << "update pos:" << position << endl;
            glblc->setColor(color);
            unlock();
        }
    }

    void Catoms3DWorld::updateGlData(Catoms3DBlock *blc, bool visible) {
        auto *glblc = (Catoms3DGlBlock *) blc->getGlBlock();
        if (glblc) {
            lock();
            //cout << "update pos:" << position << endl;
            glblc->setVisible(visible);
            unlock();
        }
    }

    void Catoms3DWorld::updateGlData(Catoms3DBlock *blc, const Vector3D &position) {
        auto *glblc = (Catoms3DGlBlock *) blc->getGlBlock();
        if (glblc) {
            lock();
            //cout << "update pos:" << position << endl;
            glblc->setPosition(position);
            unlock();
        }
    }

    void Catoms3DWorld::updateGlData(Catoms3DBlock *blc, const Cell3DPosition &position) {
        auto *glblc = (Catoms3DGlBlock *) blc->getGlBlock();
        if (glblc) {
            lock();
            //cout << "update pos:" << position << endl;
            glblc->setPosition(position);
            unlock();
        }
    }

    void Catoms3DWorld::updateGlData(Catoms3DBlock *blc, const Matrix &mat) {
        auto *glblc = (Catoms3DGlBlock *) blc->getGlBlock();
        if (glblc) {
            lock();
            glblc->mat = mat;
            unlock();
        }
    }

    void Catoms3DWorld::setSelectedFace(int n) {
        numSelectedGlBlock = n / numPickingTextures;
        string name = objBlockForPicking->getObjMtlName(n % numPickingTextures);

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
            // cerr << "warning: Unrecognized picking face" << endl;
            numSelectedFace = 13;    // UNDEFINED
        }
        // cerr << name << " => " << numSelectedFace << endl;
    }

    void Catoms3DWorld::exportConfiguration() {
        auto exporter = Catoms3DConfigExporter(this);
        exporter.exportConfiguration();
    }

    void Catoms3DWorld::createHelpWindow() {
        delete GlutContext::helpWindow;
        GlutContext::helpWindow = new GlutHelpWindow(nullptr,10,40,540,500,"catoms3DHelp.txt");
    }

    vector<Vector3D> tabAddCenter;
    bool addAdd(ObjLoader::ObjData *obj, const Vector3D &pos) {

        const ObjLoader::Point3 *c = obj->getCenter();
        Vector3D center(pos[0] + c->v[0], pos[1] + c->v[1], pos[2] + c->v[2]);

        // calculate distance to min center
        double dmin = 1e32, d;
        for (Vector3D v: tabAddCenter) {
            d = (v - center).norme2();
            if (d < dmin) {
                dmin = d;
            }
        }
        if (dmin > 0.1) {
            tabAddCenter.push_back(center);
            return true;
        }

        return false;
    }

/*bool Catoms3DWorld::exportSTLModel(string title) {
  const char* connectorStr[12]={"connecteur00","connecteur01","connecteur02","connecteur03","connecteur04","connecteur05","connecteur06","connecteur07","connecteur08","connecteur09","connecteur10","connecteur11"};
    const char* refStr[6]={"ref0","ref1","ref2","ref3","ref4","ref5"};
    const char* cornerStr[8]={"corner0","corner1","corner2","corner3","corner4","corner5","corner6","corner7"};
    uint8_t tabRefConId[6][4] = {{2,3,4,5},{0,2,1,10},{0,9,7,5},{4,7,8,6},{1,3,6,11},{8,9,10,11}};
    uint8_t tabCornerConId[8][3] = {{0,2,5},{0,9,10},{3,4,6},{6,8,11},{1,2,3},{1,10,11},{4,5,7},{7,8,9}};
    uint8_t tabRefCornerId[6][4] = {{0,2,4,6},{0,1,4,5},{0,1,6,7},{2,3,6,7},{4,5,2,3},{1,5,3,7}};

    bool memRefs[6];
    bool memCorners[8];

    cout << "Writing STL output file..." << endl;
    ObjLoader::ObjLoader stl((textureDirectory+"catoms3DTextures").c_str(),"catoms3D_stl_model.obj");
    ObjLoader::ObjData* tabConnectors[12];
    ObjLoader::ObjData* tabRefs[6];
    ObjLoader::ObjData* tabCorners[8];
    ObjLoader::ObjData* tabAdds[6][4];

    Matrix mt;
    Vector3D pos;
    Cell3DPosition cell,neighborCell;


    for (int i=0; i<12; i++) {
        tabConnectors[i] = stl.getObjDataByName(connectorStr[i]);
    }
    for (int i=0; i<6; i++) {
        tabRefs[i] = stl.getObjDataByName(refStr[i]);
    }
    for (int i=0; i<8; i++) {
        tabCorners[i] = stl.getObjDataByName(cornerStr[i]);
    }
    string str;
    for (int i=0; i<6; i++) {
        for (int j=0; j<4; j++) {
            str = "add" + to_string(i) + to_string(tabRefCornerId[i][j]);
            tabAdds[i][j] = stl.getObjDataByName(str);
        }
    }

    // select catom in the border
    vector <Catoms3DBlock*> borderBlocks;
    cout << "step #1: " << endl;
    for (const auto& pair : buildingBlocksMap) {
        if (pair.second->getState() != BuildingBlock::REMOVED
            and (pair.second->ptrGlBlock and pair.second->ptrGlBlock->isVisible())) {
            auto *bb = (Catoms3DBlock *)pair.second;
            if (bb->getNbNeighbors()<12) { // soit 12 voisins
                bb->setColor(RED);
                borderBlocks.push_back(bb);
            } else { // soit Il existe un ref qui a 4 voisins borders
                int i=0;
                bool visible = false;
                Catoms3DBlock *neighbor;
                while (i<6 && !visible) {
                    // C'est suffisant de tester un connecteur
                    neighbor = (Catoms3DBlock *)bb->getInterface(tabRefConId[i][0])->connectedInterface->hostBlock;
                    visible = !neighbor->getInterface(tabRefConId[i][2])->isConnected();
                    i++;
                }
                if (visible) {
                    bb->setColor(YELLOW);
                    borderBlocks.push_back(bb);
                } else {
                    bb->setColor(WHITE);
                }
            }
        }
    }
    cout << "border blocks: " << borderBlocks.size() << "/" << buildingBlocksMap.size() << endl;

    cout << "step #2: " << endl;
    int loop=0,nbreLoop=borderBlocks.size();
    ofstream file(title);
    if (!file.is_open()) return false;

    lock();
    for (auto block: borderBlocks) {
        auto *glblock = (Catoms3DGlBlock*)block->getGlBlock();
        file << "solid catom#" << glblock->blockId << endl;
        pos.set(glblock->position[0],glblock->position[1],glblock->position[2]);
        cell = lattice->worldToGridPosition(pos);
        // refs si l'un des 4 voisins est absent ou bien ils sont la mais il n'y a pas d'opposé
        bool hidden=true;
        BuildingBlock *neighbor;
        for (int j=0; j<6; j++) {
            hidden=true;
            int i=0;
            while (i<4 && hidden) {
                hidden = block->getInterface(tabRefConId[j][i])->isConnected();
                i++;
            }
            if (hidden) {
                neighbor = block->getInterface(tabRefConId[j][0])->connectedInterface->hostBlock;
                hidden = neighbor->getInterface(tabRefConId[j][2])->isConnected();
            }
            if (!hidden) {
                tabRefs[j]->saveSTLfacets(file,pos,0);
            }
            memRefs[j] = !hidden;
        }
        // connectors
        for (int i=0; i<12; i++) {
            if (!block->getInterface(i)->isConnected()) {
                tabConnectors[i]->saveSTLfacets(file,pos,0);
            }
        }
        // corners
        for (int i=0; i<8; i++) {
            int j=0;
            bool hidden=true;
            while (j<3 && hidden) {
                hidden = block->getInterface(tabCornerConId[i][j])->isConnected();
                j++;
            }
            if (!hidden) {
                tabCorners[i]->saveSTLfacets(file,pos,0);
            }
            memCorners[i] = !hidden;
        }

        // adds
        for (int i=0; i<6; i++) {
            if (memRefs[i]) {
                for (int j=0; j<4; j++) {
                    if (!memCorners[tabRefCornerId[i][j]]) {
                        if (addAdd(tabAdds[i][j],pos)) {
                            tabAdds[i][j]->saveSTLfacets(file,pos,0);
                        }
                    }
                }
            } else {
                BuildingBlock *neighbor;
                int corner,n;
                for (int j=0; j<4; j++) {
                    corner = tabRefCornerId[i][j];
                    n=0;
                    for (int j=0; j<3; j++) {
                        if (block->getInterface(tabCornerConId[corner][j])->isConnected()) {
                            neighbor = block->getInterface(tabCornerConId[corner][j])->connectedInterface->hostBlock;
                            if (neighbor->getNbNeighbors()<12) {
                                n++;
                            } else {
                                n-=3; // cancel
                            }
                        }
                    }
                    if (n==2 && addAdd(tabAdds[i][j],pos)) {
                        tabAdds[i][j]->saveSTLfacets(file,pos,0,-1,true);
                    }
                }
            }
        }

        file << "        endsolid catom#" << glblock->blockId << endl;

        int p0 = (10*loop)/nbreLoop;
        loop++;
        int p1 = (10*loop)/nbreLoop;

        if (p0!=p1) {
            cout << p1*10 << "%" << endl;
        }
    }
    unlock();
    file.close();
    cout << "...done." << endl;

    tabAddCenter.clear();

    return true;
}*/


} // Catoms3DBlock namespace
