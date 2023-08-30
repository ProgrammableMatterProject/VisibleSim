/*
 * slidingCubesWorld.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include <string>
#include <stdlib.h>
#include <sys/types.h>
#include <signal.h>

#include "slidingCubesWorld.h"
#include "slidingCubesBlock.h"
#include "../../utils/trace.h"
#include "../../utils/configExporter.h"
#include "../../replay/replayExporter.h"

using namespace std;
//! \namespace SlidingCubes
namespace SlidingCubes {
#ifdef WIN32
    string textureDirectory = string(ROOT_DIR) + "/simulatorCore/resources/textures/";
#else
    string textureDirectory = "../../simulatorCore/resources/textures/";
#endif


    SlidingCubesWorld::SlidingCubesWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                         int argc, char *argv[]) : World(argc, argv) {
        OUTPUT << TermColor::LifecycleColor << "SlidingCubesWorld constructor" << TermColor::Reset << endl;

        if (GlutContext::GUIisEnabled) {
            string modelDir = textureDirectory + "slidingCubesTextures";
            objBlock = new ObjLoader::ObjLoader(modelDir.c_str(), "slidingCube.obj");
            objBlockForPicking = new ObjLoader::ObjLoader(modelDir.c_str(), "slidingCubePicking.obj");
            objRepere = new ObjLoader::ObjLoader(modelDir.c_str(), "repere25.obj");
        }
        lattice = new SCLattice2(gridSize, gridScale.isZero() ? defaultBlockSize : gridScale);
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
        delete ((SlidingCubesWorld *) world);
    }

    void SlidingCubesWorld::createPopupMenu(int ix, int iy) {
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

            GlutContext::popupMenu->addButton(1, menuTextureDirectory + "menu_add_sub_sc.tga", addBlockSubMenu);
            GlutContext::popupMenu->addButton(2, menuTextureDirectory + "menu_del.tga");
            GlutContext::popupMenu->addButton(6, menuTextureDirectory + "menu_move_sub.tga", rotateBlockSubMenu);
            GlutContext::popupMenu->addButton(3, menuTextureDirectory + "menu_tap.tga");
            GlutContext::popupMenu->addButton(4, menuTextureDirectory + "menu_save.tga");
            GlutContext::popupMenu->addButton(5, menuTextureDirectory + "menu_cancel.tga");
        }

        // update rotateSubMenu depending on rotation catoms3DCapabilities
        SlidingCubesBlock *sc = (SlidingCubesBlock *) getSelectedBuildingBlock();
        vector<SlidingCubesMotionRule *> tab = motionRules->getValidMotionList(sc);

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
            short finalOrient = 0;
            for (auto &rule: tab) {
                rule->getFinalPositionAndOrientation(sc, finalPos, finalOrient);
                cout << "printed: " << (rule->isRotation()?"Rot":"Trs") << ":" << rule->getToID() << "," << finalPos << endl;
                rotateBlockSubMenu->addButton(
                        new GlutRBMotionButton(nullptr, i++, 0, 0, 0, 0, menuTextureDirectory + "menu_move_rb.tga", rule->isRotation(),
                                               rule->getToID(), finalPos, finalOrient));
            }
        }

        if (iy < GlutContext::popupMenu->h) iy = GlutContext::popupMenu->h;
        cout << "Block " << numSelectedGlBlock << ":" << lattice->getDirectionString(numSelectedFace)
             << " selected" << endl;
        GlutContext::popupMenu->activate(1, canAddBlockToFace((int) numSelectedGlBlock, (int) numSelectedFace));
        GlutContext::popupMenu->setCenterPosition(ix, GlutContext::screenHeight - iy);
        GlutContext::popupMenu->show(true);
        if (GlutContext::popupSubMenu) GlutContext::popupSubMenu->show(false);
    }

    void SlidingCubesWorld::menuChoice(int n) {
        SlidingCubesBlock *rb = (SlidingCubesBlock *) getSelectedBuildingBlock();
        Cell3DPosition nPos;
        switch (n) {
            case 1 :
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
                if (rb->getNeighborPos(numSelectedFace, nPos)) {
                    addBlock(0, rb->buildNewBlockCode, nPos, rb->color, 0);
                    linkBlock(nPos);
                    linkNeighbors(nPos);
                } else {
                    cerr << "Position out of the grid" << endl;
                }
                break;
            case 12:
                GlutContext::popupSubMenu->show(false);
                GlutContext::popupMenu->show(false);
                if (rb->getNeighborPos(numSelectedFace, nPos)) {
                    addBlock(0, rb->buildNewBlockCode, nPos, rb->color, rb->orientationCode);
                    linkBlock(nPos);
                    linkNeighbors(nPos);
                } else {
                    cerr << "Position out of the grid" << endl;
                }
                break;
            case 13:
                GlutContext::popupSubMenu->show(false);
                GlutContext::popupMenu->show(false);
                if (rb->getNeighborPos(numSelectedFace, nPos)) {
                    int orient = rand() % 24; // 6 faces x 4 rotations/X
                    addBlock(0, rb->buildNewBlockCode, nPos, rb->color, orient);
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

                    Cell3DPosition pos = ((GlutRotationButton *) GlutContext::popupSubMenu->getButton(
                            n))->finalPosition;
                    short orient = ((GlutRotationButton *) GlutContext::popupSubMenu->getButton(
                            n))->finalOrientation;
                    SlidingCubesWorld *wrld = getWorld();
                    wrld->disconnectBlock(rb);
                    rb->setPositionAndOrientation(pos, orient);
                    wrld->connectBlock(rb);
                } else {
                    //cout << "menu world:" << n << endl;
                    World::menuChoice(n); // For all non-specific cases
                }
                break;
        }
    }

    void SlidingCubesWorld::addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos,
                                     const Color &col, uint8_t orient) {
        if (blockId > maxBlockId)
            maxBlockId = blockId;
        else if (blockId == 0)
            blockId = incrementBlockId();

        SlidingCubesBlock *sc = new SlidingCubesBlock(blockId, bcb);
        buildingBlocksMap.insert(std::pair<int, BaseSimulator::BuildingBlock *>
                                         (sc->blockId, (BaseSimulator::BuildingBlock *) sc));

        getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(), sc));

        SlidingCubesGlBlock *glBlock = new SlidingCubesGlBlock(blockId);
        mapGlBlocks.insert(make_pair(blockId, glBlock));
        sc->setGlBlock(glBlock);
        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writeAddModule(getScheduler()->now(), blockId);
        sc->setPosition(pos);
        sc->setPositionAndOrientation(pos, orient);
        sc->setColor(col);

        if (lattice->isInGrid(pos)) {
            lattice->insert(sc, pos);
        } else {
            ERRPUT << "ERROR : BLOCK #" << blockId << " out of the grid !!!!!" << endl;
            exit(1);
        }
    }

    void SlidingCubesWorld::linkBlock(const Cell3DPosition &pos) {
        SlidingCubesBlock *ptrNeighbor;
        SlidingCubesBlock *ptrBlock = (SlidingCubesBlock *) lattice->getBlock(pos);
        vector<Cell3DPosition> nRelCells = lattice->getRelativeConnectivity(pos);
        Cell3DPosition nPos;

        // Check neighbors for each interface
        for (int i = 0; i < 6; i++) {
            nPos = pos + nRelCells[i];
            ptrNeighbor = (SlidingCubesBlock *) lattice->getBlock(nPos);
            if (ptrNeighbor) {
                (ptrBlock)->getInterface(SCLattice2::Direction(i))->
                        connect(ptrNeighbor->getInterface(SCLattice2::Direction(
                        lattice->getOppositeDirection(i))));

#ifdef DEBUG_NEIGHBORHOOD
                OUTPUT << "connection #" << (ptrBlock)->blockId << ":" << int(i) << lattice->getDirectionString(i) <<
                    " to #" << ptrNeighbor->blockId << ":" << int(lattice->getOppositeDirection(i))
                       << lattice->getDirectionString(lattice->getOppositeDirection(i)) << endl;
#endif
            } else {
                (ptrBlock)->getInterface(SCLattice2::Direction(i))->connect(nullptr);
            }
        }
    }

    void SlidingCubesWorld::glDraw() {
        glPushMatrix();
        glDisable(GL_TEXTURE_2D);
        lock();
        for (const auto &pair: mapGlBlocks) {
            ((SlidingCubesGlBlock *) pair.second)->glDraw(objBlock);
        }
        unlock();

        BuildingBlock *bb = getSelectedBuildingBlock() ?: getMap().begin()->second;
        if (bb) bb->blockCode->onGlDraw();
        lattice->glDraw();
        glPopMatrix();

        if (GlutContext::editMode && bb) {
            Cell3DPosition nPos;
            if (bb->getNeighborPos(numSelectedFace, nPos)) {
                static const GLfloat transpRed[4] = {255.0, 0, 0, 0.5};
                Matrix mat = SlidingCubesBlock::getMatrixFromPositionAndOrientation(nPos, bb->orientationCode);
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

    void SlidingCubesWorld::glDrawId() {
        glPushMatrix();
        glDisable(GL_TEXTURE_2D);
        lock();
        for (const auto &pair: mapGlBlocks) {
            ((SlidingCubesGlBlock *) pair.second)->glDrawId(objBlock, pair.first);
        }
        unlock();
        glPopMatrix();
    }

    void SlidingCubesWorld::glDrawIdByMaterial() {
        glPushMatrix();

        glDisable(GL_TEXTURE_2D);
        int n;
        lock();
        for (const auto &pair: mapGlBlocks) {
            n = pair.first * numPickingTextures;
            ((SlidingCubesGlBlock *) pair.second)->glDrawIdByMaterial(objBlockForPicking, n);
        }
        unlock();
        glPopMatrix();
    }

    void SlidingCubesWorld::glDrawBackground() {
        static const GLfloat white[] = {0.8f, 0.8f, 0.8f, 1.0f},
                gray[] = {0.2f, 0.2f, 0.2f, 1.0f};
        float tx = lattice->gridSize[0] / 2.0, ty = lattice->gridSize[1] / 2.0, tz = lattice->gridSize[2] / 2.0;
        glMaterialfv(GL_FRONT, GL_AMBIENT, gray);
        glMaterialfv(GL_FRONT, GL_DIFFUSE, white);
        glMaterialfv(GL_FRONT, GL_SPECULAR, gray);
        glMaterialf(GL_FRONT, GL_SHININESS, 40.0);
        glPushMatrix();
        enableTexture(true);
        glBindTexture(GL_TEXTURE_2D, idTextureWall);
        glTranslatef(-0.5 * lattice->gridScale[0], -0.5 * lattice->gridScale[1], -0.5 * lattice->gridScale[2]);
        glScalef(lattice->gridSize[0] * lattice->gridScale[0],
                 lattice->gridSize[1] * lattice->gridScale[1],
                 lattice->gridSize[2] * lattice->gridScale[2]);
        glBegin(GL_QUADS);
        // bottom
        glNormal3f(0, 0, 1.0f);
        glTexCoord2f(0, 0);
        glVertex3f(0.0f, 0.0f, 0.0f);
        glTexCoord2f(tx, 0);
        glVertex3f(1.0f, 0.0f, 0.0f);
        glTexCoord2f(tx, ty);
        glVertex3f(1.0f, 1.0f, 0.0f);
        glTexCoord2f(0, ty);
        glVertex3f(0.0, 1.0f, 0.0f);
        // top
        glNormal3f(0, 0, -1.0f);
        glTexCoord2f(0, 0);
        glVertex3f(0.0f, 0.0f, 1.0f);
        glTexCoord2f(0, ty);
        glVertex3f(0.0, 1.0, 1.0f);
        glTexCoord2f(tx, ty);
        glVertex3f(1.0, 1.0, 1.0f);
        glTexCoord2f(tx, 0);
        glVertex3f(1.0f, 0.0f, 1.0f);
        // left
        glNormal3f(1.0, 0, 0);
        glTexCoord2f(0, 0);
        glVertex3f(0.0f, 0.0f, 0.0f);
        glTexCoord2f(ty, 0);
        glVertex3f(0.0f, 1.0f, 0.0f);
        glTexCoord2f(ty, tz);
        glVertex3f(0.0, 1.0, 1.0f);
        glTexCoord2f(0, tz);
        glVertex3f(0.0, 0.0, 1.0f);
        // right
        glNormal3f(-1.0, 0, 0);
        glTexCoord2f(0, 0);
        glVertex3f(1.0f, 0.0f, 0.0f);
        glTexCoord2f(0, tz);
        glVertex3f(1.0, 0.0, 1.0f);
        glTexCoord2f(ty, tz);
        glVertex3f(1.0, 1.0, 1.0f);
        glTexCoord2f(ty, 0);
        glVertex3f(1.0f, 1.0f, 0.0f);
        // back
        glNormal3f(0, -1.0, 0);
        glTexCoord2f(0, 0);
        glVertex3f(0.0f, 1.0f, 0.0f);
        glTexCoord2f(tx, 0);
        glVertex3f(1.0f, 1.0f, 0.0f);
        glTexCoord2f(tx, tz);
        glVertex3f(1.0f, 1.0, 1.0f);
        glTexCoord2f(0, tz);
        glVertex3f(0.0, 1.0, 1.0f);
        // front
        glNormal3f(0, 1.0, 0);
        glTexCoord2f(0, 0);
        glVertex3f(0.0f, 0.0f, 0.0f);
        glTexCoord2f(0, tz);
        glVertex3f(0.0, 0.0, 1.0f);
        glTexCoord2f(tx, tz);
        glVertex3f(1.0f, 0.0, 1.0f);
        glTexCoord2f(tx, 0);
        glVertex3f(1.0f, 0.0f, 0.0f);
        glEnd();
        glPopMatrix();
        // draw the axes
        glPushMatrix();
        glScalef(0.2f, 0.2f, 0.2f);
        objRepere->glDraw();
        glPopMatrix();
    }

    void SlidingCubesWorld::loadTextures(const string &str) {
        string path = str + "/texture_plane.tga";
        int lx, ly;
        idTextureWall = loadTexture(path.c_str(), lx, ly);
    }

    void SlidingCubesWorld::updateGlData(SlidingCubesBlock *blc, const Matrix &mat) {
        auto *glblc = (SlidingCubesGlBlock *) blc->getGlBlock();
        if (glblc) {
            lock();
            glblc->mat = mat;
            unlock();
        }
    }

    void SlidingCubesWorld::setSelectedFace(int n) {
        numSelectedGlBlock = n / numPickingTextures;
        string name = objBlockForPicking->getObjMtlName(n % numPickingTextures);
        cout << name << endl;
        if (name == "faceZ") numSelectedFace = SCLattice2::PlusZ;
        else if (name == "face_Z") numSelectedFace = SCLattice2::MinusZ;
        else if (name == "faceX") numSelectedFace = SCLattice2::PlusX;
        else if (name == "face_X") numSelectedFace = SCLattice2::MinusX;
        else if (name == "face_Y") numSelectedFace = SCLattice2::MinusY;
        else if (name == "faceY") numSelectedFace = SCLattice2::PlusY;
    }

    void SlidingCubesWorld::exportConfiguration() {
        SlidingCubesConfigExporter exporter = SlidingCubesConfigExporter(this);
        exporter.exportConfiguration();
    }

    void saveStlRect(ofstream &fout, const Vector3D &O, const Vector3D &u, const Vector3D &v, const Vector3D &N) {
        char buf[25];
        snprintf(buf, 25, "%5.3f %5.3f %5.3f", N[0], N[1], N[2]);
        fout << "          facet normal " << buf << endl;
        fout << "            outer loop" << endl;
        snprintf(buf, 25, "%5.3f %5.3f %5.3f", O[0], O[1], O[2]);
        fout << "              vertex " << buf << endl;
        snprintf(buf, 25, "%5.3f %5.3f %5.3f", O[0] + u[0], O[1] + u[1], O[2] + u[2]);
        fout << "              vertex " << buf << endl;
        snprintf(buf, 25, "%5.3f %5.3f %5.3f", O[0] + v[0], O[1] + v[1], O[2] + v[2]);
        fout << "              vertex " << buf << endl;
        fout << "            endloop" << endl;
        fout << "          endfacet" << endl;

        snprintf(buf, 25, "%5.3f %5.3f %5.3f", N[0], N[1], N[2]);
        fout << "          facet normal " << buf << endl;
        fout << "            outer loop" << endl;
        snprintf(buf, 25, "%5.3f %5.3f %5.3f", O[0] + u[0], O[1] + u[1], O[2] + u[2]);
        fout << "              vertex " << buf << endl;
        snprintf(buf, 25, "%5.3f %5.3f %5.3f", O[0] + u[0] + v[0], O[1] + u[1] + v[1], O[2] + u[2] + v[2]);
        fout << "              vertex " << buf << endl;
        snprintf(buf, 25, "%5.3f %5.3f %5.3f", O[0] + v[0], O[1] + v[1], O[2] + v[2]);
        fout << "              vertex " << buf << endl;
        fout << "            endloop" << endl;
        fout << "          endfacet" << endl;
    }

    void SlidingCubesWorld::createHelpWindow() {
        delete GlutContext::helpWindow;
        GlutContext::helpWindow = new GlutHelpWindow(nullptr,10,40,540,500,"slidingCubesHelp.txt");
    }

    bool SlidingCubesWorld::exportSTLModel(string title) {
        cout << "Writing STL output file..." << endl;
        Matrix mt;
        Vector3D pos, v1, v2, N;
        Cell3DPosition cell, neighborCell;


        // select slidingCube in the border
        vector<SlidingCubesBlock *> borderBlocks;
        cout << "step #1: " << endl;
        for (const std::pair<bID, BuildingBlock *> &pair: buildingBlocksMap) {
            if (pair.second->getState() != BuildingBlock::REMOVED
                and (pair.second->ptrGlBlock and pair.second->ptrGlBlock->isVisible())) {
                SlidingCubesBlock *rb = (SlidingCubesBlock *) pair.second;
                if (rb->getNbNeighbors() < 6) { // moins de 6 voisins
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
            pos.set(glblock->position[0], glblock->position[1], glblock->position[2]);
            cell = lattice->worldToGridPosition(pos);

            // top connector
            if (!block->getInterface(SCLattice2::Direction::PlusZ)->isConnected() ||
                block->getInterface(SCLattice2::Direction::PlusZ)->connectedInterface->hostBlock == nullptr) {
                pos.set(glblock->position[0] - 5.0, glblock->position[1] - 5.0, glblock->position[2] + 5.0);
                v1.set(10.0, 0.0, 0.0);
                v2.set(0.0, 10.0, 0.0);
                N.set(0.0, 0.0, 1.0);
                saveStlRect(file, pos, v1, v2, N);
            }
            // bottom connector
            if (!block->getInterface(SCLattice2::Direction::MinusZ)->isConnected() ||
                block->getInterface(SCLattice2::Direction::MinusZ)->connectedInterface->hostBlock == nullptr) {
                pos.set(glblock->position[0] + 5.0, glblock->position[1] - 5.0, glblock->position[2] - 5.0);
                v1.set(-10.0, 0.0, 0.0);
                v2.set(0.0, 10.0, 0.0);
                N.set(0.0, 0.0, -1.0);
                saveStlRect(file, pos, v1, v2, N);
            }
            // left connector
            if (!block->getInterface(SCLattice2::Direction::MinusX)->isConnected() ||
                block->getInterface(SCLattice2::Direction::MinusX)->connectedInterface->hostBlock == nullptr) {
                pos.set(glblock->position[0] - 5.0, glblock->position[1] + 5.0, glblock->position[2] - 5.0);
                v1.set(0.0, -10.0, 0.0);
                v2.set(0.0, 0.0, 10.0);
                N.set(-1.0, 0.0, 0.0);
                saveStlRect(file, pos, v1, v2, N);
            }
            // right connector
            if (!block->getInterface(SCLattice2::Direction::PlusX)->isConnected() ||
                block->getInterface(SCLattice2::Direction::PlusX)->connectedInterface->hostBlock == nullptr) {
                pos.set(glblock->position[0] + 5.0, glblock->position[1] - 5.0, glblock->position[2] - 5.0);
                v1.set(0.0, 10.0, 0.0);
                v2.set(0.0, 0.0, 10.0);
                N.set(1.0, 0.0, 0.0);
                saveStlRect(file, pos, v1, v2, N);
            }
            // back connector
            if (!block->getInterface(SCLattice2::Direction::PlusY)->isConnected() ||
                block->getInterface(SCLattice2::Direction::PlusY)->connectedInterface->hostBlock == nullptr) {
                pos.set(glblock->position[0] + 5.0, glblock->position[1] + 5.0, glblock->position[2] - 5.0);
                v1.set(-10.0, 0.0, 0.0);
                v2.set(0.0, 0.0, 10.0);
                N.set(0.0, -1.0, 0.0);
                saveStlRect(file, pos, v1, v2, N);
            }
            // front connector
            if (!block->getInterface(SCLattice2::Direction::MinusY)->isConnected() ||
                block->getInterface(SCLattice2::Direction::MinusY)->connectedInterface->hostBlock == nullptr) {
                pos.set(glblock->position[0] - 5.0, glblock->position[1] - 5.0, glblock->position[2] - 5.0);
                v1.set(10.0, 0.0, 0.0);
                v2.set(0.0, 0.0, 10.0);
                N.set(0.0, 1.0, 0.0);
                saveStlRect(file, pos, v1, v2, N);
            }
            file << "        endsolid rb#" << glblock->blockId << endl;
        }
        unlock();
        file.close();
        cout << "...done." << endl;

        return true;
    }

} // SlidingCube namespace
