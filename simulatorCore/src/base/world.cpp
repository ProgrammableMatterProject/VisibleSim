/*
 * world.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <cstdlib>

#include "world.h"
#include "../utils/trace.h"
#include "../gui/openglViewer.h"
#include "../replay/replayExporter.h"

using namespace std;

namespace BaseSimulator {

World *World::world = nullptr;
map<bID, BuildingBlock*>World::buildingBlocksMap;
unordered_map <bID, GlBlock*>World::mapGlBlocks;

World::World(int argc, char *argv[]) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "World constructor" << endl;
#endif
    selectedGlBlock = nullptr;
    numSelectedFace=0;
    numSelectedGlBlock=0;
    menuId = 0;

    if (world == nullptr) {
        world = this;

        if (GlutContext::GUIisEnabled) {
            GlutContext::init(argc,argv);
            camera = new Camera(-M_PI/2.0,M_PI/3.0,750.0);
            camera->setLightParameters(Vector3D(0,0,0),45.0,80.0,800.0,45.0,10.0,1500.0);
            camera->setTarget(Vector3D(0,0,1.0));
        }
    } else {
        ERRPUT << TermColor::ErrorColor << "Only one World instance can be created, aborting !" << TermColor::Reset << endl;
        exit(EXIT_FAILURE);
    }
}

World::~World() {
    // free building blocks
    std::map<bID, BuildingBlock*>::iterator it;
    for( it = buildingBlocksMap.begin() ; it != buildingBlocksMap.end() ; ++it) {
        delete it->second;
    }

    // free glBlocks
    for (const auto& pair : mapGlBlocks) {
        delete (GlBlock*)pair.second;
    }

    // /* free Scenario Events */
    // vector<ScenarioEvent*>::const_iterator it=tabEvents.begin();
    // while (it!=tabEvents.end()) {
    //  delete (*it);
    //  it++;
    // }
    // tabEvents.clear();

    delete lattice;
    delete camera;
    // delete [] targetGrid;
    delete objBlock;
    delete objBlockForPicking;
    delete objRepere;

#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "World destructor" << endl;
#endif
}


BuildingBlock* World::getBlockById(int bId) {
    map<bID, BuildingBlock*>::iterator it;
    it = buildingBlocksMap.find(bId);
    if (it == buildingBlocksMap.end()) {
        return(nullptr);
    } else {
        return(it->second);
    }
}

BuildingBlock* World::getBlockByPosition(const Cell3DPosition &pos) {
    map<bID, BuildingBlock*>::iterator it;
    for (it = buildingBlocksMap.begin(); it != buildingBlocksMap.end(); it++) {
        if (it->second->position == pos)
            return it->second;
    }
    return(nullptr);
}

void World::updateGlData(BuildingBlock *bb) {
    GlBlock *glblc = bb->getGlBlock();
    if (glblc) {
        lock();
        glblc->setPosition(lattice->gridToWorldPosition(bb->position));
        glblc->setColor(bb->color);
        unlock();
    }
}

void World::updateGlData(BuildingBlock *bb, const Color &c) {
    GlBlock *glblc = bb->getGlBlock();
    if (glblc) {
        //lock();
        glblc->setColor(bb->color);
        //unlock();
    }
}

void World::updateGlData(BuildingBlock *bb, const Cell3DPosition &p) {
    GlBlock *glblc = bb->getGlBlock();
    if (glblc) {
        //lock();
        glblc->setPosition(lattice->gridToWorldPosition(p));
        //unlock();
    }
}


void World::updateGlData(BuildingBlock*blc, Vector3D &p) {
    GlBlock *glblc = blc->getGlBlock();
    if (glblc) {
        //lock();
        glblc->setPosition(p);
        //unlock();
    }
}

void World::linkBlocks() {
    //TODO: Might not be necessary anymore, since a module is now linked to its neighbors when added to the lattice
    const Cell3DPosition& lb = lattice->getGridLowerBounds();
    const Cell3DPosition& ub = lattice->getGridUpperBounds();
    Cell3DPosition p;

    OUTPUT << "Link blocks..." << lb << "/" << ub << endl;
    for (p.pt[2] = lb.pt[2]; p[2] <= ub.pt[2]; p.pt[2]++) { // z
        for (p.pt[1] = lb.pt[1]; p[1] <= ub.pt[1]; p.pt[1]++) { // y
            for (p.pt[0] = lb.pt[0]; p[0] <= ub.pt[0]; p.pt[0]++) { // x
                if (lattice->cellHasBlock(p)) {
                    // cerr << "l.cellHasBlock(" << p << "/"
                    //   << lattice->getIndex(p) << ")  = true ; id: "
                    //	 << lattice->getBlock(p)->blockId << endl;

                    linkBlock(p);
                }
            }
        }
    }
}

void World::linkNeighbors(const Cell3DPosition &pos) {
    vector<Cell3DPosition> nCells = lattice->getActiveNeighborCells(pos);
    // Check neighbors for each interface
    for (Cell3DPosition nPos : nCells) {
        linkBlock(nPos);
    }
}


void World::connectBlock(BuildingBlock *block, bool count) {
    Cell3DPosition pos = block->position;
    //OUTPUT << "Connect Block " << block->blockId << " pos = " << pos << endl;
    lattice->insert(block, pos, count);
    linkBlock(pos);
    linkNeighbors(pos);
}

void World::disconnectBlock(BuildingBlock *block, bool count) {
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

    lattice->remove(block->position, count);

    OUTPUT << getScheduler()->now() << " : Disconnect Block " << block->blockId <<
        " pos = " << block->position << endl;
}

void World::deleteBlock(BuildingBlock *bb) {
    if (bb->getState() >= BuildingBlock::ALIVE ) {
        // cut links between bb and others and remove it from the grid
        disconnectBlock(bb);
        bb->setState(BuildingBlock::REMOVED);
    }

    if (selectedGlBlock == bb->ptrGlBlock) {
        selectedGlBlock = nullptr;
        GlutContext::mainWindow->select(nullptr);
    }

    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeRemoveModule(getScheduler()->now(), bb->blockId);


    // remove the associated glBlock
    lock();
    mapGlBlocks.erase(bb->blockId);
    unlock();

    delete bb->ptrGlBlock;
}

void World::stopSimulation() {
    map<bID, BuildingBlock*>::iterator it;
    for( it = buildingBlocksMap.begin() ; it != buildingBlocksMap.end() ; it++) {
        // it->second->stop();
    }
}

bool World::canAddBlockToFace(bID numSelectedGlBlock, uint8_t numSelectedFace) {
    BuildingBlock *bb = getBlockById(mapGlBlocks[numSelectedGlBlock]->blockId);
    Cell3DPosition nPos;
    bool isInGrid = bb->getNeighborPos(numSelectedFace,nPos);
    return isInGrid && lattice->isFree(nPos);
}

void World::menuChoice(int n) {
    BuildingBlock *bb = getSelectedBuildingBlock();

    switch (n) {
    case 1 : {
        OUTPUT << "ADD block link to : " << bb->blockId << "     num Face : " << int(numSelectedFace) << endl;
        Cell3DPosition nPos;
        if (bb->getNeighborPos(numSelectedFace,nPos)) {
            addBlock(0, bb->buildNewBlockCode, nPos, bb->color);
            linkBlock(nPos);
            linkNeighbors(nPos);
        } else {
            cerr << "Position out of the grid" << endl;
        }
    } break;
    case 2 : {
        OUTPUT << "DEL num block : " << int(mapGlBlocks[numSelectedGlBlock]->blockId) << endl;
        deleteBlock(bb);
    } break;
    case 3 : {
        tapBlock(getScheduler()->now(), bb->blockId, numSelectedFace);
    } break;
    case 4:                 // Save current configuration
        exportConfiguration();
        break;
    }
}

void World::createHelpWindow() {
    delete GlutContext::helpWindow;
    GlutContext::helpWindow = new GlutHelpWindow(nullptr,10,40,540,500,"genericHelp.txt");
}

void World::tapBlock(Time date, bID bId, int face) {
    BuildingBlock *bb = getBlockById(bId);
    cerr << bb->blockId << " : " << bb->position << " : " << face << endl;
    bb->tap(date, face < lattice->getMaxNumNeighbors() ? face : -1);
}

void World::addObstacle(const Cell3DPosition &pos,const Color &col) {
    bID blockId = incrementBlockId();

    GlBlock *glBlock = new GlBlock(blockId);
    Vector3D position(lattice->gridScale[0]*pos[0],
                      lattice->gridScale[1]*pos[1],
                      lattice->gridScale[2]*pos[2]);
    glBlock->setPosition(position);
    glBlock->setColor(col);

    lock();
    mapGlBlocks.insert(make_pair(blockId, glBlock));
    unlock();
}

void World::createPopupMenu(int ix, int iy) {
    if (!GlutContext::popupMenu) {
        GlutContext::popupMenu = new GlutPopupMenuWindow(nullptr,0,0,200,180);
        GlutContext::popupMenu->addButton(1,menuTextureDirectory+"menu_add.tga");
        GlutContext::popupMenu->addButton(2,menuTextureDirectory+"menu_del.tga");
        GlutContext::popupMenu->addButton(3,menuTextureDirectory+"menu_tap.tga");
        GlutContext::popupMenu->addButton(4,menuTextureDirectory+"menu_save.tga");
        GlutContext::popupMenu->addButton(5,menuTextureDirectory+"menu_cancel.tga");
    }

    if (iy < GlutContext::popupMenu->h) iy = GlutContext::popupMenu->h;
        cerr << "Block " << numSelectedGlBlock << ":" << lattice->getDirectionString(numSelectedFace)
         << " selected" << endl;
    // cerr << "Block " << numSelectedGlBlock << ":" << numSelectedFace << " selected" << endl;

    GlutContext::popupMenu->activate(1, canAddBlockToFace((int)numSelectedGlBlock,
                                                          (int)numSelectedFace));
    GlutContext::popupMenu->setCenterPosition(ix,GlutContext::screenHeight-iy);
    GlutContext::popupMenu->show(true);
}

void World::getBoundingBox(float &xmin,float &ymin,float &zmin,float &xmax,float &ymax,float &zmax) {
    lock();
    xmin = ymin = zmin = 10000;
    xmax = ymax = zmax = 0;
    float *pos;
    for (const auto& pair : mapGlBlocks) {
        pos = (pair.second)->position;
        if (xmin>pos[0]) xmin=pos[0];
        if (ymin>pos[1]) ymin=pos[1];
        if (zmin>pos[2]) zmin=pos[2];
        if (xmax<pos[0]) xmax=pos[0];
        if (ymax<pos[1]) ymax=pos[1];
        if (zmax<pos[2]) zmax=pos[2];
    }
    unlock();
}

} // BaseSimulator namespace
