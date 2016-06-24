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

using namespace std;

namespace SmartBlocks {

SmartBlocksWorld::SmartBlocksWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                   int argc, char *argv[]):World(argc, argv) {
    cout << "\033[1;31mSmartBlocksWorld constructor\033[0m" << endl;

    idTextureFloor=0;
    idTextureDigits=0;
#ifdef GLUT
    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/smartBlocksTextures","smartBlockSimple.obj");
    objRepere = new ObjLoader::ObjLoader("../../simulatorCore/smartBlocksTextures","repere25.obj");
#else
    objBlock=NULL;
    objRepere=NULL;
#endif

    nbreStats=0;
    for (int i = 0; i < 10; i++) {
        tabStatsData[i] = 0;
    }

    lattice = new SLattice(gridSize, gridScale.hasZero() ? defaultBlockSize : gridScale);
    targetGrid = NULL;
}

SmartBlocksWorld::~SmartBlocksWorld() {
    cout << "\033[1;31mSmartBlocksWorld destructor" << endl;

/* block linked are deleted by world::~world() */
    delete objBlock;
    delete objRepere;
    delete [] targetGrid;
    delete capabilities;
}

void SmartBlocksWorld::deleteWorld() {
    delete((SmartBlocksWorld*)world);
    world=NULL;
}

void SmartBlocksWorld::addBlock(int blockId,
                                SmartBlocksBlockCode *(*smartBlockCodeBuildingFunction)(SmartBlocksBlock*),
                                const Cell3DPosition &pos,const Color &col) {
    SmartBlocksBlock *smartBlock = new SmartBlocksBlock(blockId,smartBlockCodeBuildingFunction);
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
            (ptrBlock)->getInterface(NeighborDirection::Direction(i))->
                connect(ptrNeighbor->getInterface(NeighborDirection::Direction(
                                                      NeighborDirection::getOpposite(i))));

            OUTPUT << "connection #" << (ptrBlock)->blockId <<
                " to #" << ptrNeighbor->blockId << endl;
        } else {
            (ptrBlock)->getInterface(NeighborDirection::Direction(i))->connect(NULL);
        }
    }
}

void SmartBlocksWorld::getPresenceMatrix(const PointCel &pos,PresenceMatrix &pm) {
    presence *gpm=pm.grid;
    SmartBlocksBlock **grb;

    for (int i=0; i<9; i++) { *gpm++ = wallCell; };
    int ix0 = (pos.x<1)?1-pos.x:0,
        ix1 = (pos.x>lattice->gridSize[0]-2)?lattice->gridSize[0]-pos.x+1:3,
        iy0 = (pos.y<1)?1-pos.y:0,
        iy1 = (pos.y>lattice->gridSize[1]-2)?lattice->gridSize[1]-pos.y+1:3,
        ix,iy;
    for (iy=iy0; iy<iy1; iy++) {
        gpm = pm.grid+(iy*3+ix0);
        // PTHY: TODO: idem fix 2D->3D
        grb = (SmartBlocksBlock **)lattice->grid+(ix0+pos.x-1+(iy+pos.y-1)*lattice->gridSize[0]);
        for (ix=ix0; ix<ix1; ix++) {
            *gpm++ = (*grb)?
                ((isBorder(ix+pos.x-1,iy+pos.y-1))?
                 (isSingle(ix+pos.x-1,iy+pos.y-1)?
                  singleCell
                  :borderCell)
                 :fullCell)
                :emptyCell;
            grb++;
        }
    }
}

void SmartBlocksWorld::getPresenceMatrix0(const PointCel &pos,PresenceMatrix &pm) {
    presence *gpm=pm.grid;
    SmartBlocksBlock **grb;

    for (int i=0; i<9; i++) { *gpm++ = wallCell; };

    int ix0 = (pos.x<1)?1-pos.x:0,
        ix1 = (pos.x>lattice->gridSize[0]-2)?lattice->gridSize[0]-pos.x+1:3,
        iy0 = (pos.y<1)?1-pos.y:0,
        iy1 = (pos.y>lattice->gridSize[1]-2)?lattice->gridSize[1]-pos.y+1:3,
        ix,iy;
    for (iy=iy0; iy<iy1; iy++) {
        gpm = pm.grid+(iy*3+ix0);
        grb = (SmartBlocksBlock **)lattice->grid+(ix0+pos.x-1+(iy+pos.y-1)*lattice->gridSize[0]);
        for (ix=ix0; ix<ix1; ix++) {
            *gpm++ = (*grb)?fullCell:emptyCell;
            grb++;
        }
    }
}

bool SmartBlocksWorld::isBorder(int x,int y) {
    SmartBlocksBlock **grb=(SmartBlocksBlock **)lattice->grid+x+y*lattice->gridSize[0];
    //if ((*grb)->_isBorder) return true;
    int ix0 = (x<1)?1-x:0,
        ix1 = (x>lattice->gridSize[0]-2)?lattice->gridSize[0]-x+1:3,
        iy0 = (y<1)?1-y:0,
        iy1 = (y>lattice->gridSize[1]-2)?lattice->gridSize[1]-y+1:3,
        ix,iy;
    for (iy=iy0; iy<iy1; iy++) {
        grb = (SmartBlocksBlock **)lattice->grid+(ix0+x-1+(iy+y-1)*lattice->gridSize[0]);
        for (ix=ix0; ix<ix1; ix++) {
            //if (*grb==NULL || (*grb)->wellPlaced) return true;
            if (*grb==NULL) return true;
            grb++;
        }
    }
    return false;
}

bool SmartBlocksWorld::isSingle(int x,int y) {
    SmartBlocksBlock **grb=(SmartBlocksBlock **)lattice->grid+x+y*lattice->gridSize[0];
    return (*grb)->_isSingle;
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
    //string path = str+"/circuit.tga";
    string path = str+"/bois.tga";
    int lx,ly;
    idTextureFloor = GlutWindow::loadTexture(path.c_str(),lx,ly);

    path=str+"/digits.tga";
    idTextureDigits = GlutWindow::loadTexture(path.c_str(),lx,ly);
}

void SmartBlocksWorld::connectBlock(SmartBlocksBlock *block) {
    Cell3DPosition pos = block->position;

    lattice->insert(block, pos);
//	OUTPUT << "Reconnection " << block->blockId << " pos ="<< ix << "," << iy << endl;
    linkBlock(pos);
    linkNeighbors(pos);
}

void SmartBlocksWorld::disconnectBlock(SmartBlocksBlock *block) {
    P2PNetworkInterface *fromBlock,*toBlock;

    for(int i=0; i<4; i++) {
        fromBlock = block->getInterface(NeighborDirection::Direction(i));
        if (fromBlock && fromBlock->connectedInterface) {
            toBlock = fromBlock->connectedInterface;
            fromBlock->connectedInterface=NULL;
            toBlock->connectedInterface=NULL;
        }
    }

    lattice->remove(block->position);
    //	OUTPUT << getScheduler()->now() << " : Disconnection " <<
    // block->blockId << " pos ="<< ix << "," << iy << endl;
}

int SmartBlocksWorld::nbreWellPlacedBlock() {
    std::map<int, BuildingBlock*>::iterator it;
    int n=0;
    SmartBlocksBlock *sb;
    for( it = buildingBlocksMap.begin() ; it != buildingBlocksMap.end() ; ++it) {
        sb = (SmartBlocksBlock *)(it->second);
        if (sb->wellPlaced) n++;
    }
    return n;
}

void SmartBlocksWorld::deleteBlock(SmartBlocksBlock *bb) {
    if (bb->getState() >= SmartBlocksBlock::ALIVE ) {
        // cut links between bb and others
        for(int i=0; i<4; i++) {
            P2PNetworkInterface *bbi = bb->getInterface(NeighborDirection::Direction(i));
            if (bbi->connectedInterface) {
                //bb->removeNeighbor(bbi); //Useless
                bbi->connectedInterface->hostBlock->removeNeighbor(bbi->connectedInterface);
                bbi->connectedInterface->connectedInterface=NULL;
                bbi->connectedInterface=NULL;
            }
        }

        // free grid cell
        lattice->remove(bb->position);

        disconnectBlock(bb);
    }

    if (selectedBlock == bb->ptrGlBlock) {
        selectedBlock = NULL;
        GlutContext::mainWindow->select(NULL);
    }

    // remove the associated glBlock
    std::vector<GlBlock*>::iterator cit=tabGlBlocks.begin();
    if (*cit==bb->ptrGlBlock) tabGlBlocks.erase(cit);
    else {
        while (cit!=tabGlBlocks.end() && (*cit)!=bb->ptrGlBlock) {
            cit++;
        }
        if (*cit==bb->ptrGlBlock) tabGlBlocks.erase(cit);
    }
    delete bb->ptrGlBlock;
}


void SmartBlocksWorld::menuChoice(int n) {
    SmartBlocksBlock *bb = (SmartBlocksBlock *)getBlockById(tabGlBlocks[numSelectedBlock]->blockId);

    switch (n) {
    case 1 : {
        OUTPUT << "ADD block link to : " << bb->blockId << "     num Face : " << numSelectedFace << endl;

        Cell3DPosition pos = bb->getPosition(NeighborDirection::Direction(numSelectedFace));

        addBlock(-1, bb->buildNewBlockCode, pos, bb->color);
        linkNeighbors(pos);
    } break;
    case 2 : {
        OUTPUT << "DEL num block : " << tabGlBlocks[numSelectedBlock]->blockId << endl;
        deleteBlock(bb);
        linkNeighbors(bb->position);
    } break;
    case 3 : {
        tapBlock(getScheduler()->now(), bb->blockId);
    } break;
    case 4:                 // Save current configuration
        exportConfiguration();
        break;
    }
}

void SmartBlocksWorld::setSelectedFace(int n) {
    numSelectedBlock = n / numPickingTextures;
    string name = objBlockForPicking->getObjMtlName(n % numPickingTextures);

    numSelectedFace = numPickingTextures;   // Undefined NeighborDirection

    if (name == "face_north") numSelectedFace = NeighborDirection::North;
    else if (name == "face_east") numSelectedFace = NeighborDirection::East;
    else if (name == "face_south") numSelectedFace = NeighborDirection::South;
    else if (name == "face_west") numSelectedFace = NeighborDirection::West;
}

void SmartBlocksWorld::addStat(int n,int v) {
    tabStatsData[n]+=v;
    if (nbreStats<=n) nbreStats=n+1;
}

void SmartBlocksWorld::printStats() {
    OUTPUT << "stats: \t" << nbreWellPlacedBlock();
    for (int i=0;i<nbreStats; i++) {
        OUTPUT << "\t"<< tabStatsData[i] ;
    }
    OUTPUT << "\t" << getScheduler()->getNbreMessages() << endl;
}

void SmartBlocksWorld::initTargetGrid() {
    if (targetGrid) delete [] targetGrid;
    int sz = lattice->gridSize[0]*lattice->gridSize[1];
    targetGrid = new presence[sz];
    memset(targetGrid,emptyCell,sz*sizeof(presence));
}

} // SmartBlocks namespace
