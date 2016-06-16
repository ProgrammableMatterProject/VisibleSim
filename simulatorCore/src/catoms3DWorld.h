/*
 * catoms3DWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS3DWORLD_H_
#define CATOMS3DWORLD_H_

#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "cell3DPosition.h"
#include "catoms3DBlock.h"
#include "objLoader.h"
#include "skeleton.h"
#include <boost/asio.hpp>
#include "trace.h"
#include <vector>

//!< \namespace Catoms3D
namespace Catoms3D {

/**
 * \class Catoms3DWorld catoms3DWorld.h
 */
class Catoms3DWorld : BaseSimulator::World {
protected:
    int gridSize[3];
    Catoms3DBlock **gridPtrBlocks;
    GLfloat blockSize[3];
    Camera *camera;
    GLuint idTextureHexa,idTextureGrid;
    Skeleton *skeleton;
/*	presence *targetGrid;
	Catoms3DCapabilities *capabilities;*/
    ObjLoader::ObjLoader *objBlock,*objBlockForPicking,*objRepere;

    Catoms3DWorld(int slx,int sly,int slz, int argc, char *argv[]);
    virtual ~Catoms3DWorld();

    void linkBlock(const Cell3DPosition &pos);
public:
    static void createWorld(int slx,int sly,int slz, int argc, char *argv[]);
    static void deleteWorld();
    static Catoms3DWorld* getWorld() {
	assert(world != NULL);
	return((Catoms3DWorld*)world);
    }

    void printInfo() {
	OUTPUT << "I'm a Catoms3DWorld" << endl;
    }

    virtual Catoms3DBlock* getBlockById(int bId) {
	return((Catoms3DBlock*)World::getBlockById(bId));
    }

    virtual void addBlock(int blockId,
			  Catoms3DBlockCode *(*robotBlockCodeBuildingFunction)(Catoms3DBlock*),
			  const Cell3DPosition&pos, short orientation,
			  const Color &col, bool master=false);
    void deleteBlock(Catoms3DBlock *bb);
    inline void setBlocksSize(float *siz) { blockSize[0] = siz[0]; blockSize[1] = siz[1]; blockSize[2] = siz[2]; };
    inline const float *getBlocksSize() { return blockSize; };

/**
 * \brief associate a block to a cell of the grid
 */
    inline void setGridPtr(const Cell3DPosition &pos,Catoms3DBlock *ptr) { gridPtrBlocks[pos.pt[0]+(pos.pt[1]+pos.pt[2]*gridSize[1])*gridSize[0]]=ptr; };
/**
 * \brief return the block placed on the cell of position pos
 */
    inline Catoms3DBlock* getGridPtr(const Cell3DPosition &pos) { return gridPtrBlocks[pos.pt[0]+(pos.pt[1]+pos.pt[2]*gridSize[1])*gridSize[0]]; };
    inline int* getGridSize() {return gridSize;}
    //inline presence *getTargetGridPtr(int *gs) { memcpy(gs,gridSize,3*sizeof(int)); return targetGrid; };
    //inline presence getTargetGrid(int ix,int iy,int iz) { return targetGrid[(iz*gridSize[1]+iy)*gridSize[0]+ix]; };
    //inline void setTargetGrid(presence value,int ix,int iy,int iz) { targetGrid[(iz*gridSize[1]+iy)*gridSize[0]+ix]=value; };
    inline void setSkeleton(Skeleton *s) { skeleton=s; };
    inline double getSkeletonPotentiel(const Vector3D& pos) { return (skeleton==NULL)?-1:skeleton->potentiel(pos); };
    //void initTargetGrid();

    //inline void setCapabilities(Catoms3DCapabilities *capa) { capabilities=capa; };
    //void getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm);
    //inline Catoms3DCapabilities* getCapabilities() { return capabilities; };
/**
 * \brief update the list of connected blocks
 */
    void linkBlocks();

    Cell3DPosition worldToGridPosition(Vector3D &pos);
    Vector3D gridToWorldPosition(const Cell3DPosition &pos);

    virtual void glDraw();
    virtual void glDrawId();
    virtual void glDrawIdByMaterial();
    void updateGlData(BuildingBlock *bb);
     void updateGlData(Catoms3DBlock*blc,const Color &color);
     void updateGlData(Catoms3DBlock*blc, bool visible);
     void updateGlData(Catoms3DBlock*blc, const Cell3DPosition &position);
     void updateGlData(Catoms3DBlock*blc, const Vector3D &position);
     void updateGlData(Catoms3DBlock*blc, const Matrix &mat);
    virtual void createHelpWindow();
    inline virtual Camera *getCamera() { return camera; };
    virtual void setSelectedFace(int n);
    virtual void menuChoice(int n);
    virtual void disconnectBlock(Catoms3DBlock *block);
    virtual void connectBlock(Catoms3DBlock *block);
    virtual bool canAddBlockToFace(int numSelectedBlock, int numSelectedFace);
/**
 * \brief load the background textures (internal)
 */
    void loadTextures(const string &str);

};

inline void createWorld(int slx,int sly,int slz, int argc, char *argv[]) {
    Catoms3DWorld::createWorld(slx,sly,slz, argc,argv);
}

inline void deleteWorld() {
    Catoms3DWorld::deleteWorld();
}

inline Catoms3DWorld* getWorld() { return(Catoms3DWorld::getWorld()); }

} // Catoms3D namespace

#endif /* CATOMS3DWORLD_H_ */
