/*
 * \file catoms3DWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS3DWORLD_H_
#define CATOMS3DWORLD_H_

#include <boost/asio.hpp>
#include <vector>

#include "buildingBlock.h"
#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "cell3DPosition.h"
#include "catoms3DBlock.h"
#include "objLoader.h"
#include "skeleton.h"
#include "trace.h"

//!< \namespace Catoms3D
namespace Catoms3D {

static const Vector3D defaultBlockSize{10.0, 10.0, 10.0};

/**
 * \class Catoms3DWorld catoms3DWorld.h
 */
class Catoms3DWorld : public BaseSimulator::World {
protected:
    GLuint idTextureHexa,idTextureGrid;
    Skeleton *skeleton;
/*	presence *targetGrid;
	Catoms3DCapabilities *capabilities;*/
    ObjLoader::ObjLoader *objBlock,*objBlockForPicking,*objRepere;

    virtual ~Catoms3DWorld();
public:
    Catoms3DWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                  int argc, char *argv[]);

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


    virtual void addBlock(int blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation, bool master);
    virtual void deleteBlock(BuildingBlock *blc);

    //inline presence *getTargetGridPtr(int *gs) { memcpy(gs,gridSize,3*sizeof(int)); return targetGrid; };
    //inline presence getTargetGrid(int ix,int iy,int iz) { return targetGrid[(iz*lattice->gridSize[1]+iy)*lattice->gridSize[0]+ix]; };
    //inline void setTargetGrid(presence value,int ix,int iy,int iz) { targetGrid[(iz*lattice->gridSize[1]+iy)*lattice->gridSize[0]+ix]=value; };
    inline void setSkeleton(Skeleton *s) { skeleton=s; };
    inline double getSkeletonPotentiel(const Vector3D& pos) { return (skeleton==NULL)?-1:skeleton->potentiel(pos); };
    //void initTargetGrid();

    //inline void setCapabilities(Catoms3DCapabilities *capa) { capabilities=capa; };
    //void getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm);
    //inline Catoms3DCapabilities* getCapabilities() { return capabilities; };

    /**
     * \brief Connects block on grid cell pos to its neighbor
     * \param pos : Position of the block to connect
     */
    virtual void linkBlock(const Cell3DPosition &pos);

    virtual void glDraw();
    virtual void glDrawId();
    virtual void glDrawIdByMaterial();
    void updateGlData(BuildingBlock *bb);
    void updateGlData(Catoms3DBlock*blc,const Color &color);
    void updateGlData(Catoms3DBlock*blc, bool visible);
    void updateGlData(Catoms3DBlock*blc, const Cell3DPosition &position);
    void updateGlData(Catoms3DBlock*blc, const Vector3D &position);
    void updateGlData(Catoms3DBlock*blc, const Matrix &mat);
    virtual void setSelectedFace(int n);
    virtual void disconnectBlock(Catoms3DBlock *block);
    virtual void connectBlock(Catoms3DBlock *block);

/**
 * \brief load the background textures (internal)
 */
    void loadTextures(const string &str);

};

inline void deleteWorld() {
    Catoms3DWorld::deleteWorld();
}

inline Catoms3DWorld* getWorld() { return(Catoms3DWorld::getWorld()); }

} // Catoms3D namespace

#endif /* CATOMS3DWORLD_H_ */
