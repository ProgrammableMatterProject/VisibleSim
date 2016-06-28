/*
 * robotBlocksWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef ROBOTBLOCKSWORLD_H_
#define ROBOTBLOCKSWORLD_H_

#include <vector>
#include <boost/asio.hpp>

#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "robotBlocksBlock.h"
#include "robotBlocksCapabilities.h"
#include "objLoader.h"
#include "scheduler.h"
#include "trace.h"

namespace RobotBlocks {

static const Vector3D defaultBlockSize{39.0, 39.0, 40.0};

class RobotBlocksWorld : public BaseSimulator::World {
protected:
    GLuint idTextureWall;
    presence *targetGrid; //!< An array representing the target grid of the simulation, i.e. the shape to produce (can be 2D / 3D)
    RobotBlocksCapabilities *capabilities;
    ObjLoader::ObjLoader *objBlock,*objBlockForPicking,*objRepere;

    virtual ~RobotBlocksWorld();
public:
    RobotBlocksWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
             int argc, char *argv[]);

    static void deleteWorld();
    static RobotBlocksWorld* getWorld() {
    assert(world != NULL);
    return((RobotBlocksWorld*)world);
    }

    void printInfo() {
    OUTPUT << "I'm a RobotBlocksWorld" << endl;
    }

    virtual RobotBlocksBlock* getBlockById(int bId) {
    return((RobotBlocksBlock*)World::getBlockById(bId));
    }

    virtual void addBlock(int blockId, BlockCode *(*blockCodeBuildingFunction)(BuildingBlock*),
                          const Cell3DPosition &pos, const Color &col,
                          short orientation = 0, bool master = false);
    virtual void deleteBlock(BuildingBlock *blc);
    inline presence *getTargetGridPtr(short *gs)
    { memcpy(gs,lattice->gridSize.pt,3*sizeof(short)); return targetGrid; };
    inline presence getTargetGrid(int ix,int iy,int iz)
    { return targetGrid[(iz*lattice->gridSize[1]+iy)*lattice->gridSize[0]+ix]; };
    inline void setTargetGrid(presence value,int ix,int iy,int iz)
    { targetGrid[(iz*lattice->gridSize[1]+iy)*lattice->gridSize[0]+ix]=value; };
    void initTargetGrid();

    inline void setCapabilities(RobotBlocksCapabilities *capa) { capabilities=capa; };
    void getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm);
    inline RobotBlocksCapabilities* getCapabilities() { return capabilities; };
    /**
     * \brief Connects block on grid cell pos to its neighbor
     * \param pos : Position of the block to connect
     */
    virtual void linkBlock(const Cell3DPosition &pos);
    void loadTextures(const string &str);
    virtual void glDraw();
    virtual void glDrawId();
    virtual void glDrawIdByMaterial();
    virtual void updateGlData(RobotBlocksBlock*blc,int prev,int next);
    virtual void setSelectedFace(int n);
    virtual void menuChoice(int n);
    virtual void disconnectBlock(RobotBlocksBlock *block);
    virtual void connectBlock(RobotBlocksBlock *block);
    virtual void exportConfiguration();
};

inline void deleteWorld() {
    RobotBlocksWorld::deleteWorld();
}

inline RobotBlocksWorld* getWorld() { return(RobotBlocksWorld::getWorld()); }

} // RobotBlocks namespace

#endif /* ROBOTBLOCKSWORLD_H_ */
