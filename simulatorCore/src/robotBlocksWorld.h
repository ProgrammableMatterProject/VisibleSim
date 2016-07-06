/*
 * robotBlocksWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef ROBOTBLOCKSWORLD_H_
#define ROBOTBLOCKSWORLD_H_

#include <vector>

#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "robotBlocksBlock.h"
#include "objLoader.h"
#include "scheduler.h"
#include "trace.h"

namespace RobotBlocks {

static const Vector3D defaultBlockSize{40.0, 40.0, 40.0};

class RobotBlocksWorld : public BaseSimulator::World {
protected:
    GLuint idTextureWall;
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
    
    /**
     * @copydoc World::addBlock
     */
    virtual void addBlock(int blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation = 0, bool master = false);
    virtual void deleteBlock(BuildingBlock *blc);
    /**
     * \copydoc World::linkBlock
     */
    virtual void linkBlock(const Cell3DPosition &pos);
    /**
     * \copydoc World::loadTextures
     */
    virtual void loadTextures(const string &str);
    /**
     * @copydoc World::glDraw
     */
    virtual void glDraw();
    /**
     * @copydoc World::glDrawId
     */
    virtual void glDrawId();
    /**
     * @copydoc World::glDrawIdByMaterial
     */
    virtual void glDrawIdByMaterial();
    /**
     * @copydoc World::updateGlData
     */
    virtual void updateGlData(RobotBlocksBlock*blc,int prev,int next);
    /**
     * @copydoc World::setSelectedFace
     */
    virtual void setSelectedFace(int n);
    /**
     * @copydoc World::menuChoice
     */
    virtual void menuChoice(int n);
    /**
     * @copydoc World::disconnectBlock
     */
    virtual void disconnectBlock(RobotBlocksBlock *block);
    /**
     * @copydoc World::connectBlock
     */
    virtual void connectBlock(RobotBlocksBlock *block);
    /**
     * @copydoc World::exportConfiguration
     */
    virtual void exportConfiguration();
};

inline void deleteWorld() {
    RobotBlocksWorld::deleteWorld();
}

inline RobotBlocksWorld* getWorld() { return(RobotBlocksWorld::getWorld()); }

} // RobotBlocks namespace

#endif /* ROBOTBLOCKSWORLD_H_ */
