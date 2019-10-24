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

static const Vector3D defaultBlockSize{10.0, 10.0, 10.0};

class RobotBlocksWorld : public BaseSimulator::World {
    inline static const int numPickingTextures = 6; /* The number of picking textures defined
                                                       for this type of catom,
                                                       used to deduce selected Block / face */
protected:
    GLuint idTextureWall = 0;

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

    virtual RobotBlocksBlock* getBlockById(int bId) override {
        return((RobotBlocksBlock*)World::getBlockById(bId));
    }

    /**
     * @copydoc World::addBlock
     */
    virtual void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation = 0, bool master = false) override;
    /**
     * \copydoc World::linkBlock
     */
    virtual void linkBlock(const Cell3DPosition &pos) override;
    /**
     * \copydoc World::loadTextures
     */
    virtual void loadTextures(const string &str) override;
    /**
     * @copydoc World::glDraw
     */
    virtual void glDraw() override;
    /**
     * @copydoc World::glDrawId
     */
    virtual void glDrawId() override;
    /**
     * @copydoc World::glDrawIdByMaterial
     */
    virtual void glDrawIdByMaterial() override;
    /**
     * @copydoc World::glDrawSpecificBg
     */
    virtual void glDrawSpecificBg() override;

    using World::updateGlData; // Suppresses hiding warning

    /**
     * @copydoc World::updateGlData
     */
    virtual void updateGlData(RobotBlocksBlock*blc,int prev,int next);
    /**
     * @copydoc World::setSelectedFace
     */
    virtual void setSelectedFace(int n) override;
    /**
     * @copydoc World::exportConfiguration
     */
    virtual void exportConfiguration() override;
};

inline void deleteWorld() {
    RobotBlocksWorld::deleteWorld();
}

inline RobotBlocksWorld* getWorld() { return(RobotBlocksWorld::getWorld()); }

} // RobotBlocks namespace

#endif /* ROBOTBLOCKSWORLD_H_ */
