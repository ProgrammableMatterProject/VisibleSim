/*
 * smartBlocksWorld.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSWORLD_H_
#define SMARTBLOCKSWORLD_H_

#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "smartBlocksBlock.h"
#include "scheduler.h"

namespace SmartBlocks {

static const Vector3D defaultBlockSize{25.0, 25.0, 11.0};

class SmartBlocksWorld : public BaseSimulator::World {
protected:
    virtual ~SmartBlocksWorld();
public:
    SmartBlocksWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                     int argc, char *argv[]);

    GLuint idTextureFloor,idTextureDigits;

    static void deleteWorld();
    static SmartBlocksWorld* getWorld() {
        assert(world != NULL);
        return((SmartBlocksWorld*)world);
    }

    void printInfo() {
        cout << "I'm a SmartBlocksWorld" << endl;
    }

    virtual void addBlock(bID blockId, BlockCode *(*blockCodeBuildingFunction)(BuildingBlock*),
                          const Cell3DPosition &pos, const Color &col,
                          short orientation = 0, bool master = false);

    void linkBlock(const Cell3DPosition &pos);
    void loadTextures(const string &str);
    virtual void glDraw();
    virtual void glDrawId();
    virtual void glDrawIdByMaterial();
    virtual void glDrawSpecificBg();
    virtual void setSelectedFace(int n);
    virtual void exportConfiguration();
};

inline void deleteWorld() {
    SmartBlocksWorld::deleteWorld();
}

inline SmartBlocksWorld* getWorld() { return(SmartBlocksWorld::getWorld()); }

} // SmartBlocks namespace

#endif /* SMARTBLOCKSWORLD_H_ */
