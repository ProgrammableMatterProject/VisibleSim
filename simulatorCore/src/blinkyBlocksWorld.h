/*
 * blinkyBlocksWorld.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef BLINKYBLOCKSWORLD_H_
#define BLINKYBLOCKSWORLD_H_

#include <vector>

#include "world.h"
#include "openglViewer.h"
#include "vector3D.h"
#include "blinkyBlocksBlock.h"
#include "trace.h"
#include "blinkyBlocksScenario.h"

namespace BlinkyBlocks {

static const Vector3D defaultBlockSize{40.0, 40.0, 41.0};

class BlinkyBlocksWorld : public BaseSimulator::World {
protected:   
    GLuint idTextureWall;
    vector<ScenarioEvent*> tabEvents;

    virtual ~BlinkyBlocksWorld();
public:
    BlinkyBlocksWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                      int argc, char *argv[]);

    static void deleteWorld();
    static BlinkyBlocksWorld* getWorld() {
        assert(world != NULL);
        return((BlinkyBlocksWorld*)world);
    }
    void printInfo() {
        OUTPUT << "I'm a BlinkyBlocksWorld" << endl;
    }

    virtual BlinkyBlocksBlock* getBlockById(int bId) {
        return((BlinkyBlocksBlock*)World::getBlockById(bId));
    }

    virtual void addBlock(int blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation = 0, bool master = false);

    virtual void linkBlock(const Cell3DPosition &pos);
    virtual void loadTextures(const string &str);

    virtual void glDraw();
    virtual void glDrawId();
    virtual void glDrawIdByMaterial();
    virtual void setSelectedFace(int n);
    virtual void exportConfiguration();

    /* Sends the appropriate message (tap, ...) to the VM associated to bId block (through the scheduler)*/
    void accelBlock(uint64_t date, int bId, int x, int y, int z);
    void shakeBlock(uint64_t date, int bId, int f);
    virtual void stopBlock(uint64_t date, int bId);

    void addScenarioEvent(ScenarioEvent *ev) { tabEvents.push_back(ev); };

    // Prints information about the blocks
    void dump();

};

std::ostream& operator<<(std::ostream &stream, BlinkyBlocksBlock const& bb);

inline void deleteWorld() {
    BlinkyBlocksWorld::deleteWorld();
}

inline BlinkyBlocksWorld* getWorld() { return(BlinkyBlocksWorld::getWorld()); }

} // BlinkyBlocks namespace

#endif /* BLINKYBLOCKSWORLD_H_ */
