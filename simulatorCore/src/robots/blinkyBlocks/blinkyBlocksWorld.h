/*
 * blinkyBlocksWorld.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef BLINKYBLOCKSWORLD_H_
#define BLINKYBLOCKSWORLD_H_

#include <vector>

#include "base/world.h"
#include "gui/openglViewer.h"
#include "math/vector3D.h"
#include "robots/blinkyBlocks/blinkyBlocksBlock.h"
#include "utils/trace.h"

namespace BlinkyBlocks {

static const Vector3D defaultBlockSize{40.0, 40.0, 41.0};

class BlinkyBlocksWorld : public BaseSimulator::World {
    inline static const int numPickingTextures = 6; /* The number of picking textures defined
                                                       for this type of catom,
                                                       used to deduce selected Block / face */
protected:
    GLuint idTextureWall;

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

    virtual BlinkyBlocksBlock* getBlockById(int bId) override {
        return((BlinkyBlocksBlock*)World::getBlockById(bId));
    }

    virtual void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation = 0, bool master = false) override;

    virtual void linkBlock(const Cell3DPosition &pos) override;
    virtual void loadTextures(const string &str) override;

    virtual void glDraw() override;
    virtual void glDrawShadows(bool enableBG) override;
    virtual void glDrawId() override;
    virtual void glDrawIdByMaterial() override;
    virtual void glDrawSpecificBg() override;
    virtual void setSelectedFace(int n) override;
    virtual void exportConfiguration() override;

    /* Sends the appropriate message (tap, ...) to the VM associated to id block (through the scheduler)*/
    void accelBlock(Time date, bID id, int x, int y, int z);
    void shakeBlock(Time date, bID id, int f);
    virtual void stopBlock(Time date, bID id);

    // void addScenarioEvent(ScenarioEvent *ev) { tabEvents.push_back(ev); };

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
