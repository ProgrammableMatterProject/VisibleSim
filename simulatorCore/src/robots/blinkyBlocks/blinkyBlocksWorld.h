/*
 * blinkyBlocksWorld.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom+ben
 */

#ifndef BLINKYBLOCKSWORLD_H_
#define BLINKYBLOCKSWORLD_H_

#include <vector>

#include "../../base/world.h"
#include "../../gui/openglViewer.h"
#include "../../math/vector3D.h"
#include "blinkyBlocksBlock.h"
#include "../../utils/trace.h"

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

        /**
         * Return an ID of the type of current Blocks
         * @return byte value of Block type from 'replayTags.h' list
         */
        ReplayTags::u1 getBlockType() override { return ReplayTags::MODULE_TYPE_BB; };

        BlinkyBlocksBlock* getBlockById(int bId) override {
            return ((BlinkyBlocksBlock*)World::getBlockById(bId));
        }

        void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                      uint8_t orient=0) override;

        void linkBlock(const Cell3DPosition &pos) override;
        void loadTextures(const string &str) override;

        void glDraw() override;
        void glDrawShadows() override;
        void glDrawId() override;
        void glDrawIdByMaterial() override;
        void glDrawBackground() override;
        void setSelectedFace(int n) override;
        void exportConfiguration() override;

       // void updateGlData(BlinkyBlocksBlock*blc, short rotCode);

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
