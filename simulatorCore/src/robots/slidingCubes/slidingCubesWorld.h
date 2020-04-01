/*
 * slidingCubesWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef SLIDINGCUBESWORLD_H_
#define SLIDINGCUBESWORLD_H_

#include <vector>

#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "slidingCubesBlock.h"
#include "objLoader.h"
#include "scheduler.h"
#include "trace.h"
#include "slidingCubesMotionRules.h"

namespace SlidingCubes {

static const Vector3D defaultBlockSize{10.0, 10.0, 10.0};

class SlidingCubesWorld : public BaseSimulator::World {
    inline static const int numPickingTextures = 6; /* The number of picking textures defined
                                                       for this type of catom,
                                                       used to deduce selected Block / face */
protected:
    GLuint idTextureWall = 0;
        SlidingCubesMotionRules *motionRules;
    virtual ~SlidingCubesWorld();
public:
    SlidingCubesWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                     int argc, char *argv[]);

    static void deleteWorld();
    static SlidingCubesWorld* getWorld() {
        assert(world != NULL);
        return((SlidingCubesWorld*)world);
    }

    void printInfo() {
        OUTPUT << "I'm a SlidingCubesWorld" << endl;
    }

    SlidingCubesMotionRules *getMotionRules() { return motionRules; }
    virtual SlidingCubesBlock* getBlockById(int bId) override {
        return((SlidingCubesBlock*)World::getBlockById(bId));
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
    virtual void updateGlData(SlidingCubesBlock*blc,int prev,int next);
    /**
     * @copydoc World::setSelectedFace
     */
    virtual void setSelectedFace(int n) override;
    /**
     * @copydoc World::exportConfiguration
     */
    virtual void exportConfiguration() override;

        virtual void createPopupMenu(int ix, int iy) override;
        virtual void menuChoice(int n) override;

        /**
         * \brief Export a 3D model in STL format to print the whole configuration
         * \param title : title of the STL file
         * \result Returns true if the faces was well written
         */
        virtual bool exportSTLModel(string title) override;

};

inline void deleteWorld() {
    SlidingCubesWorld::deleteWorld();
}

inline SlidingCubesWorld* getWorld() { return(SlidingCubesWorld::getWorld()); }

} // SlidingCube namespace

#endif /* SLIDINGCUBESWORLD_H_ */
