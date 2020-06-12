/*
 * catoms2DWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS2DWORLD_H_
#define CATOMS2DWORLD_H_

#include <vector>

#include "../../gui/openglViewer.h"
#include "../../base/world.h"
#include "../../math/vector3D.h"
#include "catoms2DBlock.h"
#include "../../gui/objLoader.h"
#include "../../utils/trace.h"

namespace Catoms2D {

static const Vector3D defaultBlockSize{1.0, 5.0, 1.0};

class Catoms2DWorld : public BaseSimulator::World {
protected:
    GLuint idTextureHexa,idTextureLines;
    static const int numPickingTextures = 7; /* The number of picking textures defined for
                                                this type of catom,
                                                used to deduce selected Block / face */

    virtual ~Catoms2DWorld();
public:
    Catoms2DWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
          int argc, char *argv[]);

    static void deleteWorld();
    static Catoms2DWorld* getWorld() {
    assert(world != NULL);
    return((Catoms2DWorld*)world);
    }

    void printInfo() {
    OUTPUT << "I'm a Catoms2DWorld" << endl;
    }

    /**
     * Return an ID of the type of current Blocks
     * @return byte value of Block type from 'replayTags.h' list
     */
    ReplayTags::u1 getBlockType() override { return ReplayTags::MODULE_TYPE_C2D; };

    virtual Catoms2DBlock* getBlockById(int bId) override {
    return((Catoms2DBlock*)World::getBlockById(bId));
    }

    void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation = 0, bool master = false) override;
    void loadTextures(const string &str) override;

    /**
     * \brief Connects block on grid cell pos to its neighbor
     * \param pos : Position of the block to connect
     */
    void linkBlock(const Cell3DPosition &pos) override;

    bool areNeighborsWorldPos(Vector3D &pos1, Vector3D &pos2);
    bool areNeighborsGridPos(Cell3DPosition &pos1, Cell3DPosition &pos2);

    using World::updateGlData; // Suppresses hiding warning
    void updateGlData(BuildingBlock*blc) override;
    virtual void updateGlData(Catoms2DBlock*blc, const Vector3D &position);
    virtual void updateGlData(Catoms2DBlock*blc, const Vector3D &position, double angle);

    /** @copydoc World::glDraw() */
    void glDraw() override;
    /** @copydoc World::glDrawId() */
    void glDrawId() override;
    /** @copydoc World::glDrawIdByMaterial() */
    void glDrawIdByMaterial() override;
    /** @copydoc World::glDrawSpecificBg() */
    void glDrawBackground() override;
    /** @copydoc World::createPopupMenu(int ix,int iy) */
    void createPopupMenu(int ix,int iy) override;
    /** @copydoc World::setSelectedFace(int n) */
    void setSelectedFace(int n) override;
    /** @copydoc World::menuChoice(int n) */
    void menuChoice(int n) override;
    /** @copydoc World::exportConfiguration() */
    void exportConfiguration() override;
};

inline void deleteWorld() {
    Catoms2DWorld::deleteWorld();
}

inline Catoms2DWorld* getWorld() { return(Catoms2DWorld::getWorld()); }

} // Catoms2D namespace

#endif /* CATOMS2DWORLD_H_ */
