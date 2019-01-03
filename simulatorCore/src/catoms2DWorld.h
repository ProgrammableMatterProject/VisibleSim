/*
 * catoms2DWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS2DWORLD_H_
#define CATOMS2DWORLD_H_

#include <vector>

#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "catoms2DBlock.h"
#include "objLoader.h"
#include "trace.h"

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

    virtual Catoms2DBlock* getBlockById(int bId) {
    return((Catoms2DBlock*)World::getBlockById(bId));
    }

    virtual void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation = 0, bool master = false);
    void loadTextures(const string &str);

    /**
     * \brief Connects block on grid cell pos to its neighbor
     * \param pos : Position of the block to connect
     */
    virtual void linkBlock(const Cell3DPosition &pos);

    bool areNeighborsWorldPos(Vector3D &pos1, Vector3D &pos2);
    bool areNeighborsGridPos(Cell3DPosition &pos1, Cell3DPosition &pos2);

    virtual void updateGlData(BuildingBlock*blc);
    virtual void updateGlData(Catoms2DBlock*blc, const Vector3D &position);
    virtual void updateGlData(Catoms2DBlock*blc, const Vector3D &position, double angle);

    /** @copydoc World::glDraw() */
    virtual void glDraw();
    /** @copydoc World::glDrawId() */
    virtual void glDrawId();
    /** @copydoc World::glDrawIdByMaterial() */
    virtual void glDrawIdByMaterial();
    /** @copydoc World::glDrawSpecificBg() */
    virtual void glDrawSpecificBg();
    /** @copydoc World::createPopupMenu(int ix,int iy) */
    virtual void createPopupMenu(int ix,int iy);
    /** @copydoc World::setSelectedFace(int n) */
    virtual void setSelectedFace(int n);
    /** @copydoc World::menuChoice(int n) */
    virtual void menuChoice(int n);
    /** @copydoc World::exportConfiguration() */
    virtual void exportConfiguration();
};

inline void deleteWorld() {
    Catoms2DWorld::deleteWorld();
}

inline Catoms2DWorld* getWorld() { return(Catoms2DWorld::getWorld()); }

} // Catoms2D namespace

#endif /* CATOMS2DWORLD_H_ */
