/*
 * catoms2DWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS2DWORLD_H_
#define CATOMS2DWORLD_H_

#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "catoms2DBlock.h"
#include "catoms2DCapabilities.h"
#include "objLoader.h"
#include <boost/asio.hpp>
#include "trace.h"
#include <vector>

namespace Catoms2D {

static const Vector3D defaultBlockSize{1.0, 5.0, 1.0};

class Catoms2DWorld : public BaseSimulator::World {
protected:
    ObjLoader::ObjLoader *objBlock,*objBlockForPicking,*objRepere;
    GLuint idTextureHexa,idTextureLines;
    presence *targetGrid;
    Catoms2DCapabilities *capabilities;
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

    inline void setCapabilities(Catoms2DCapabilities *capa) { capabilities=capa; };
    void getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm);
    inline Catoms2DCapabilities* getCapabilities() { return capabilities; };
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
    /** @copydoc World::createPopupMenu(int ix,int iy) */
    virtual void createPopupMenu(int ix,int iy);
    /** @copydoc World::setSelectedFace(int n) */
    virtual void setSelectedFace(int n);
    /** @copydoc World::menuChoice(int n) */
    virtual void menuChoice(int n);
    /** @copydoc World::exportConfiguration() */
    virtual void exportConfiguration();

    void disconnectBlock(Catoms2DBlock *block);
    void connectBlock(Catoms2DBlock *block);
};

inline void deleteWorld() {
    Catoms2DWorld::deleteWorld();
}

inline Catoms2DWorld* getWorld() { return(Catoms2DWorld::getWorld()); }

} // Catoms2D namespace

#endif /* CATOMS2DWORLD_H_ */
