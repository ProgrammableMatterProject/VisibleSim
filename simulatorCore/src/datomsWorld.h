/*
 * \file datomsWorld.h
 *
 *  Created on: 28 janvier 2018
 *      Author: Benoît
 */

#ifndef DATOMSWORLD_H_
#define DATOMSWORLD_H_

#include <vector>

#include "buildingBlock.h"
#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "cell3DPosition.h"
#include "datomsBlock.h"
#include "objLoader.h"
#include "trace.h"
#include "datomsMotionRules.h"

//!< \namespace Datoms
namespace Datoms {

static const Vector3D defaultBlockSize{10.0, 10.0, 10.0};

/**
 * \class DatomsWorld datomsWorld.h
 */
class DatomsWorld : public BaseSimulator::World {
protected:
    GLuint idTextureHexa,idTextureGrid;
	DatomsMotionRules *motionRules;

    virtual ~DatomsWorld();
public:
    DatomsWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                  int argc, char *argv[]);

    static void deleteWorld();
    static DatomsWorld* getWorld() {
        assert(world != NULL);
        return((DatomsWorld*)world);
    }

    void printInfo() {
        OUTPUT << "I'm a DatomsWorld" << endl;
    }

    virtual DatomsBlock* getBlockById(int bId) {
        return((DatomsBlock*)World::getBlockById(bId));
    }


    virtual void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation, bool master);
	inline DatomsMotionRules *getMotionRules() { return motionRules; };

    /**
     * \brief Connects block on grid cell pos to its neighbor
     * \param pos : Position of the block to connect
     */
    virtual void linkBlock(const Cell3DPosition &pos);

    virtual void glDraw();
    virtual void glDrawId();
    virtual void glDrawIdByMaterial();
    virtual void glDrawBackground();
    void updateGlData(BuildingBlock *bb);
    void updateGlData(DatomsBlock*blc,const Color &color);
    void updateGlData(DatomsBlock*blc, bool visible);
    void updateGlData(DatomsBlock*blc, const Cell3DPosition &position);
    void updateGlData(DatomsBlock*blc, const Vector3D &position);
    void updateGlData(DatomsBlock*blc, const Matrix &mat);
    virtual void setSelectedFace(int n);
    virtual void exportConfiguration();

/**
 * \brief load the background textures (internal)
 */
    void loadTextures(const string &str);

};

inline void deleteWorld() {
    DatomsWorld::deleteWorld();
}

inline DatomsWorld* getWorld() { return(DatomsWorld::getWorld()); }

} // Datoms namespace

#endif /* DATOMSWORLD_H_ */
