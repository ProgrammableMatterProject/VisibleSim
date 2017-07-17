/*!
 * \file okteenWorld.h
 * \brief okteen World
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#ifndef OKTEENWORLD_H_
#define OKTEENWORLD_H_

#include <vector>

#include "buildingBlock.h"
#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "cell3DPosition.h"
#include "okteenBlock.h"
#include "objLoader.h"
#include "trace.h"

//!< \namespace Okteen
namespace Okteen {

static const Vector3D defaultBlockSize{10.0, 10.0, 10.0};

/**
 * \class OkteenWorld okteenWorld.h
 */
class OkteenWorld : public BaseSimulator::World {
protected:
    GLuint idTextureWall;
    ObjLoader::ObjLoader *objConnector = NULL;           //!< Object loader for a block

    virtual ~OkteenWorld();
public:
    OkteenWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                  int argc, char *argv[]);

    static void deleteWorld();
    static OkteenWorld* getWorld() {
        assert(world != NULL);
        return((OkteenWorld*)world);
    }

    void printInfo() {
        OUTPUT << "I'm a OkteenWorld" << endl;
    }

    virtual OkteenBlock* getBlockById(int bId) {
        return((OkteenBlock*)World::getBlockById(bId));
    }

    virtual void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation, bool master);

    /**
     * \brief Connects block on grid cell pos to its neighbor
     * \param pos : Position of the block to connect
     */
    virtual void linkBlock(const Cell3DPosition &pos);

    virtual void glDraw();
    virtual void glDrawId();
    virtual void glDrawIdByMaterial();
    void updateGlData(BuildingBlock *bb);
    void updateGlData(OkteenBlock*blc,const Color &color);
    void updateGlData(OkteenBlock*blc, bool visible);
    void updateGlData(OkteenBlock*blc, const Cell3DPosition &position);
    void updateGlData(OkteenBlock*blc, const Vector3D &position);
    void updateGlData(OkteenBlock*blc, const Matrix &mat);
    void updateGlData(OkteenBlock*blc, short id, float length);
    virtual void setSelectedFace(int n);
    virtual void exportConfiguration();

/**
 * \brief load the background textures (internal)
 */
    void loadTextures(const string &str);

};

inline void deleteWorld() {
    OkteenWorld::deleteWorld();
}

inline OkteenWorld* getWorld() { return(OkteenWorld::getWorld()); }


} // Okteen namespace

#endif /* OKTEENWORLD_H_ */
