/*!
 * \file oktenWorld.h
 * \brief okten World
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#ifndef OKTENWORLD_H_
#define OKTENWORLD_H_

#include <vector>

#include "buildingBlock.h"
#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "cell3DPosition.h"
#include "oktenBlock.h"
#include "objLoader.h"
#include "trace.h"

//!< \namespace Okten
namespace Okten {

static const Vector3D defaultBlockSize{10.0, 10.0, 10.0};

/**
 * \class OktenWorld oktenWorld.h
 */
class OktenWorld : public BaseSimulator::World {
protected:
    GLuint idTextureWall;
    ObjLoader::ObjLoader *objConnector = NULL;           //!< Object loader for a block

    virtual ~OktenWorld();
public:
    OktenWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                  int argc, char *argv[]);

    static void deleteWorld();
    static OktenWorld* getWorld() {
        assert(world != NULL);
        return((OktenWorld*)world);
    }

    void printInfo() {
        OUTPUT << "I'm a OktenWorld" << endl;
    }

    virtual OktenBlock* getBlockById(int bId) {
        return((OktenBlock*)World::getBlockById(bId));
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
    void updateGlData(OktenBlock*blc,const Color &color);
    void updateGlData(OktenBlock*blc, bool visible);
    void updateGlData(OktenBlock*blc, const Cell3DPosition &position);
    void updateGlData(OktenBlock*blc, const Vector3D &position);
    void updateGlData(OktenBlock*blc, const Matrix &mat);
    void updateGlData(OktenBlock*blc, short id, float length);
    virtual void setSelectedFace(int n);
    virtual void exportConfiguration();

/**
 * \brief load the background textures (internal)
 */
    void loadTextures(const string &str);

};

inline void deleteWorld() {
    OktenWorld::deleteWorld();
}

inline OktenWorld* getWorld() { return(OktenWorld::getWorld()); }


} // Okten namespace

#endif /* OKTENWORLD_H_ */
