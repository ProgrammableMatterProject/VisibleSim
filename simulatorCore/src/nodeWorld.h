/**
 * @file   nodeWorld.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:59:04 2019
 *
 * @brief
 *
 *
 */

#ifndef NODEWORLD_H_
#define NODEWORLD_H_

#include <vector>

#include "buildingBlock.h"
#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "cell3DPosition.h"
#include "nodeBlock.h"
#include "objLoader.h"
#include "trace.h"

//!< \namespace Node
namespace Node {

static const Vector3D defaultBlockSize{10.0, 10.0, 10.0};

/**
 * \class NodeWorld nodeWorld.h
 */
class NodeWorld : public BaseSimulator::World {
protected:
    ObjLoader::ObjLoader *objConnector = NULL;           //!< Object loader for a block

    virtual ~NodeWorld();
public:
    NodeWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                  int argc, char *argv[]);

    static void deleteWorld();
    static NodeWorld* getWorld() {
        assert(world != NULL);
        return((NodeWorld*)world);
    }

    void printInfo() {
        OUTPUT << "I'm a NodeWorld" << endl;
    }

    virtual NodeBlock* getBlockById(int bId) override {
        return((NodeBlock*)World::getBlockById(bId));
    }

    virtual void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation, bool master) override;

    GLuint idTextureWall, idTextureDigits;

    /**
     * \brief Connects block on grid cell pos to its neighbor
     * \param pos : Position of the block to connect
     */
    virtual void linkBlock(const Cell3DPosition &pos) override;

    virtual void glDraw() override;
    virtual void glDrawId() override;
    virtual void glDrawIdByMaterial() override;
    void updateGlData(BuildingBlock *bb) override;
    void updateGlData(NodeBlock*blc,const Color &color);
    void updateGlData(NodeBlock*blc, bool visible);
    void updateGlData(NodeBlock*blc, const Cell3DPosition &position);
    void updateGlData(NodeBlock*blc, const Vector3D &position);
    void updateGlData(NodeBlock*blc, const Matrix &mat);
    void updateGlData(NodeBlock*blc, short id, float length);
    virtual void setSelectedFace(int n) override;
    virtual void exportConfiguration() override;

    virtual void disconnectBlock(BuildingBlock *block);

/**
 * \brief load the background textures (internal)
 */
    void loadTextures(const string &str) override;

};

inline void deleteWorld() {
    NodeWorld::deleteWorld();
}

inline NodeWorld* getWorld() { return(NodeWorld::getWorld()); }


} // Node namespace

#endif /* NODEWORLD_H_ */
