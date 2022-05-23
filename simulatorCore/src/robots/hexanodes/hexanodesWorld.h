/**
 * @file   nodeWorld.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:59:04 2019
 *
 * @brief
 *
 *
 */

#ifndef HEXANODESWORLD_H_
#define HEXANODESWORLD_H_

#include <vector>

#include "../../base/buildingBlock.h"
#include "../../gui/openglViewer.h"
#include "../../base/world.h"
#include "../../math/vector3D.h"
#include "math/cell3DPosition.h"
#include "hexanodesBlock.h"
#include "../../gui/objLoader.h"
#include "../../utils/trace.h"

//!< \namespace Hexanodes
namespace Hexanodes {

static const Vector3D defaultBlockSize{50, 50, 12.5};

class HexanodesMotionEngine;
class HexanodesMotion;

/**
 * \class HexanodesWorld nodeWorld.h
 */
class HexanodesWorld : public BaseSimulator::World {
protected:
    ObjLoader::ObjLoader *objConnector = NULL;           //!< Object loader for a block
        HexanodesMotionEngine *nodeMotionEngine;
    virtual ~HexanodesWorld();
public:
    HexanodesWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                  int argc, char *argv[]);

    static void deleteWorld();
    static HexanodesWorld* getWorld() {
        assert(world != NULL);
        return((HexanodesWorld*)world);
    }

    /**
     * Return an ID of the type of current Blocks
     * @return byte value of Block type from 'replayTags.h' list
     */
    ReplayTags::u1 getBlockType() override { return ReplayTags::MODULE_TYPE_HEXANODE; };

    void printInfo() {
        OUTPUT << "I'm a HexanodesWorld" << endl;
    }

    virtual HexanodesBlock* getBlockById(int bId) override {
        return((HexanodesBlock*)World::getBlockById(bId));
    }

    virtual void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          uint8_t orient) override;

    GLuint idTextureWall, idTextureDigits;

    /**
     * \brief Connects block on grid cell pos to its neighbor
     * \param pos : Position of the block to connect
     */
    void linkBlock(const Cell3DPosition &pos) override;

    void glDraw() override;
    void glDrawShadows() override;
    void glDrawBackground() override;
    void glDrawId() override;
    void glDrawIdByMaterial() override;

    void updateGlData(BuildingBlock *bb) override;
    void updateGlData(HexanodesBlock*blc,const Color &color);
    void updateGlData(HexanodesBlock*blc, bool visible);
    void updateGlData(HexanodesBlock*blc, const Cell3DPosition &position);
    void updateGlData(HexanodesBlock*blc, const Vector3D &position);
    void updateGlData(HexanodesBlock*blc, const Matrix &mat);
    void setSelectedFace(int n) override;
    void exportConfiguration() override;

    virtual void disconnectBlock(BuildingBlock *block);
    void createPopupMenu(int ix, int iy) override;
    void menuChoice(int n) override;

    vector<HexanodesMotion*>getAllMotionsForModule(HexanodesBlock*nb);
/**
 * \brief load the background textures (internal)
 */
    void loadTextures(const string &str) override;
};

inline void deleteWorld() {
    HexanodesWorld::deleteWorld();
}

inline HexanodesWorld* getWorld() { return(HexanodesWorld::getWorld()); }


} // Hexanodes namespace

#endif /* HEXANODESWORLD_H_ */
