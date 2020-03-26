/**
 * @file   nodeWorld.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:59:04 2019
 *
 * @brief
 *
 *
 */

#ifndef HEXANODEWORLD_H_
#define HEXANODEWORLD_H_

#include <vector>

#include "buildingBlock.h"
#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "cell3DPosition.h"
#include "hexanodeBlock.h"
#include "objLoader.h"
#include "trace.h"

//!< \namespace Hexanode
namespace Hexanode {

static const Vector3D defaultBlockSize{10.0, 10.0, 10.0};

class HexanodeMotionEngine;
class HexanodeMotion;

/**
 * \class HexanodeWorld nodeWorld.h
 */
class HexanodeWorld : public BaseSimulator::World {
protected:
    ObjLoader::ObjLoader *objConnector = NULL;           //!< Object loader for a block
		HexanodeMotionEngine *nodeMotionEngine;
    virtual ~HexanodeWorld();
public:
    HexanodeWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                  int argc, char *argv[]);

    static void deleteWorld();
    static HexanodeWorld* getWorld() {
        assert(world != NULL);
        return((HexanodeWorld*)world);
    }

    void printInfo() {
        OUTPUT << "I'm a HexanodeWorld" << endl;
    }

    virtual HexanodeBlock* getBlockById(int bId) override {
        return((HexanodeBlock*)World::getBlockById(bId));
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
		virtual void glDrawShadows() override;
		virtual void glDrawId() override;
    virtual void glDrawIdByMaterial() override;
    void updateGlData(BuildingBlock *bb) override;
    void updateGlData(HexanodeBlock*blc,const Color &color);
    void updateGlData(HexanodeBlock*blc, bool visible);
    void updateGlData(HexanodeBlock*blc, const Cell3DPosition &position);
    void updateGlData(HexanodeBlock*blc, const Vector3D &position);
    void updateGlData(HexanodeBlock*blc, const Matrix &mat);
    virtual void setSelectedFace(int n) override;
    virtual void exportConfiguration() override;

    virtual void disconnectBlock(BuildingBlock *block);
		virtual void glDrawSpecificBg() override;
		
		virtual void createPopupMenu(int ix, int iy) override;
		virtual void menuChoice(int n) override;
		
		vector<HexanodeMotion*>getAllMotionsForModule(HexanodeBlock*nb);
/**
 * \brief load the background textures (internal)
 */
    void loadTextures(const string &str) override;
};

inline void deleteWorld() {
    HexanodeWorld::deleteWorld();
}

inline HexanodeWorld* getWorld() { return(HexanodeWorld::getWorld()); }


} // Hexanode namespace

#endif /* HEXANODEWORLD_H_ */
