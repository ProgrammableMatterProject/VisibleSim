/*
 * \file datomsWorld.h
 *
 *  Created on: 28 janvier 2018
 *      Author: Benoît
 */

#ifndef DATOMSWORLD_H_
#define DATOMSWORLD_H_

#include <vector>

#include "base/buildingBlock.h"
#include "gui/openglViewer.h"
#include "base/world.h"
#include "math/vector3D.h"
#include "grid/cell3DPosition.h"
#include "robots/datoms/datomsBlock.h"
#include "gui/objLoader.h"
#include "utils/trace.h"
#include "robots/datoms/datomsMotionRules.h"

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
    inline static const int numPickingTextures = 13; /* The number of picking textures defined
                                                        for this type of catom,
                                                        used to deduce selected Block / face */
    virtual ~DatomsWorld();
public:
    DatomsWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale, int argc, char *argv[]);

    static void deleteWorld();
    static DatomsWorld* getWorld() {
        assert(world != NULL);
        return((DatomsWorld*)world);
    }

    void printInfo() {
        OUTPUT << "I'm a DatomsWorld" << endl;
    }

    virtual DatomsBlock* getBlockById(int bId) override {
        return((DatomsBlock*)World::getBlockById(bId));
    }


    virtual void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation, bool master) override;
    inline DatomsMotionRules *getMotionRules() { return motionRules; };

    /**
     * \brief Connects block on grid cell pos to its neighbor
     * \param pos : Position of the block to connect
     */
    virtual void linkBlock(const Cell3DPosition &pos) override;

    virtual void glDraw() override;
    virtual void glDrawId() override;
    virtual void glDrawIdByMaterial() override;
    virtual void glDrawBackground();

    using World::updateGlData; // Suppresses hiding warning
    void updateGlData(BuildingBlock *bb) override;
    void updateGlData(const DatomsBlock*blc,const Color &color);
    void updateGlData(DatomsBlock*blc, bool visible);
    void updateGlData(DatomsBlock*blc, const Cell3DPosition &position);
    void updateGlData(DatomsBlock*blc, const Vector3D &position);
    void updateGlData(DatomsBlock*blc, const Matrix &mat);
      void updateGlData(const DatomsBlock*blc, PistonId id);
    virtual void setSelectedFace(int n) override;
    virtual void exportConfiguration() override;

    virtual void createPopupMenu(int ix, int iy) override;
    virtual void menuChoice(int n) override;
/**
 * \brief load the background textures (internal)
 */
    void loadTextures(const string &str) override;

};

inline void deleteWorld() {
    DatomsWorld::deleteWorld();
}

inline DatomsWorld* getWorld() { return(DatomsWorld::getWorld()); }

} // Datoms namespace

#endif /* DATOMSWORLD_H_ */
