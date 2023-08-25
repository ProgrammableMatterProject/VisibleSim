/*
 * \file catoms3DWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS3DWORLD_H_
#define CATOMS3DWORLD_H_

#include <vector>

#include "../../base/buildingBlock.h"
#include "../../gui/openglViewer.h"
#include "../../base/world.h"
#include "../../math/vector3D.h"
#include "math/cell3DPosition.h"
#include "../../robots/catoms3D/catoms3DBlock.h"
#include "../../gui/objLoader.h"
#include "../../utils/trace.h"
#include "catoms3DMotionRules.h"

//!< \namespace Catoms3D
namespace Catoms3D {


    static const Vector3D defaultBlockSize{10.0, 10.0, 10.0};

/**
 * \class Catoms3DWorld catoms3DWorld.h
 */
    class Catoms3DWorld : public BaseSimulator::World {
    protected:
        GLuint idTextureHexa, idTextureGrid;
        Catoms3DMotionRules *motionRules;
        inline static const int numPickingTextures = 13; /* The number of picking textures defined
                                                        for this type of catom,
                                                        used to deduce selected Block / face */

        virtual ~Catoms3DWorld();

    public:
        Catoms3DWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                      int argc, char *argv[]);

        static void deleteWorld();

        static Catoms3DWorld *getWorld() {
            assert(world != NULL);
            return ((Catoms3DWorld *) world);
        }

        void printInfo() {
            OUTPUT << "I'm a Catoms3DWorld" << endl;
        }

        /**
         * Return an ID of the type of current Blocks
         * @return byte value of Block type from 'replayTags.h' list
         */
        ReplayTags::u1 getBlockType() override { return ReplayTags::MODULE_TYPE_C3D; };

        virtual Catoms3DBlock *getBlockById(int bId) override {
            return ((Catoms3DBlock *) World::getBlockById(bId));
        }

        virtual void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                              uint8_t orient) override;

        inline Catoms3DMotionRules *getMotionRules() { return motionRules; };

        /**
         * \brief Connects block on grid cell pos to its neighbor
         * \param pos : Position of the block to connect
         */
        virtual void linkBlock(const Cell3DPosition &pos) override;

        /**
         * Get the number of selected face and -1 if none
         */
        virtual int getNumSelectedFace() {
            return (numSelectedFace == 13 ? -1 : numSelectedFace);
        }

        void glDraw() override;

        void glDrawId() override;

        void glDrawIdByMaterial() override;

        void glDrawBackground() override;

        void updateGlData(BuildingBlock *bb) override;

        using World::updateGlData; // Suppresses hiding warning
        void updateGlData(Catoms3DBlock *blc, const Vector3D &position);

        void updateGlData(Catoms3DBlock *blc, const Matrix &mat);

        void updateGlData(Catoms3DBlock *blc, const Color &color);

        void updateGlData(Catoms3DBlock *blc, bool visible);

        void updateGlData(Catoms3DBlock *blc, const Cell3DPosition &position);

        virtual void setSelectedFace(int n) override;

        virtual void exportConfiguration() override;

        virtual void createPopupMenu(int ix, int iy) override;

        virtual void menuChoice(int n) override;

        virtual void createHelpWindow() override;
/**
 * \brief Export a 3D model in STL format to print the whole configuration
 * \param title : title of the STL file
 * \result Returns true if the faces was well written
 */
        //virtual bool exportSTLModel(string title) override ;

/**
 * \brief load the background textures (internal)
 */
        void loadTextures(const string &str) override;
    };

    inline void deleteWorld() {
        Catoms3DWorld::deleteWorld();
    }

    inline Catoms3DWorld *getWorld() { return (Catoms3DWorld::getWorld()); }
} // Catoms3D namespace

#endif /* CATOMS3DWORLD_H_ */
