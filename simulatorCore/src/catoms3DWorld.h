/*
 * \file catoms3DWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS3DWORLD_H_
#define CATOMS3DWORLD_H_

#include <vector>

#include "buildingBlock.h"
#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "cell3DPosition.h"
#include "catoms3DBlock.h"
#include "objLoader.h"
#include "skeleton.h"
#include "trace.h"

//!< \namespace Catoms3D
namespace Catoms3D {

//Nurbs surface
/*const int S_NUMPOINTS=5;
const int S_ORDER=3;
const int S_NUMKNOTS=(S_NUMPOINTS + S_ORDER);
const int T_NUMPOINTS=5;
const int T_ORDER=3;
const int T_NUMKNOTS=(T_NUMPOINTS + T_ORDER);
*/

//Nurbs car
/*const int S_NUMPOINTS=7;
const int S_ORDER=3;
const int S_NUMKNOTS=(S_NUMPOINTS + S_ORDER);
const int T_NUMPOINTS=4;
const int T_ORDER=3;
const int T_NUMKNOTS=(T_NUMPOINTS + T_ORDER);
*/
static const Vector3D defaultBlockSize{10.0, 10.0, 10.0};

/**
 * \class Catoms3DWorld catoms3DWorld.h
 */
class Catoms3DWorld : public BaseSimulator::World {
protected:
    GLuint idTextureHexa,idTextureGrid;
    Skeleton *skeleton = NULL;
//Nurbs surface
/*	GLfloat sknots[S_NUMKNOTS] =
	    {0.0,0.125,0.25,0.375,0.5,0.625f,0.750f,1.0f};
	GLfloat tknots[T_NUMKNOTS] = {0.0,0.125,0.25,0.375,0.5,0.625f,0.750f,1.0f};

	GLfloat ctlpoints[S_NUMPOINTS][T_NUMPOINTS][4] = {
		{{0.0,0.0,0.0,45.},{0.0,45.0,0.0,45.},{0.0,90.0,0.0,45.},{0.0,135.0,0.0,45.},{0.0,180.0,0.0,45.}},
		{{45.0,0.0,0.0,45.},{45.0,45.0,0.0,45.},{45.0,90.0,0.0,45.},{45.0,135.0,0.0,45.},{45.0,180.0,0.0,45.}},
		{{90.0,0.0,0.0,45.},{90.0,45.0,270.0,45.},{90.0,90.0,0.0,45.},{90.0,135.0,0.0,45.},{90.0,180.0,0.0,45.}},
		{{135.0,0.0,0.0,45.},{135.0,45.0,0.0,45.},{135.0,90.0,0.0,45.},{135.0,135.0,90.0,45.},{135.0,180.0,0.0,45.}},
		{{180.0,0.0,0.0,45.},{180.0,45.0,0.0,45.},{180.0,90.0,0.0,45.},{180.0,135.0,0.0,45.},{180.0,180.0,0.0,45.}},
	};
*/

//Nurbs car
/*GLfloat sknots[S_NUMKNOTS] =
    {0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0f};
GLfloat tknots[T_NUMKNOTS] = {0.0,0.15,0.3,0.45,0.60,0.75,1.0f};

GLfloat ctlpoints[S_NUMPOINTS][T_NUMPOINTS][4] = {
        {{0.0,0.0,-33.33,33.33},{0.0,33.33,-33.33,33.33},{0.0,66.66,-33.33,33.33},{0.0,100.0,-33.33,33.33}},
        {{6.66,0.0,40.0,33.33},{6.66,33.33,53.33,33.33},{6.66,66.66,53.33,33.33},{6.66,100.0,40.0,33.33}},
        {{66.66,0.0,66.66,33.33},{66.66,33.33,80.0,33.33},{66.66,66.66,80.0,33.33},{66.66,100.0,66.66,33.33}},
        {{116.66,0.0,66.66,33.33},{116.66,33.33,80.0,33.33},{116.66,66.66,80.0,33.33},{116.66,100.0,66.66,33.33}},
        {{150.0,0.0,26.66,33.33},{150.0,33.33,40.0,33.33},{150.0,66.66,40.0,33.33},{150.0,100.0,26.66,33.33}},
        {{193.33,0.0,26.66,33.33},{193.33,33.33,40.0,33.33},{193.33,66.66,40.0,33.33},{193.33,100.0,26.66,33.33}},
        {{200.0,0.0,-20.0,33.33},{200.0,33.33,-20.0,33.33},{200.0,66.66,-20.0,33.33},{200.0,100.0,-20.0,33.33}},

};

	GLUnurbsObj *theNurb;
*/
    virtual ~Catoms3DWorld();
public:
    Catoms3DWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                  int argc, char *argv[]);

    static void deleteWorld();
    static Catoms3DWorld* getWorld() {
        assert(world != NULL);
        return((Catoms3DWorld*)world);
    }

    void printInfo() {
        OUTPUT << "I'm a Catoms3DWorld" << endl;
    }

    virtual Catoms3DBlock* getBlockById(int bId) {
        return((Catoms3DBlock*)World::getBlockById(bId));
    }


    virtual void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation, bool master);
    inline void setSkeleton(Skeleton *s) { skeleton=s; };
    inline double getSkeletonPotentiel(const Vector3D& pos) { return (skeleton==NULL)?-1:skeleton->potentiel(pos); };

    /**
     * \brief Connects block on grid cell pos to its neighbor
     * \param pos : Position of the block to connect
     */
    virtual void linkBlock(const Cell3DPosition &pos);

    virtual void glDraw();
    virtual void glDrawId();
    virtual void glDrawIdByMaterial();
    virtual void glDrawSpecificBg();
    void updateGlData(BuildingBlock *bb);
    void updateGlData(Catoms3DBlock*blc,const Color &color);
    void updateGlData(Catoms3DBlock*blc, bool visible);
    void updateGlData(Catoms3DBlock*blc, const Cell3DPosition &position);
    void updateGlData(Catoms3DBlock*blc, const Vector3D &position);
    void updateGlData(Catoms3DBlock*blc, const Matrix &mat);
    virtual void setSelectedFace(int n);
    virtual void exportConfiguration();

/**
 * \brief load the background textures (internal)
 */
    void loadTextures(const string &str);

};

inline void deleteWorld() {
    Catoms3DWorld::deleteWorld();
}

inline Catoms3DWorld* getWorld() { return(Catoms3DWorld::getWorld()); }

} // Catoms3D namespace

#endif /* CATOMS3DWORLD_H_ */
