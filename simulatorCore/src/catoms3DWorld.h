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
#include "trace.h"
#include "catoms3DMotionRules.h"

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

//Nurbs mirror
const int S_NUMPOINTS=6;
const int S_ORDER=3;
const int S_NUMKNOTS=(S_NUMPOINTS + S_ORDER);
const int T_NUMPOINTS=8;
const int T_ORDER=3;
const int T_NUMKNOTS=(T_NUMPOINTS + T_ORDER);


static const Vector3D defaultBlockSize{10.0, 10.0, 10.0};

/**
 * \class Catoms3DWorld catoms3DWorld.h
 */
class Catoms3DWorld : public BaseSimulator::World {
protected:
    GLuint idTextureHexa,idTextureGrid;
    Catoms3DMotionRules *motionRules;
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
*/

//Nurbs mirror
    GLfloat sknots[S_NUMKNOTS] = {0.0,0.125,0.250,0.375,0.5,0.625,0.75,0.875,1.0};
    GLfloat tknots[T_NUMKNOTS] = {0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0};

/*GLfloat ctlpoints[S_NUMPOINTS][T_NUMPOINTS][4] = {
  {{6.707,4.201,0,1},{6.303,5.362,0,1},{6.303,6.975,0,1},{6.303,8.589,0,1},{6.303,10.203,0,1},{6.303,11.817,0,1},{6.303,13.43,0,1},{6.303,15.044,0,1}},
  {{7.807,3.748,0,1},{5.747,3.585,3.032,1},{5.674,6.975,3.032,1},{5.678,8.589,3.032,1},{5.729,10.203,3.032,1},{5.833,11.817,3.032,1},{5.945,13.43,3.032,1},{5.945,15.044,3.032,1}},
  {{9.31,3.748,0,1},{7.54,3.548,3.193,1},{6.785,5.75,7.171,1},{6.966,8.589,8.83,1},{6.823,10.948,10.273,1},{6.259,14.622,10.101,1},{5.547,16.364,7.069,1},{7.54,15.044,2.936,1}},
  {{10.813,3.748,0,1},{12.46,3.457,3.193,1},{13.215,5.75,7.171,1},{13.034,8.589,8.83,1},{12.583,10.948,10.273,1},{13.585,14.764,10.101,1},{14.114,15.262,6.772,1},{12.583,15.04,2.936,1}},
  {{12.316,3.748,0,1},{14.253,3.585,3.032,1},{13.701,6.975,3.306,1},{13.994,8.589,3.502,1},{14.098,10.203,3.972,1},{13.993,11.817,4.295,1},{14.137,13.43,4.426,1},{13.624,15.044,3.032,1}},
  {{13.323,4.203,0,1},{13.819,5.362,0,1},{13.819,6.975,0,1},{13.819,8.589,0,1},{13.819,10.203,0,1},{13.819,11.817,0,1},{13.819,13.43,0,1},{13.819,15.044,0,1}},
  };
*/


/*GLfloat ctlpoints[S_NUMPOINTS][T_NUMPOINTS][4] = {
  {{7.807,3.748,0,1},{5.747,3.585,3.032,1},{5.674,6.975,3.032,1},{5.678,8.589,3.032,1},{5.729,10.203,3.032,1},{5.833,11.817,3.032,1},{5.945,13.43,3.032,1},{5.945,15.044,3.032,1}},
  {{6.707,4.201,0,1},{6.303,5.362,0,1},{6.303,6.975,0,1},{6.303,8.589,0,1},{6.303,10.203,0,1},{6.303,11.817,0,1},{6.303,13.43,0,1},{6.303,15.044,0,1}},
  {{9.31,3.748,0,1},{7.54,3.548,3.193,1},{6.785,5.75,7.171,1},{6.966,8.589,8.83,1},{6.823,10.948,10.273,1},{6.259,14.622,10.101,1},{5.547,16.364,7.069,1},{7.54,15.044,2.936,1}},
  {{10.813,3.748,0,1},{12.46,3.457,3.193,1},{13.215,5.75,7.171,1},{13.034,8.589,8.83,1},{12.583,10.948,10.273,1},{13.585,14.764,10.101,1},{14.114,15.262,6.772,1},{12.583,15.04,2.936,1}},
  {{13.323,4.203,0,1},{13.819,5.362,0,1},{13.819,6.975,0,1},{13.819,8.589,0,1},{13.819,10.203,0,1},{13.819,11.817,0,1},{13.819,13.43,0,1},{13.819,15.044,0,1}},
  {{12.316,3.748,0,1},{14.253,3.585,3.032,1},{13.701,6.975,3.306,1},{13.994,8.589,3.502,1},{14.098,10.203,3.972,1},{13.993,11.817,4.295,1},{14.137,13.43,4.426,1},{13.624,15.044,3.032,1}},
  };
*/


    GLfloat ctlpoints[S_NUMPOINTS][T_NUMPOINTS][4] = {
        {{15.802,6.132,2.914,1},{14.144,6.132,2.193,1},{12.487,6.132,1.483,1},{10.829,6.132,0.881,1},{9.171,6.132,0.408,1},{7.513,6.132,0.192,1},{5.856,6.132,0.068,1},{4.198,6.132,0,1}},
        {{15.802,7.6792,5.402,1},{14.144,7.6792,8.383,1},{12.487,7.6792,8.459,1},{10.829,7.6792,8.102,1},{9.171,7.6792,7.353,1},{7.513,7.6792,6.414,1},{5.856,7.6792,5.031,1},{4.198,7.6792,1.356,1}},
        {{15.802,9.2264,7.538,1},{14.144,9.2264,9.23,1},{12.487,9.2264,9.292,1},{10.829,9.2264,9.087,1},{9.171,9.2264,8.303,1},{7.513,9.2264,7.398,1},{5.856,9.2264,5.652,1},{4.198,9.2264,2.135,1}},
        {{15.802,10.7736,7.541,1},{14.144,10.7736,9.167,1},{12.487,10.7736,9.288,1},{10.829,10.7736,8.929,1},{9.171,10.7736,8.471,1},{7.513,10.7736,7.377,1},{5.856,10.7736,5.691,1},{4.198,10.7736,2.134,1}},
        {{15.802,12.3208,5.772,1},{14.144,12.3208,8.067,1},{12.487,12.3208,8.042,1},{10.829,12.3208,7.71,1},{9.171,12.3208,7.12,1},{7.513,12.3208,6.306,1},{5.856,12.3208,4.862,1},{4.198,12.3208,1.354,1}},
        {{15.802,13.868,2.918,1},{14.144,13.868,2.191,1},{12.487,13.868,1.498,1},{10.829,13.868,0.86,1},{9.171,13.868,0.42,1},{7.513,13.868,0.157,1},{5.856,13.868,0.034,1},{4.198,13.868,0,1}},
    };


/*GLfloat ctlpoints[S_NUMPOINTS][T_NUMPOINTS][4] = {
  {{15.802,6.132,2.914,1},{15.802,7.6792,5.402,1},{15.802,9.2264,7.538,1},{15.802,10.7736,7.541,1},{15.802,12.3208,5.772,1},{15.802,13.868,2.918,1},{14.144,6.132,2.193,1},{14.144,7.6792,8.383,1}},
  {{14.144,9.2264,9.23,1},{14.144,10.7736,9.167,1},{14.144,12.3208,8.067,1},{14.144,13.868,2.191,1},{12.487,6.132,1.483,1},{12.487,7.6792,8.459,1},{12.487,9.2264,9.292,1},{12.487,10.7736,9.288,1}},
  {{12.487,12.3208,8.042,1},{12.487,13.868,1.498,1},{10.829,6.132,0.881,1},{10.829,7.6792,8.102,1},{10.829,9.2264,9.087,1},{10.829,10.7736,8.929,1},{10.829,12.3208,7.71,1},{10.829,13.868,0.86,1}},
  {{9.171,6.132,0.408,1},{9.171,7.6792,7.353,1},{9.171,9.2264,8.303,1},{9.171,10.7736,8.471,1},{9.171,12.3208,7.12,1},{9.171,13.868,0.42,1},{7.513,6.132,0.192,1},{7.513,7.6792,6.414,1}},
  {{7.513,9.2264,7.398,1},{7.513,10.7736,7.377,1},{7.513,12.3208,6.306,1},{7.513,13.868,0.157,1},{5.856,6.132,0.068,1},{5.856,7.6792,5.031,1},{5.856,9.2264,5.652,1},{5.856,10.7736,5.691,1}},
  {{5.856,12.3208,4.862,1},{5.856,13.868,0.034,1},{4.198,6.132,0,1},{4.198,7.6792,1.356,1},{4.198,9.2264,2.135,1},{4.198,10.7736,2.134,1},{4.198,12.3208,1.354,1},{4.198,13.868,0,1}},
  };
*/
    GLUnurbsObj *theNurb;

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

    virtual Catoms3DBlock* getBlockById(int bId) override {
        return((Catoms3DBlock*)World::getBlockById(bId));
    }

    virtual void addBlock(bID blockId, BlockCodeBuilder bcb, const Cell3DPosition &pos, const Color &col,
                          short orientation, bool master) override;

    inline Catoms3DMotionRules *getMotionRules() { return motionRules; };

    /**
     * \brief Connects block on grid cell pos to its neighbor
     * \param pos : Position of the block to connect
     */
    virtual void linkBlock(const Cell3DPosition &pos) override;

    virtual void glDraw() override;
    virtual void glDrawId() override;
    virtual void glDrawIdByMaterial() override;
    virtual void glDrawSpecificBg() override;
    virtual void updateGlData(BuildingBlock *bb) override;

    using World::updateGlData; // Suppresses hiding warning
    void updateGlData(Catoms3DBlock*blc, const Vector3D &position);
    void updateGlData(Catoms3DBlock*blc, const Matrix &mat);
    void updateGlData(Catoms3DBlock*blc,const Color &color);
    void updateGlData(Catoms3DBlock*blc, bool visible);
    void updateGlData(Catoms3DBlock*blc, const Cell3DPosition &position);

    virtual void setSelectedFace(int n) override;
    virtual void exportConfiguration() override;

    virtual void createPopupMenu(int ix, int iy) override;
    virtual void menuChoice(int n) override;
/**
 * \brief Export a 3D model in STL format to print the whole configuration
 * \param title : title of the STL file
 * \result Returns true if the faces was well written
 */
    virtual bool exportSTLModel(string title) override;

/**
 * \brief load the background textures (internal)
 */
    void loadTextures(const string &str) override;
};

inline void deleteWorld() {
    Catoms3DWorld::deleteWorld();
}

inline Catoms3DWorld* getWorld() { return(Catoms3DWorld::getWorld()); }


} // Catoms3D namespace

#endif /* CATOMS3DWORLD_H_ */
