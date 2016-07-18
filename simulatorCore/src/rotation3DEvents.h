/*!
 * \file rotation3DEvents.h
 * \brief Motion events for 3D rotating blocks
 * \date 15/02/2015
 * \author Benoît Piranda, Pierre Thalamy
 *
 * For now, only Catoms3D implement this kind of movements, if other blocks are added, we may consider creating subclasses of BuildingBlocks depending on the type of motion they implement.
 *
 */

#ifndef ROTATION3DEVENTS_H_
#define ROTATION3DEVENTS_H_

#include "matrix44.h"
#include "catoms3DBlock.h"
#include "events.h"

const int nbRotationSteps = 100;

using namespace Catoms3D;

class Rotations3D {
public :
/**
    \brief Create a couple of rotations
    \param p : fixed pivot catom
    \param ax1 : rotation axe for the first rotation
    \param ang1 : rotation angle for the first rotation
    \param ax2 : rotation axe for the second rotation
    \param ang2 : rotation angle for the second rotation
*/
    Rotations3D(Catoms3DBlock *mobile,Catoms3DBlock *fixe,const Vector3D &ax1,double ang1,const Vector3D &ax2,double ang2);
    Rotations3D() {};

    void init(const Matrix& m) {
        firstRotation=true;
        step=0;
        initialMatrix=m;
        firstStepMatrix.identity();
    }

/**
    \brief Return current transformation matrix in m
    \param m : result matrix
*/
//    void getCurrentMatrix(Matrix &m) { m=mat; };
/**
    \brief Calculate transformation matrix for one more step of rotations
    \param m : result matrix
    \return true at the end of the animation.
*/
    bool nextStep(Matrix &m);
    void getFinalPositionAndOrientation(Cell3DPosition &position, short &orientation);
protected :
    bool firstRotation;
    short step;
    Matrix initialMatrix,firstStepMatrix;//,matTBA,matTAB,matTBC,matTCB;
    Vector3D AB,AD,CB;
    Vector3D axe1;
    Vector3D axe2;
    double angle1;
    double angle2;
    static constexpr double angleArticulation=6.46237535743;
    static constexpr double coefRayonCourbure=1.00639465274;
};

//===========================================================================================================
//
//          Rotation3DStartEvent  (class)
//
//===========================================================================================================

class Rotation3DStartEvent : public BlockEvent {
    Rotations3D rot;
public:
	Rotation3DStartEvent(uint64_t, Catoms3DBlock *block,const Rotations3D& r);
	Rotation3DStartEvent(Rotation3DStartEvent *ev);
	~Rotation3DStartEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          Rotation3DStepEvent  (class)
//
//===========================================================================================================

class Rotation3DStepEvent : public BlockEvent {
    Rotations3D rot;
public:
	Rotation3DStepEvent(uint64_t, Catoms3DBlock *block,const Rotations3D& r);
	Rotation3DStepEvent(Rotation3DStepEvent *ev);
	~Rotation3DStepEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          Rotation3DStopEvent  (class)
//
//===========================================================================================================

class Rotation3DStopEvent : public BlockEvent {
    Rotations3D rot;
public:
	Rotation3DStopEvent(uint64_t, Catoms3DBlock *block,const Rotations3D& r);
	Rotation3DStopEvent(Rotation3DStepEvent *ev);
	~Rotation3DStopEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          Rotation3DEndEvent  (class)
//
//===========================================================================================================

class Rotation3DEndEvent : public BlockEvent {
public:
	Rotation3DEndEvent(uint64_t, Catoms3DBlock *block);
	Rotation3DEndEvent(Rotation3DEndEvent *ev);
	~Rotation3DEndEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

#endif /* ROTATIONS3DEVENTS_H_ */
