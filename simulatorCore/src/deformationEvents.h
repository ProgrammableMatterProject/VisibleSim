/*!
 * \file deformationEvents.h
 * \brief Motion events for 3D deformation of Datoms modules
 * \date 15/02/2015
 * \author Benoît Piranda, Pierre Thalamy
 *
 */

#ifndef DEFORMATIONEVENTS_H_
#define DEFORMATIONEVENTS_H_

#include "matrix44.h"
#include "datomsBlock.h"
#include "events.h"

const int nbSteps_4 = 2;

using namespace Datoms;

class Deformation {
public :
/**
    \brief Create a couple of rotations
    \param p : fixed pivot catom
    \param ax1 : rotation axe for the first rotation
    \param ang1 : rotation angle for the first rotation
    \param ax2 : rotation axe for the second rotation
    \param ang2 : rotation angle for the second rotation
*/
    Deformation (DatomsBlock *mobile,DatomsBlock *fixe,const Vector3D &ax1,const Vector3D &ax2,uint8_t id);
    Deformation () {};

    void init(const Matrix& m) {
        step=0;
        initialMatrix=m;
        finalMatrix=m*finalMatrix;
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

	uint8_t modelId;
protected :
    short step;
    Matrix initialMatrix,finalMatrix;
    Vector3D A0P0,A1P1;
    Vector3D axe1;
    Vector3D axe2;
};

//===========================================================================================================
//
//          DeformationStartEvent  (class)
//
//===========================================================================================================

class DeformationStartEvent : public BlockEvent {
    Deformation deform;
public:
	DeformationStartEvent(Time, DatomsBlock *block,const Deformation& r);
	DeformationStartEvent(DeformationStartEvent *ev);
	~DeformationStartEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          DeformationStepEvent  (class)
//
//===========================================================================================================

class DeformationStepEvent : public BlockEvent {
    Deformation deform;
public:
	DeformationStepEvent(Time, DatomsBlock *block,const Deformation& r);
	DeformationStepEvent(DeformationStepEvent *ev); ~DeformationStepEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          DeformationStopEvent  (class)
//
//===========================================================================================================

class DeformationStopEvent : public BlockEvent {
    Deformation deform;
public:
	DeformationStopEvent(Time, DatomsBlock *block,const Deformation& r);
	DeformationStopEvent(DeformationStepEvent *ev);
	~DeformationStopEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

//===========================================================================================================
//
//          DeformationEndEvent  (class)
//
//===========================================================================================================

class DeformationEndEvent : public BlockEvent {
public:
	DeformationEndEvent(Time, DatomsBlock *block);
	DeformationEndEvent(DeformationEndEvent *ev);
	~DeformationEndEvent();
	void consumeBlockEvent() {};
	void consume();
	const virtual string getEventName();
};

#endif /* DEFORMATIONEVENTS_H_ */
