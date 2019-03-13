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

using namespace Datoms;

class Deformation {
public :
/**
    \brief Create a couple of rotations
    \param p : fixed pivot catom
    \param C1 : rotation axe center for the first rotation
    \param V1 : rotation axe for the first rotation
    \param C2 : rotation axe center for the second rotation
    \param V2 : rotation axe for the second rotation
    \param mid : geometrical model id for mobile module
    \param pid : geometrical model id for pivot module
    */
    Deformation(const DatomsBlock *mobile,const DatomsBlock *fixe,const Vector3D &C1,const Vector3D &V1,const Vector3D &C2,const Vector3D &V2,PistonId mid,PistonId  pid, vector<pair<DatomsBlock*,PistonId>> blockingModules);
	Deformation() {};

	void setup(const Vector3D &C1,const Vector3D &V1,const Vector3D &C2,const Vector3D &V2);

	void init() {
		step=0;
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

	PistonId mobileShape,pivotShape;
	const DatomsBlock *ptrPivot,*ptrMobile;
protected :
	short step;
	Matrix initialMatrix,interMatrix,finalMatrix;
	Vector3D Caxis0,Caxis1,Vaxis0,Vaxis1;
    vector<pair<DatomsBlock*,PistonId>> animated;
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
