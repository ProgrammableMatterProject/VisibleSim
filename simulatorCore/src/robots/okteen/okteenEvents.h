/*!
 * \file okteenEvents.h
 * \brief Motion events for okteen modules
 * \date 17/07/2017
 * \author Benoît Piranda
 *
 * Special rotation/translation for Okteen modules
 *
 */

#ifndef OKTEENMOTIONSEVENTS_H_
#define OKTEENMOTIONSEVENTS_H_

#include "math/matrix44.h"
#include "robots/okteen/okteenBlock.h"
#include "events/events.h"
#include "grid/lattice.h"

const int nbRotationSteps = 20;

using namespace Okteen;

class OkteenMotions {
public :
/**
    \brief Create a sequence of motions
    \param mobile : mobile module
    \param connector : direction of fixed module
    \param axis : direction of rotation axis (CCW)
    */
    OkteenMotions(OkteenBlock *mobile,SCLattice::Direction connector,SCLattice::Direction axisDir);
    OkteenMotions() {};

    void init() {
        //currentAction=1;
        step=0;
        //initialMatrix=((OkteenGlBlock*)(module->ptrGlBlock))->mat;*/
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
    void getFinalPosition(Cell3DPosition &position);
public :
    OkteenBlock *module;
protected :
    /*short currentAction;
    Matrix initialMatrix;
    Vector3D axis;
    Vector3D translation;*/
    short step;
    Vector3D initPos;
    Vector3D initConnectorDir;
    Vector3D finalPos;
    Vector3D finalConnectorDir;
};

//===========================================================================================================
//
//          OkteenMotionsStartEvent  (class)
//
//===========================================================================================================

class OkteenMotionsStartEvent : public BlockEvent {
    OkteenMotions motion;
public:
    OkteenMotionsStartEvent(Time, const OkteenMotions& om);
    OkteenMotionsStartEvent(OkteenMotionsStartEvent *ev);
    ~OkteenMotionsStartEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          OkteenMotionsStepEvent  (class)
//
//===========================================================================================================

class OkteenMotionsStepEvent : public BlockEvent {
    OkteenMotions motion;
public:
    OkteenMotionsStepEvent(Time, const OkteenMotions& om);
    OkteenMotionsStepEvent(OkteenMotionsStepEvent *ev);
    ~OkteenMotionsStepEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          OkteenMotionsStopEvent  (class)
//
//===========================================================================================================

class OkteenMotionsStopEvent : public BlockEvent {
    OkteenMotions motion;
public:
    OkteenMotionsStopEvent(Time, const OkteenMotions& om);
    OkteenMotionsStopEvent(OkteenMotionsStepEvent *ev);
    ~OkteenMotionsStopEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          OkteenMotionsEndEvent  (class)
//
//===========================================================================================================

class OkteenMotionsEndEvent : public BlockEvent {
public:
    OkteenMotionsEndEvent(Time, BuildingBlock* module);
    OkteenMotionsEndEvent(OkteenMotionsEndEvent *ev);
    ~OkteenMotionsEndEvent();
    void consumeBlockEvent() override {}
    void consume() override;
    const virtual string getEventName() override;
};

#endif /* OKTEENMOTIONSEVENTS_H_ */
