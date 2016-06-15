/*
 * catoms3DEvents.cpp
 *
 *  Created on: 2014 fevrary 1st
 *      Author: Benoît
 */

#include "catoms3DEvents.h"
#include "catoms3DScheduler.h"
#include "catoms3DWorld.h"

const int ANIMATION_DELAY=100000;
const int COM_DELAY=2000;

namespace Catoms3D {

//===========================================================================================================
//
//          MotionStartEvent  (class)
//
//===========================================================================================================

MotionStartEvent::MotionStartEvent(uint64_t t, Catoms3DBlock *block,const Rotations &r): BlockEvent(t,block) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_MOTION_START;
	rot = r;
}

MotionStartEvent::MotionStartEvent(MotionStartEvent *ev) : BlockEvent(ev) {
	EVENT_CONSTRUCTOR_INFO();
}

MotionStartEvent::~MotionStartEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void MotionStartEvent::consume() {
	EVENT_CONSUME_INFO();
	Catoms3DScheduler *scheduler = Catoms3D::getScheduler();
	Catoms3DBlock *catom = (Catoms3DBlock *)concernedBlock;
    Catoms3DWorld::getWorld()->disconnectBlock(catom);
    catom->setColor(DARKGREY);
	rot.init(catom->ptrGlBlock->mat);
	scheduler->schedule(new MotionStepEvent(scheduler->now() + ANIMATION_DELAY,catom, rot));
}

const string MotionStartEvent::getEventName() {
	return("MotionStart Event");
}

//===========================================================================================================
//
//          MotionStepEvent  (class)
//
//===========================================================================================================

MotionStepEvent::MotionStepEvent(uint64_t t, Catoms3DBlock *block,const Rotations &r): BlockEvent(t,block) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_MOTION_STEP;

    rot=r;
}

MotionStepEvent::MotionStepEvent(MotionStepEvent *ev) : BlockEvent(ev) {
	EVENT_CONSTRUCTOR_INFO();
}

MotionStepEvent::~MotionStepEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void MotionStepEvent::consume() {
	EVENT_CONSUME_INFO();
	Catoms3DBlock *catom = (Catoms3DBlock*)concernedBlock;
	Catoms3DScheduler *scheduler = Catoms3D::getScheduler();

    Matrix mat;
    bool rotationEnd=rot.nextStep(mat);

    getWorld()->updateGlData(catom,mat);
    if (rotationEnd) {
        scheduler->schedule(new MotionStopEvent(scheduler->now() + ANIMATION_DELAY, catom, rot));
    } else {
        scheduler->schedule(new MotionStepEvent(scheduler->now() + ANIMATION_DELAY, catom, rot));
    }
}

const string MotionStepEvent::getEventName() {
	return("MotionStep Event");
}

//===========================================================================================================
//
//          MotionStepEvent  (class)
//
//===========================================================================================================

MotionStopEvent::MotionStopEvent(uint64_t t, Catoms3DBlock *block,const Rotations& r): BlockEvent(t,block) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_MOTION_STOP;
	rot = r;
}

MotionStopEvent::MotionStopEvent(MotionStepEvent *ev) : BlockEvent(ev) {
	EVENT_CONSTRUCTOR_INFO();
}

MotionStopEvent::~MotionStopEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void MotionStopEvent::consume() {
	EVENT_CONSUME_INFO();
	Catoms3DBlock *catom = (Catoms3DBlock*)concernedBlock;
    catom->setColor(YELLOW);

    Cell3DPosition position;
    short orientation;
/* Transformer les coordonnées GL en coordonnées grille*/
    rot.getFinalPositionAndOrientation(position,orientation);

    Catoms3DWorld *wrld=Catoms3DWorld::getWorld();

    catom->setPositionAndOrientation(position,orientation);

	wrld->setGridPtr(position,catom);
	stringstream info;
    info.str("");
    info << "connect Block " << catom->blockId;
    getScheduler()->trace(info.str(),catom->blockId,LIGHTBLUE);
	wrld->connectBlock(catom);
    Catoms3DScheduler *scheduler = Catoms3D::getScheduler();
    scheduler->schedule(new MotionEndEvent(scheduler->now() + ANIMATION_DELAY, catom));
}

const string MotionStopEvent::getEventName() {
	return("MotionStop Event");
}

//===========================================================================================================
//
//          MotionEndEvent  (class)
//
//===========================================================================================================

MotionEndEvent::MotionEndEvent(uint64_t t, Catoms3DBlock *block): BlockEvent(t,block) {
	EVENT_CONSTRUCTOR_INFO();
	eventType = EVENT_MOTION_END;
}

MotionEndEvent::MotionEndEvent(MotionEndEvent *ev) : BlockEvent(ev) {
	EVENT_CONSTRUCTOR_INFO();
}

MotionEndEvent::~MotionEndEvent() {
	EVENT_DESTRUCTOR_INFO();
}

void MotionEndEvent::consume() {
	EVENT_CONSUME_INFO();
	Catoms3DBlock *rb = (Catoms3DBlock*)concernedBlock;
	concernedBlock->blockCode->processLocalEvent(EventPtr(new MotionEndEvent(date+COM_DELAY,rb)));
}

const string MotionEndEvent::getEventName() {
	return("MotionEnd Event");
}

//===========================================================================================================
//
//          Rotations  (class)
//
//===========================================================================================================

Rotations::Rotations(Catoms3DBlock *mobile,Catoms3DBlock *fixe,const Vector3D &ax1,double ang1,const Vector3D &ax2,double ang2):angle1(ang1),angle2(ang2) {
    static const double c_2 = 0.5/(3+sqrt(2));
    Matrix MA = mobile->getGlBlock()->mat;
    Matrix MB = fixe->getGlBlock()->mat;
    Matrix MA_1;

    // we calculate AB translation in A referentiel
    MA.inverse(MA_1);
    Matrix m = MA_1*MB;
    AB = m*Vector3D(0,0,0,1);

    Matrix matTAB,matTBA;
    matTAB.setTranslation(AB);
    matTBA.setTranslation(-AB);

    // we write rotation R1 axes in A referentiel
    axe1 = m*ax1;
    axe1 = axe1.normer();

    Vector3D v=(AB^axe1).normer();
    AD = 0.5*AB + c_2*v;


    axe2 = ax2.normer();

    Matrix mr;
    mr.setRotation(angle1,axe1);
    firstStepMatrix = matTBA*mr;
    firstStepMatrix = mr*firstStepMatrix;
    firstStepMatrix  = matTAB*firstStepMatrix;
    firstStepMatrix  = MA*firstStepMatrix;

    // we calculate AC=firstStep*AB translation in A referentiel
    firstStepMatrix.inverse(MA_1);
    m = MA_1*MB;
    CB = m*Vector3D(0,0,0,1);
    //matTCB.setTranslation(AB);
    //matTBC.setTranslation(-AB);

    // we write rotation R2 axes in new firstStep A referentiel
    axe2 = m*ax2;
    axe2 = axe2.normer();
}

bool Rotations::nextStep(Matrix &m) {
    if (firstRotation) {
        step++;
        double angle=angle1*step/nbRotationSteps;
        OUTPUT << "step=" << step << "   angle=" << angle << endl;
        Matrix mr;
        mr.setRotation(angle,axe1);
        Matrix matTAB,matTBA;
        /*if (angle<angleArticulation) {
            matTAB.setTranslation(AD);
            matTBA.setTranslation(-AD);
            m = mr*matTBA;
            m = matTAB*m;
        } else */{
            //double coef=(angle<angleArticulation)?angle*coefRayonCourbure/angleArticulation:coefRayonCourbure;
            double coef=coefRayonCourbure;
            matTAB.setTranslation(coef*AB);
            matTBA.setTranslation((-coef)*AB);
// TRT-1R
            m = matTBA*mr;
            m = mr*m;
            m = matTAB*m;
        }
        m = initialMatrix * m;
        if (step==nbRotationSteps) {
            firstRotation=false;
            firstStepMatrix = m;
            step=0;
        }
    } else {
        step++;
        double angle=angle2*step/nbRotationSteps;
        // TRT-1R
        Matrix mr;
        mr.setRotation(angle,axe2);
        //double coef=(angle>angle2-angleArticulation)?(angle2-angle)*coefRayonCourbure/(angle2-angleArticulation):coefRayonCourbure;
        double coef=coefRayonCourbure;
        Matrix matTCB,matTBC;
        matTCB.setTranslation(coef*CB);
        matTBC.setTranslation((-coef)*CB);

        m = matTBC*mr;
        m = mr*m;
        m = matTCB*m;
        m = firstStepMatrix * m;
        if (step>=nbRotationSteps) {
            step=nbRotationSteps;
            return true;
        }
    }
    return false;
}

void Rotations::getFinalPositionAndOrientation(Cell3DPosition &position, short &orientation) {
    Matrix mr,m;
    mr.setRotation(angle1,axe1);
    Matrix matTAB,matTBA;
    matTAB.setTranslation(AB);
    matTBA.setTranslation(-AB);
    m = matTBA*mr;
    m = mr*m;
    m = matTAB*m;
    firstStepMatrix = initialMatrix*m;

    mr.setRotation(angle2,axe2);
    matTAB.setTranslation(CB);
    matTBA.setTranslation(-CB);
    m = matTBA*mr;
    m = mr*m;
    m = matTAB*m;
    m = firstStepMatrix * m;

    Vector3D p(0,0,0,1),q = m * p;

    OUTPUT << "final=" << q << endl;
    position = getWorld()->worldToGridPosition(q);
    OUTPUT << "final grid=" << position << endl;

    orientation=Catoms3DBlock::getOrientationFromMatrix(m);
}

} // Catoms3D namespace
