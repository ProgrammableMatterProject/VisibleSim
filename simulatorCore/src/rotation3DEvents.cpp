/*
 * @file rotation3DEvents.cpp
 * 
 * formerly catoms3DEvents.cpp
 * 
 *  Created on: 18/07/2016
 *      Author: Benoit Piranda, Pierre Thalamy
 */

#include "rotation3DEvents.h"
#include "catoms3DWorld.h"

using namespace BaseSimulator::utils;

const int ANIMATION_DELAY=100000;
const int COM_DELAY=2000;

//===========================================================================================================
//
//          Rotation2DStartEvent  (class)
//
//===========================================================================================================

Rotation3DStartEvent::Rotation3DStartEvent(Time t, Catoms3DBlock *block,const Rotations3D &r): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION3D_START;
    rot = r;
}

Rotation3DStartEvent::Rotation3DStartEvent(Rotation3DStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Rotation3DStartEvent::~Rotation3DStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Rotation3DStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    Catoms3DBlock *catom = (Catoms3DBlock *)concernedBlock;
    Catoms3DWorld::getWorld()->disconnectBlock(catom);
    catom->setColor(DARKGREY);
    rot.init(((Catoms3DGlBlock*)catom->ptrGlBlock)->mat);
    scheduler->schedule(new Rotation3DStepEvent(scheduler->now() + ANIMATION_DELAY,catom, rot));
}

const string Rotation3DStartEvent::getEventName() {
    return("Rotation3DStart Event");
}

//===========================================================================================================
//
//          Rotation3DStepEvent  (class)
//
//===========================================================================================================

Rotation3DStepEvent::Rotation3DStepEvent(Time t, Catoms3DBlock *block,const Rotations3D &r): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION3D_STEP;

    rot=r;
}

Rotation3DStepEvent::Rotation3DStepEvent(Rotation3DStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Rotation3DStepEvent::~Rotation3DStepEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Rotation3DStepEvent::consume() {
    EVENT_CONSUME_INFO();
    Catoms3DBlock *catom = (Catoms3DBlock*)concernedBlock;
    Scheduler *scheduler = getScheduler();

    Matrix mat;
    bool rotationEnd=rot.nextStep(mat);

    Catoms3DWorld::getWorld()->updateGlData(catom,mat);
    if (rotationEnd) {
        scheduler->schedule(new Rotation3DStopEvent(scheduler->now() + ANIMATION_DELAY, catom, rot));
    } else {
        scheduler->schedule(new Rotation3DStepEvent(scheduler->now() + ANIMATION_DELAY, catom, rot));
    }
}

const string Rotation3DStepEvent::getEventName() {
    return("Rotation3DStep Event");
}

//===========================================================================================================
//
//          Rotation3DStepEvent  (class)
//
//===========================================================================================================

Rotation3DStopEvent::Rotation3DStopEvent(Time t, Catoms3DBlock *block,const Rotations3D& r): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION3D_STOP;
    rot = r;
}

Rotation3DStopEvent::Rotation3DStopEvent(Rotation3DStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Rotation3DStopEvent::~Rotation3DStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Rotation3DStopEvent::consume() {
    EVENT_CONSUME_INFO();
    Catoms3DBlock *catom = (Catoms3DBlock*)concernedBlock;
    catom->setColor(YELLOW);

    Cell3DPosition position;
    short orientation;
/* Transformer les coordonnées GL en coordonnées grille*/
    rot.getFinalPositionAndOrientation(position,orientation);

    Catoms3DWorld *wrld=Catoms3DWorld::getWorld();

    catom->setPositionAndOrientation(position,orientation);
    stringstream info;
    info.str("");
    info << "connect Block " << catom->blockId;
    getScheduler()->trace(info.str(),catom->blockId,LIGHTBLUE);
    wrld->connectBlock(catom);
    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new Rotation3DEndEvent(scheduler->now() + ANIMATION_DELAY, catom));
}

const string Rotation3DStopEvent::getEventName() {
    return("Rotation3DStop Event");
}

//===========================================================================================================
//
//          Rotation3DEndEvent  (class)
//
//===========================================================================================================

Rotation3DEndEvent::Rotation3DEndEvent(Time t, Catoms3DBlock *block): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION3D_END;
}

Rotation3DEndEvent::Rotation3DEndEvent(Rotation3DEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Rotation3DEndEvent::~Rotation3DEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Rotation3DEndEvent::consume() {
    EVENT_CONSUME_INFO();
    Catoms3DBlock *rb = (Catoms3DBlock*)concernedBlock;
    concernedBlock->blockCode->processLocalEvent(EventPtr(new Rotation3DEndEvent(date+COM_DELAY,rb)));
    StatsCollector::getInstance().incMotionCount();
}

const string Rotation3DEndEvent::getEventName() {
    return("Rotation3DEnd Event");
}

//===========================================================================================================
//
//          Rotations3D  (class)
//
//===========================================================================================================

Rotations3D::Rotations3D(Catoms3DBlock *mobile,Catoms3DBlock *fixe,const Vector3D &ax1,
                         double ang1,const Vector3D &ax2,double ang2):angle1(ang1),angle2(ang2) {
    static const double c_2 = 0.5/(3+sqrt(2));
    Matrix MA = ((Catoms3DGlBlock*)mobile->getGlBlock())->mat;
    Matrix MB = ((Catoms3DGlBlock*)fixe->getGlBlock())->mat;
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

bool Rotations3D::nextStep(Matrix &m) {
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

void Rotations3D::getFinalPositionAndOrientation(Cell3DPosition &position, short &orientation) {
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
    position = Catoms3D::getWorld()->lattice->worldToGridPosition(q);
    OUTPUT << "final grid=" << position << endl;

    orientation=Catoms3DBlock::getOrientationFromMatrix(m);
}
