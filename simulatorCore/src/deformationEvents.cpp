/*
 * @file deformationEvents.cpp
 *
 * formerly deformationEvents.cpp
 *
 *  Created on: 18/07/2016
 *      Author: Benoit Piranda, Pierre Thalamy
 */

#include "deformationEvents.h"
#include "datomsWorld.h"

using namespace BaseSimulator::utils;

const int ANIMATION_DELAY=100000;
const int COM_DELAY=2000;

//===========================================================================================================
//
//          DeformationStartEvent  (class)
//
//===========================================================================================================

DeformationStartEvent::DeformationStartEvent(Time t, DatomsBlock *block,const Deformation &d): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_DEFORMATION_START;
    deform = d;
}

DeformationStartEvent::DeformationStartEvent(DeformationStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

DeformationStartEvent::~DeformationStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void DeformationStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    DatomsBlock *datom = (DatomsBlock *)concernedBlock;
    DatomsWorld::getWorld()->disconnectBlock(datom, false);

    datom->getGlBlock()->currentModel=deform.modelId;
    cout << "Model="<< (int)deform.modelId << endl;
//    datom->setColor(DARKGREY);
    deform.init(((DatomsGlBlock*)datom->ptrGlBlock)->mat);
    scheduler->schedule(new DeformationStepEvent(scheduler->now() + ANIMATION_DELAY,datom, deform));
}

const string DeformationStartEvent::getEventName() {
    return("DeformationStart Event");
}

//===========================================================================================================
//
//          DeformationStepEvent  (class)
//
//===========================================================================================================

DeformationStepEvent::DeformationStepEvent(Time t, DatomsBlock *block,const Deformation &r): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_DEFORMATION_STEP;

    deform=r;
}

DeformationStepEvent::DeformationStepEvent(DeformationStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

DeformationStepEvent::~DeformationStepEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void DeformationStepEvent::consume() {
    EVENT_CONSUME_INFO();
    DatomsBlock *datom = (DatomsBlock*)concernedBlock;
    Scheduler *scheduler = getScheduler();

    Matrix mat;
    bool rotationEnd=deform.nextStep(mat);

    DatomsWorld::getWorld()->updateGlData(datom,mat);
    if (rotationEnd) {
        scheduler->schedule(new DeformationStopEvent(scheduler->now() + ANIMATION_DELAY, datom, deform));
    } else {
        scheduler->schedule(new DeformationStepEvent(scheduler->now() + ANIMATION_DELAY, datom, deform));
    }
}

const string DeformationStepEvent::getEventName() {
    return("DeformationStep Event");
}

//===========================================================================================================
//
//          DeformationStepEvent  (class)
//
//===========================================================================================================

DeformationStopEvent::DeformationStopEvent(Time t, DatomsBlock *block,const Deformation& d): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_DEFORMATION_STOP;
    deform = d;
}

DeformationStopEvent::DeformationStopEvent(DeformationStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

DeformationStopEvent::~DeformationStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void DeformationStopEvent::consume() {
    EVENT_CONSUME_INFO();
    DatomsBlock *datom = (DatomsBlock*)concernedBlock;
//    datom->setColor(YELLOW);

    Cell3DPosition position;
    short orientation;
/* Transformer les coordonnées GL en coordonnées grille*/
    deform.getFinalPositionAndOrientation(position,orientation);

    DatomsWorld *wrld=DatomsWorld::getWorld();

    datom->setPositionAndOrientation(position,orientation);
    stringstream info;
    info.str("");
    info << "connect Block " << datom->blockId;
    getScheduler()->trace(info.str(),datom->blockId,LIGHTBLUE);
    wrld->connectBlock(datom, false);
    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new DeformationEndEvent(scheduler->now() + ANIMATION_DELAY, datom));
}

const string DeformationStopEvent::getEventName() {
    return("DeformationStop Event");
}

//===========================================================================================================
//
//          DeformationEndEvent  (class)
//
//===========================================================================================================

DeformationEndEvent::DeformationEndEvent(Time t, DatomsBlock *block): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_DEFORMATION_END;
}

DeformationEndEvent::DeformationEndEvent(DeformationEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

DeformationEndEvent::~DeformationEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void DeformationEndEvent::consume() {
    EVENT_CONSUME_INFO();
    DatomsBlock *rb = (DatomsBlock*)concernedBlock;
    rb->getGlBlock()->currentModel=1;
    // Bizarre !
    concernedBlock->blockCode->processLocalEvent(EventPtr(new DeformationEndEvent(date+COM_DELAY,rb)));
    StatsCollector::getInstance().incMotionCount();
    StatsIndividual::incMotionCount(rb->stats);
}

const string DeformationEndEvent::getEventName() {
    return("DeformationEnd Event");
}

//===========================================================================================================
//
//          Deformation  (class)
//
//===========================================================================================================

Deformation::Deformation(const DatomsBlock *mobile,const DatomsBlock *fixe,const Vector3D &ax1,const Vector3D &ax2,uint8_t id) {
    static const double cmax_2 = 1.0/(3*sqrt(2)-1);
    Matrix MA = ((DatomsGlBlock*)mobile->getGlBlock())->mat;
    Matrix MB = ((DatomsGlBlock*)fixe->getGlBlock())->mat;
    Matrix MA_1;

    modelId = id;

    // we calculate AB translation in A referentiel
    MA.inverse(MA_1);
    Matrix m = MA_1*MB;
    Vector3D AB = m*Vector3D(0,0,0,1);

    Matrix matTAB,matTBA;
    matTAB.setTranslation(AB);
    matTBA.setTranslation(-AB);

    axe1 = ax1.normer();

    double r=AB.norme()/2.0;
    double shift = -cmax_2*r;
    Vector3D V = AB^axe1;
    V.normer_interne();

    A0P0 = 0.5*AB+shift*V;

    Matrix mr;
    mr.setRotation(45.0,axe1);
    finalMatrix = matTAB*(mr*(matTBA*mr));

    m  = MA*finalMatrix;
    m.inverse(MA_1);
    m = MA_1*MB;
    AB = m*Vector3D(0,0,0,1);

    finalMatrix.inverse(MA_1);
    axe2 = (MA_1*ax2).normer();

    matTAB.setTranslation(AB);
    matTBA.setTranslation(-AB);
    mr.setRotation(45.0,axe2);
    m = matTAB*(mr*(matTBA*mr));
    finalMatrix = finalMatrix*m;

    m = MA*finalMatrix;
    m.inverse(MA_1);
    m = MA_1*MB;
    AB = m*Vector3D(0,0,0,1);

    shift = cmax_2*r;
    V = AB^axe2;
    V.normer_interne();

    A1P1 = 0.5*AB+shift*V;
}

bool Deformation::nextStep(Matrix &m) {
    ++step;
    if (step<nbSteps_4) {
        double angle=90.0*step/nbSteps_4;
        //OUTPUT << "step=" << step << "   angle=" << angle << endl;
        Matrix mr;
        mr.setRotation(angle,axe1);

        Matrix matTPA,matTAP;
        matTPA.setTranslation(-A0P0);
        matTAP.setTranslation(A0P0);
        m = matTAP*(mr*matTPA);
        m = initialMatrix * m;
        return false;
    } else if (step<3*nbSteps_4) {
        Matrix mr;
        mr.setRotation(90,axe1);

        Matrix matTPA,matTAP;
        matTPA.setTranslation(-A0P0);
        matTAP.setTranslation(A0P0);
        m = matTAP*(mr*matTPA);
        m = initialMatrix * m;
        return false;
    } else if (step<4*nbSteps_4) {
        double angle=-90+90.0*(step-3*nbSteps_4)/nbSteps_4;

        Matrix mr;
        mr.setRotation(angle,axe2);

        Matrix matTPA,matTAP;
        matTPA.setTranslation(-A1P1);
        matTAP.setTranslation(A1P1);
        m = matTAP*(mr*matTPA);
        m = finalMatrix * m;
        return false;
    }
    return true;
}

void Deformation::getFinalPositionAndOrientation(Cell3DPosition &position, short &orientation) {
    Vector3D p(0,0,0,1),q = finalMatrix * p;

//    OUTPUT << "final=" << q << endl;
    position = Datoms::getWorld()->lattice->worldToGridPosition(q);
//    OUTPUT << "final grid=" << position << endl;
    orientation=DatomsBlock::getOrientationFromMatrix(finalMatrix);
}
