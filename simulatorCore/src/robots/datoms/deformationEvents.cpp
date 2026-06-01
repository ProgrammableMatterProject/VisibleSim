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
const int stepGrp=20;
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
    // disconnect all but the pivot
    DatomsWorld::getWorld()->disconnectBlock(datom, deform.ptrPivot);

    deform.init();
    scheduler->schedule(new DeformationStepEvent(scheduler->now() + COM_DELAY,datom, deform));
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
    auto res=deform.computeMatrixAtStep(mat);
    //OUTPUT << datom->blockId << endl << mat << endl;
    auto wrl=DatomsWorld::getWorld();
    if (res.first>=2) {
        wrl->updateGlData(deform.ptrMobile,AllPistonsOff,0);
        wrl->updateGlData(deform.ptrPivot,AllPistonsOff,0);
        scheduler->schedule(new DeformationStopEvent(scheduler->now() + ANIMATION_DELAY, datom, deform));
    } else {
        wrl->updateGlData(datom,mat);
        wrl->updateGlData(deform.ptrMobile,deform.mobilePiston,res.second);
        wrl->updateGlData(deform.ptrPivot,deform.pivotPiston,res.second);
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
    OUTPUT<< "connected" << endl;
    int i=1;
    for (int i=0; i<12; i++) {
        OUTPUT << i << ":" << datom->getInterface(i)->getConnectedBlockBId() << endl;
    }
    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new DeformationEndEvent(scheduler->now() + COM_DELAY, datom));
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
Deformation::Deformation(const DatomsBlock *mobile,const DatomsBlock *pivot,const Vector3D &C1,const Vector3D &V1,const Vector3D &C2,const Vector3D &V2,PistonId mid,PistonId pid, vector<pair<DatomsBlock*,PistonId>> blockingModules) {
    ptrPivot = pivot;
    ptrMobile = mobile;
    mobilePiston = mid;
    pivotPiston = pid;
    copy(blockingModules.begin(), blockingModules.end(),std::back_inserter(animated));
    Caxis1 = C1;
    Caxis2 = C2;
    Vaxis1 = V1;
    Vaxis2 = V2;
    setup();
}

void Deformation::setup() {
    //Matrix MA = ptrPivot->getGlBlock()->mat;
    Matrix MB = ptrMobile->getGlBlock()->mat;
    Matrix MA_1;

    //OUTPUT << "mobile:\n" << MB;
    //OUTPUT << "pivot:\n" << MA;
    initialMatrix=MB;

    Matrix matTC,matTC_1;
    matTC.setTranslation(Caxis1);
    matTC_1.setTranslation(-Caxis1);
    //OUTPUT << "matT_C1:\n" << matTC;
    //OUTPUT << "matT_C1-1:\n" << matTC_1;
    Matrix R;
    R.setRotation(-90.0,Vaxis1);
    //OUTPUT << "R1:\n" << R;

    Matrix matR1 = matTC*(R*matTC_1);
    //OUTPUT << "Rotation 1:\n" << matR1;
    interMatrix = MB*matR1;
    //OUTPUT << "C1="<< C1 << "/ V1=" << V1 << endl;
    //OUTPUT << "inter=\n" << (interMatrix) << endl;
    //Vector3D p(0,0,0,1),q = interMatrix * p;
    //OUTPUT << "interPos=" << q << endl;
    matTC.setTranslation(Caxis2);
    matTC_1.setTranslation(-Caxis2);
    //OUTPUT << "matT_C2:\n" << matTC;
    //OUTPUT << "matT_C2-1:\n" << matTC_1;

    R.setRotation(90.0,Vaxis2);
    Matrix matR2 = matTC*(R*matTC_1);
    finalMatrix = MB*(matR1*matR2);
    //OUTPUT << "C2="<< C2 << "/ V2=" << V2 << endl;
    //OUTPUT << "Rotation 2:\n" << matR2 ;
    //OUTPUT << "finalM=\n" << finalMatrix;

    /*q = finalMatrix * p;
    OUTPUT << "final=" << q  << endl;*/
}


pair<int,float> Deformation::computeMatrixAtStep(Matrix &m) {
    float morphing=0.0;
    step++;
    //OUTPUT << "step: " << step <<endl;
    int grp = step/stepGrp;
    if (grp==0) {
        float coef = float(step)/stepGrp;
        morphing=coef;
        Matrix matTC,matTC_1;
        matTC.setTranslation(Caxis1);
        matTC_1.setTranslation(-Caxis1);
        Matrix R;
        R.setRotation(-90.0*coef,Vaxis1);
        //OUTPUT << "R1:\n" << R;

        Matrix matR1 = matTC*(R*matTC_1);
        //OUTPUT << "Rotation 1:\n" << matR1;
        m = initialMatrix*matR1;
    } else if (grp==1) {
        float coef = float(step-stepGrp)/stepGrp;
        morphing=1.0-coef;
        Matrix matTC,matTC_1;
        matTC.setTranslation(Caxis2);
        matTC_1.setTranslation(-Caxis2);
        //OUTPUT << "matT_C2:\n" << matTC;
        //OUTPUT << "matT_C2-1:\n" << matTC_1;
        Matrix R;
        R.setRotation(90.0*coef,Vaxis2);
        Matrix matR2 = matTC*(R*matTC_1);
        m = interMatrix*matR2;
    }
    return {grp,morphing};
}

void Deformation::getFinalPositionAndOrientation(Cell3DPosition &position, short &orientation) {
    Vector3D p(0,0,0,1),q = finalMatrix * p;

    //OUTPUT << "final=" << q << endl;
    position = Datoms::getWorld()->lattice->worldToGridPosition(q);
    //OUTPUT << "final grid=" << position << " verif=" << Datoms::getWorld()->lattice->gridToWorldPosition(position) << endl;
    orientation=DatomsBlock::getOrientationFromMatrix(finalMatrix);
}
