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

const int ANIMATION_DELAY=50000;
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

    //    datom->setColor(DARKGREY);
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
    bool rotationEnd=deform.nextStep(mat);
cout << datom->blockId << endl << mat << endl;
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
    mobileShape = mid;
    pivotShape = pid;
    copy(blockingModules.begin(), blockingModules.end(),std::back_inserter(animated));

    setup(C1,V1,C2,V2);
}

void Deformation::setup(const Vector3D &C1,const Vector3D &V1,const Vector3D &C2,const Vector3D &V2) {
    Matrix MA = ptrPivot->getGlBlock()->mat;
    Matrix MB = ptrMobile->getGlBlock()->mat;
    Matrix MA_1;

    initialMatrix=MB;
    // we calculate BA translation
    MA.inverse(MA_1);
    Matrix m = MA_1*MB;

    Matrix matTC,matTC_1;
    matTC.setTranslation(C1);
    matTC_1.setTranslation(-C1);
    //OUTPUT << "matT_C1:\n" << matTC;
    //OUTPUT << "matT_C1-1:\n" << matTC_1;
    Matrix R;
    R.setRotation(90.0,V1);
    //OUTPUT << "R1:\n" << R;

    m = matTC*(R*(matTC_1*m));
    interMatrix = MA*m;
    //OUTPUT << C1 << "/" << V1 << endl;
    //OUTPUT << (interMatrix) << endl;

    matTC.setTranslation(C2);
    matTC_1.setTranslation(-C2);
    //OUTPUT << "matT_C2:\n" << matTC;
    //OUTPUT << "matT_C2-1:\n" << matTC_1;

    R.setRotation(-90.0,V2);
    finalMatrix = MA*matTC*(R*(matTC_1*m));

    //OUTPUT << C2 << "/" << V2 << endl;
    //OUTPUT << finalMatrix << endl;
}

bool Deformation::nextStep(Matrix &m) {
    step++;
    cout << getScheduler()->now() << ":" << step << endl;
    DatomsWorld *wrl = DatomsWorld::getWorld();
    switch (step) {
        case 1:
            m = initialMatrix;
            for (pair<const DatomsBlock*,PistonId> data:animated) {
                wrl->updateGlData(data.first,data.second);
            }
        break;
        case 2:
            m = interMatrix;
            wrl->updateGlData(ptrMobile,mobileShape);
            wrl->updateGlData(ptrPivot,pivotShape);
        break;
        case 3:
            m = finalMatrix;
            wrl->updateGlData(ptrMobile,AllPistonsOff);
            wrl->updateGlData(ptrPivot,AllPistonsOff);
        break;
        case 4:
            m = finalMatrix;
            for (pair<const DatomsBlock*,PistonId> data:animated) {
                wrl->updateGlData(data.first,AllPistonsOff);
            }
            break;

    }
    return step==4;
}

void Deformation::getFinalPositionAndOrientation(Cell3DPosition &position, short &orientation) {
    Vector3D p(0,0,0,1),q = finalMatrix * p;

    //OUTPUT << "final=" << q << endl;
    position = Datoms::getWorld()->lattice->worldToGridPosition(q);
    //OUTPUT << "final grid=" << position << " verif=" << Datoms::getWorld()->lattice->gridToWorldPosition(position) << endl;
    orientation=DatomsBlock::getOrientationFromMatrix(finalMatrix);
}
