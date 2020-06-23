/*!
 * \file okteenEvents.cpp
 * \brief Motion events for okteen modules
 * \date 17/07/2017
 * \author Benoît Piranda
 *
 * Special rotation/translation for Okteen modules
 *
 */

#include "okteenEvents.h"
#include "okteenWorld.h"

using namespace BaseSimulator::utils;

const int ANIMATION_DELAY=100000;
const int COM_DELAY=2000;

//===========================================================================================================
//
//          Catoms2DRotationStartEvent  (class)
//
//===========================================================================================================

OkteenMotionsStartEvent::OkteenMotionsStartEvent(Time t, const OkteenMotions &om): BlockEvent(t,om.module) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_OKTEEN_START;
    motion = om;
}

OkteenMotionsStartEvent::OkteenMotionsStartEvent(OkteenMotionsStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

OkteenMotionsStartEvent::~OkteenMotionsStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void OkteenMotionsStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    OkteenWorld::getWorld()->disconnectBlock(motion.module, false);

    motion.module->setColor(DARKGREY);
    motion.init();
    scheduler->schedule(new OkteenMotionsStepEvent(scheduler->now() + ANIMATION_DELAY, motion));
}

const string OkteenMotionsStartEvent::getEventName() {
    return("OkteenMotionsStart Event");
}

//===========================================================================================================
//
//          OkteenMotionsStepEvent  (class)
//
//===========================================================================================================

OkteenMotionsStepEvent::OkteenMotionsStepEvent(Time t, const OkteenMotions &om): BlockEvent(t,om.module) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_OKTEEN_STEP;

    motion=om;
}

OkteenMotionsStepEvent::OkteenMotionsStepEvent(OkteenMotionsStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

OkteenMotionsStepEvent::~OkteenMotionsStepEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void OkteenMotionsStepEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();

    Matrix mat;
    bool motionEnd=motion.nextStep(mat);

    OkteenWorld::getWorld()->updateGlData(motion.module,mat);
    if (motionEnd) {
        scheduler->schedule(new OkteenMotionsStopEvent(scheduler->now() + ANIMATION_DELAY, motion));
    } else {
        scheduler->schedule(new OkteenMotionsStepEvent(scheduler->now() + ANIMATION_DELAY, motion));
    }
}

const string OkteenMotionsStepEvent::getEventName() {
    return("OkteenMotionsStep Event");
}

//===========================================================================================================
//
//          OkteenMotionsStopEvent  (class)
//
//===========================================================================================================

OkteenMotionsStopEvent::OkteenMotionsStopEvent(Time t, const OkteenMotions& om): BlockEvent(t,om.module) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_OKTEEN_STOP;
    motion = om;
}

OkteenMotionsStopEvent::OkteenMotionsStopEvent(OkteenMotionsStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

OkteenMotionsStopEvent::~OkteenMotionsStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void OkteenMotionsStopEvent::consume() {
    EVENT_CONSUME_INFO();
#ifdef COLOR_MOTION_DEBUG
    motion.module->setColor(YELLOW);
#endif

    Cell3DPosition position;
/* Transformer les coordonnées GL en coordonnées grille*/
    motion.getFinalPosition(position);

    OkteenWorld *wrld=OkteenWorld::getWorld();

    motion.module->setPosition(position);
    stringstream info;
    info.str("");
    info << "connect Block " << motion.module->blockId;
    getScheduler()->trace(info.str(),motion.module->blockId,LIGHTBLUE);
    wrld->connectBlock(motion.module, false);
    Scheduler *scheduler = getScheduler();
    scheduler->schedule(new OkteenMotionsEndEvent(scheduler->now() + ANIMATION_DELAY, motion.module));
}

const string OkteenMotionsStopEvent::getEventName() {
    return("OkteenMotionsStop Event");
}

//===========================================================================================================
//
//          OkteenMotionsEndEvent  (class)
//
//===========================================================================================================

OkteenMotionsEndEvent::OkteenMotionsEndEvent(Time t, BuildingBlock* module): BlockEvent(t,module) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_OKTEEN_END;
}

OkteenMotionsEndEvent::OkteenMotionsEndEvent(OkteenMotionsEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

OkteenMotionsEndEvent::~OkteenMotionsEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void OkteenMotionsEndEvent::consume() {
    EVENT_CONSUME_INFO();
    OkteenBlockCode *bc = (OkteenBlockCode*)concernedBlock->blockCode;

    //module->blockCode->processLocalEvent(EventPtr(new OkteenMotionsEndEvent(date+COM_DELAY,module)));
    bc->onMotionEnd();
    StatsCollector::getInstance().incMotionCount();
    StatsIndividual::incMotionCount(concernedBlock->stats);
}

const string OkteenMotionsEndEvent::getEventName() {
    return("OkteenEnd Event");
}

//===========================================================================================================
//
//          OkteenMotions  (class)
//
//===========================================================================================================

OkteenMotions::OkteenMotions(OkteenBlock *mobile,SCLattice::Direction connector,SCLattice::Direction axisDir) {
    SCLattice* lattice = (SCLattice *)((Okteen::getWorld())->lattice);
    module = mobile;
    OUTPUT << "Motion" << endl;
    initPos = lattice->gridToWorldPosition(mobile->position);
    switch (connector) {
        case SCLattice::Left:
            switch (axisDir) {
                case SCLattice::Top:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(-1,1,0);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,1,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Bottom:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(-1,-1,0);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,-1,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Front:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(-1,0,-1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,0,-1));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Back:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(-1,0,1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,0,1));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                default :
                    OUTPUT << "Error: invalid motion" << endl;
                    break;
            }
            break;
        case SCLattice::Right:
            switch (axisDir) {
                case SCLattice::Top:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(1,-1,0);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,-1,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Bottom:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(1,1,0);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,1,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Front:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(1,0,1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,0,1));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Back:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(1,0,-1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,0,-1));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                default :
                    OUTPUT << "Error: invalid motion" << endl;
                    break;
            }
            break;
        case SCLattice::Front:
            switch (axisDir) {
                case SCLattice::Top:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(1,1,0);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(1,0,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Bottom:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(-1,1,0);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(-1,0,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Left:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(0,1,1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,0,1));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Right:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(0,1,-1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,0,-1));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                default :
                    OUTPUT << "Error: invalid motion" << endl;
                    break;
            }
            break;
        case SCLattice::Back:
            OUTPUT << "BACK" <<endl;
            initConnectorDir.set(0,-2.828427,0);
            switch (axisDir) {
                case SCLattice::Top:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(-1,-1,0);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(-1,0,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Bottom: {
                    OUTPUT << "BOTTOM"<<endl;
                    Cell3DPosition pos = mobile->position+Cell3DPosition(1,-1,0);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(1,0,0));
                        finalConnectorDir.set(0,-2.828427,0);
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                        finalConnectorDir.set(-2.828427,0,0);
                    }
                } break;
                case SCLattice::Left:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(0,-1,-1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,0,-1));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Right:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(0,-1,1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,0,1));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                default :
                    OUTPUT << "Error: invalid motion" << endl;
                    break;
            }
        case SCLattice::Top:
            switch (axisDir) {
                case SCLattice::Front:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(-1,0,1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(-1,0,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Back:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(1,0,1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(1,0,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Left:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(0,1,1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,1,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Right:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(0,-1,1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,-1,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                default :
                    OUTPUT << "Error: invalid motion" << endl;
                    break;
            }
        case SCLattice::Bottom:
            switch (axisDir) {
                case SCLattice::Front:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(1,0,-1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(1,0,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Back:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(-1,0,-1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(-1,0,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Left:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(0,1,1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,1,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                case SCLattice::Right:
                {   Cell3DPosition pos = mobile->position+Cell3DPosition(0,-1,-1);
                    if (!lattice->isFree(pos)) {
                        finalPos = lattice->gridToWorldPosition(mobile->position+Cell3DPosition(0,-1,0));
                    } else {
                        finalPos = lattice->gridToWorldPosition(pos);
                    }
                } break;
                default :
                    OUTPUT << "Error: invalid motion" << endl;
                    break;
            } break;
        default:
            OUTPUT << "Error: invalid motion" << endl;
            break;
    }
/*    translation = lattice->getNeighborRelativePos(connector);
      translation.pt[0]*=lattice->gridScale[0];
      translation.pt[1]*=lattice->gridScale[1];
      translation.pt[2]*=lattice->gridScale[2];
      axis=lattice->getNeighborRelativePos(axisDir);*/
}

bool OkteenMotions::nextStep(Matrix &m) {
    Matrix m1;
    Vector3D v;
    if (step<5) {
        v = initPos + (step/4.0)*(initConnectorDir);
        m.setTranslation(v);
    } else {
        v = initPos + initConnectorDir;
        m.setTranslation(v);
        if (step<15) {
            v = ((step-5)/9.0)*(finalPos+finalConnectorDir-initPos-initConnectorDir);
            m1.setTranslation(v);
            m = m1*m;
        } else {
            v = finalPos+(1.0-(step-15)/5.0)*(finalConnectorDir);
            m.setTranslation(v);
        }
    }

    step++;
    return (step>20);
}

void OkteenMotions::getFinalPosition(Cell3DPosition &position) {
    SCLattice* lattice = (SCLattice *)((Okteen::getWorld())->lattice);
    position = lattice->worldToGridPosition(finalPos);
}
