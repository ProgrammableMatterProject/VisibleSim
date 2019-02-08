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
#include "catoms3DMotionEngine.h"

using namespace BaseSimulator::utils;
using namespace Catoms3D;

mt19937 Rotations3D::rng = mt19937(std::random_device()());
int DELTA = 3;
uniform_int_distribution<std::mt19937::result_type>
Rotations3D::randomAnimationDelay = uniform_int_distribution<std::mt19937::result_type>
    (-(ANIMATION_DELAY / DELTA),ANIMATION_DELAY / DELTA);
const int Rotations3D::ANIMATION_DELAY = 400000;
const int Rotations3D::COM_DELAY = 0;//2000;
const int Rotations3D::nbRotationSteps = 20;
float Rotations3D::rotationDelayMultiplier = 1.0f;

std::ostream& Catoms3D::operator<<(std::ostream &stream, Rotations3D const& rots) {
    stream << rots.axe1 << "/" << rots.angle1 << " -- " << rots.axe2 << "/" << rots.angle2;
    return stream;
}

//===========================================================================================================
//
//          Rotation3DStartEvent  (class)
//
//===========================================================================================================

Rotation3DStartEvent::Rotation3DStartEvent(Time t, Catoms3DBlock *block,const Rotations3D &r): BlockEvent(t,block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION3D_START;
    rot = r;
}

Rotation3DStartEvent::Rotation3DStartEvent(Time t, Catoms3DBlock *m, Catoms3DBlock *pivot,
                                           const Cell3DPosition& tPos,
                                           RotationLinkType faceReq, bool exclusivelyReq)
    : Rotation3DStartEvent(t, m, pivot, pivot ? pivot->getConnectorId(tPos) : -1,
                           faceReq, exclusivelyReq)
{}

Rotation3DStartEvent::Rotation3DStartEvent(Time t, Catoms3DBlock *m,
                                           const Cell3DPosition& tPos,
                                           RotationLinkType faceReq, bool exclusively)
    : Rotation3DStartEvent(t, m,
                           // If not exclusively, fall back to any face type
                           (Catoms3DMotionEngine::findMotionPivot(m, tPos, faceReq) == NULL
                            and (faceReq != RotationLinkType::Any and not exclusively) ?
                            Catoms3DMotionEngine::findMotionPivot(m, tPos, Any)
                            : Catoms3DMotionEngine::findMotionPivot(m, tPos, faceReq)),
                           tPos, (Catoms3DMotionEngine::findMotionPivot(m, tPos, faceReq)==NULL
                                  and (faceReq != RotationLinkType::Any and not exclusively) ?
                                  Any : faceReq), exclusively)
{}

Rotation3DStartEvent::Rotation3DStartEvent(Time t, Catoms3DBlock *m, Catoms3DBlock *pivot,
                                           short toCon, RotationLinkType faceReq,
                                           bool exclusively) : BlockEvent(t, m) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION3D_START;

    if (not m)
        throw InvalidArgumentException(__PRETTY_FUNCTION__, "m = NULL");
    else if (not pivot)
        throw NoAvailableRotationPivotException(m->position);

    // Determine anchor connectors of module _pivot_ and m are connected to each other_
    short fromConM = m->getConnectorId(pivot->position);
    short fromConP = pivot->getConnectorId(m->position);

    // Deduce which connector of m will latch to pivot con toCon
    short toConM = Catoms3DMotionEngine::getMirrorConnectorOnModule(pivot, m, fromConP,
                                                                    fromConM, toCon);

    // Determine target cell of motion
    Cell3DPosition tPos = Cell3DPosition(-1,-1,-1);
    pivot->getNeighborPos(toCon, tPos);

    if (toConM == -1) {
        cerr << "cannot compute mirror connector of #" << pivot->blockId << "("
             << toCon << ") on module #" << m->blockId << endl;
        throw NoRotationPathForFaceException(m->position, pivot->position, tPos, faceReq);
    }

    // OUTPUT << "Building rotation from piv_con " << fromConP << " / " << m->position
    //        << " to piv_con " << toCon << "/ " << tPos
    //        << " [m_con(" << fromConM << " -> " << toConM << ")]"
    //        << " on surface of pivot #" << pivot->blockId << " " << pivot->position <<  endl;

    // VS_ASSERT_MSG(fromConM >= 0 and toConM >= 0,
    //               "attempting rotation to or from an unreachable position");
    if (fromConM < 0 or toConM < 0) {
        cerr << "attempting rotation to or from an unreachable position: " << m->position
             << " -> " << tPos<< endl;
        throw NoRotationPathForFaceException(m->position, pivot->position, tPos, faceReq);
    }


    // Get valid links on surface of m
    const Catoms3DMotionRulesLink* link =
        Catoms3DMotionEngine::findConnectorLink(m, fromConM, toConM, faceReq);

    if (not link and not exclusively) {
        link = Catoms3DMotionEngine::findConnectorLink(m,fromConM,toConM,RotationLinkType::Any);
    }

    if (link == NULL)
        throw NoRotationPathForFaceException(m->position, pivot->position, tPos, faceReq);
    else rot = link->getRotations(m, pivot);

    rot.conFromP = fromConP;
    rot.conToP = toCon;
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
    // cout << "[t-" << scheduler->now() << "] rotation starts" << endl;
    Catoms3DBlock *catom = (Catoms3DBlock *)concernedBlock;
    catom->setState(BuildingBlock::State::MOVING);

    Cell3DPosition position;
    short orientation;
    rot.getFinalPositionAndOrientation(position,orientation);

    catom->pivot = rot.pivot;

    // Trace module rotation
    stringstream info;
    info.str("");
    info << " starts rotating on pivot #" << rot.pivot->blockId << " ("
         << rot.conFromP << " -> " << rot.conToP << ")";
    scheduler->trace(info.str(),catom->blockId,LIGHTBLUE);

    // Trace pivot actuation
    info.str("");
    info << " starts actuating for module #" << catom->blockId << " ("
         << rot.conFromP << " -> " << rot.conToP << ")";
    scheduler->trace(info.str(),rot.pivot->blockId,YELLOW);

    scheduler->schedule(
        new PivotActuationStartEvent(scheduler->now(), const_cast<Catoms3DBlock*>(rot.pivot),
                                     rot.mobile, rot.conFromP, rot.conToP));

    Catoms3DWorld::getWorld()->disconnectBlock(catom);

    concernedBlock->blockCode->processLocalEvent(EventPtr(new Rotation3DStartEvent(date+Rotations3D::COM_DELAY, catom, rot)));

//    catom->setColor(DARKGREY);
    rot.init(((Catoms3DGlBlock*)catom->ptrGlBlock)->mat);
    scheduler->schedule(
        new Rotation3DStepEvent(scheduler->now()+Rotations3D::getNextRotationEventDelay(),
                                catom, rot));
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
    catom->setState(BuildingBlock::State::ALIVE);

    Scheduler *scheduler = getScheduler();
    // cout << "[t-" << scheduler->now() << "] rotation step" << endl;

    Matrix mat;
    bool rotationEnd=rot.nextStep(mat);

    Catoms3DWorld::getWorld()->updateGlData(catom,mat);
    if (rotationEnd) {
        scheduler->schedule(
            new Rotation3DStopEvent(scheduler->now(), catom, rot));
    } else {
        scheduler->schedule(new Rotation3DStepEvent(
                                scheduler->now()+Rotations3D::getNextRotationEventDelay(),
                                catom, rot));
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
    // cout << "[t-" << getScheduler()->now() << "] rotation stop" << endl;

    Catoms3DWorld *wrld=Catoms3DWorld::getWorld();
    Scheduler *scheduler = getScheduler();
    stringstream info; info.str("");
    Cell3DPosition position;
    short orientation;

    // Reset pivot
    catom->pivot = NULL;

    /* Transformer les coordonnées GL en coordonnées grille*/
    rot.getFinalPositionAndOrientation(position,orientation);
    catom->setPositionAndOrientation(position,orientation);
    wrld->connectBlock(catom);

    info << " finished rotating to " << position << " on pivot #" << rot.pivot->blockId << " ("
         << rot.conFromP << " -> " << rot.conToP << ")";
    scheduler->trace(info.str(),catom->blockId,LIGHTBLUE);

    scheduler->schedule(
        new Rotation3DEndEvent(scheduler->now(), catom));

    info.str("");
    info << " finished actuating for module #" << catom->blockId << " ("
         << rot.conFromP << " -> " << rot.conToP << ")";
    scheduler->trace(info.str(),rot.pivot->blockId,YELLOW);
    scheduler->schedule(
        new PivotActuationEndEvent(scheduler->now(), const_cast<Catoms3DBlock*>(rot.pivot),
                                   rot.mobile, rot.conFromP, rot.conToP));
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
    // cout << "[t-" << getScheduler()->now() << "] rotation ended" << endl;
    concernedBlock->blockCode->processLocalEvent(EventPtr(new Rotation3DEndEvent(date+Rotations3D::COM_DELAY,rb)));
    StatsCollector::getInstance().incMotionCount();
    StatsIndividual::incMotionCount(rb->stats);
}

const string Rotation3DEndEvent::getEventName() {
    return("Rotation3DEnd Event");
}

//===========================================================================================================
//
//          Rotations3D  (class)
//
//===========================================================================================================

Rotations3D::Rotations3D(const Catoms3DBlock *module, const Catoms3DBlock *fixe, double rprim,
                         const Vector3D &ax1, double ang1,
                         const Vector3D &ax2, double ang2,
                         short from, short to):
    mobile(module), pivot(fixe),
    conFromP(from), conToP(to),
    angle1(ang1), angle2(ang2) {

    static const double c_2 = 1.0/(3+sqrt(2));
    Matrix MA = ((Catoms3DGlBlock*)mobile->getGlBlock())->mat;
    Matrix MB = ((Catoms3DGlBlock*)fixe->getGlBlock())->mat;
    Matrix MA_1;

    // we calculate AB translation in A referentiel
    MA.inverse(MA_1);
    Matrix m = MA_1*MB;
    Vector3D AB = m*Vector3D(0,0,0,1);

    Matrix matTAB,matTBA;
    matTAB.setTranslation(AB);
    matTBA.setTranslation(-AB);

    axe1 = ax1.normer();

    double r=AB.norme()/2.0;
    double shift = (ang1>0)?c_2*r:-c_2*r;
    Vector3D V = AB^axe1;
    V.normer_interne();

    A0D0 = (0.5+0.5*rprim)*AB+shift*V;
    A0C0 = (0.5-0.5*rprim)*AB+shift*V;

    Matrix mr;
    mr.setRotation(angle1,axe1);
    finalMatrix = matTAB*(mr*(matTBA*mr));

    m  = MA*finalMatrix;
    m.inverse(MA_1);
    m = MA_1*MB;
    AB = m*Vector3D(0,0,0,1);

    finalMatrix.inverse(MA_1);
    axe2 = (MA_1*ax2).normer();


    matTAB.setTranslation(AB);
    matTBA.setTranslation(-AB);
    mr.setRotation(angle2,axe2);
    m = matTAB*(mr*(matTBA*mr));
    finalMatrix = finalMatrix*m;

    m = MA*finalMatrix;
    m.inverse(MA_1);
    m = MA_1*MB;
    AB = m*Vector3D(0,0,0,1);

    shift = (ang2>0)?-c_2*r:c_2*r;
    V = AB^axe2;
    V.normer_interne();

    A1D1 = (0.5+0.5*rprim)*AB+shift*V;
    A1C1 = (0.5-0.5*rprim)*AB+shift*V;
}

bool Rotations3D::nextStep(Matrix &m) {
    if (firstRotation) {
        step++;
        double angle=angle1*step/Rotations3D::nbRotationSteps;
        //OUTPUT << "step=" << step << "   angle=" << angle << endl;
        Matrix mr;
        mr.setRotation(angle,axe1);

        Matrix matTCA,matTDC,matTAD;
        matTCA.setTranslation(-A0C0);
        matTDC.setTranslation(-A0D0+A0C0);
        matTAD.setTranslation(A0D0);
        m = matTAD*(mr*(matTDC*(mr*matTCA)));
        m = initialMatrix * m;
//        OUTPUT << m.m[0] << " " << m.m[1] << " " << m.m[2] << " " << m.m[3] << " " << m.m[4] << " " << m.m[5] << " " << m.m[6] << " " << m.m[7] << " " << m.m[8] << " " << m.m[9] << " " << m.m[10] << " " << m.m[11] << " " << m.m[12] << " " << m.m[13] << " " << m.m[14] << " " << m.m[15] << endl;
        if (step==Rotations3D::nbRotationSteps) {
            firstRotation=false;
        }
    } else {
        step--;
        double angle=-angle2*step/Rotations3D::nbRotationSteps;
        // TRT-1R
        Matrix mr;
        mr.setRotation(angle,axe2);

        Matrix matTCA,matTDC,matTAD;
        matTCA.setTranslation(-A1C1);
        matTDC.setTranslation(-A1D1+A1C1);
        matTAD.setTranslation(A1D1);
        m = matTAD*(mr*(matTDC*(mr*matTCA)));
        m = finalMatrix * m;
//        OUTPUT << m.m[0] << " " << m.m[1] << " " << m.m[2] << " " << m.m[3] << " " << m.m[4] << " " << m.m[5] << " " << m.m[6] << " " << m.m[7] << " " << m.m[8] << " " << m.m[9] << " " << m.m[10] << " " << m.m[11] << " " << m.m[12] << " " << m.m[13] << " " << m.m[14] << " " << m.m[15] << endl;
        if (step==0) {
            return true;
        }
    }
    return false;
}

void Rotations3D::getFinalPositionAndOrientation(Cell3DPosition &position, short &orientation) {
    Vector3D p(0,0,0,1),q = finalMatrix * p;

//    OUTPUT << "final=" << q << endl;
    position = Catoms3D::getWorld()->lattice->worldToGridPosition(q);
//    OUTPUT << "final grid=" << position << endl;
    orientation=Catoms3DBlock::getOrientationFromMatrix(finalMatrix);
}
