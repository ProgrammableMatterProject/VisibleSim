/*
 * @file rotation3DEvents.cpp
 *
 * formerly catoms3DEvents.cpp
 *
 *  Created on: 18/07/2016
 *      Author: Benoit Piranda, Pierre Thalamy
 */

#include "catoms3DRotationEvents.h"
#include "catoms3DWorld.h"
#include "catoms3DMotionEngine.h"
#include "../../replay/replayExporter.h"

using namespace BaseSimulator::utils;
using namespace Catoms3D;

mt19937 Catoms3DRotation::rng = mt19937(std::random_device()());
int DELTA = 3;
uniform_int_distribution<std::mt19937::result_type>
        Catoms3DRotation::randomAnimationDelay = uniform_int_distribution<std::mt19937::result_type>
        (-(ANIMATION_DELAY / DELTA), ANIMATION_DELAY / DELTA);
const int Catoms3DRotation::ANIMATION_DELAY = 400000;
const int Catoms3DRotation::COM_DELAY = 0;//2000;
const int Catoms3DRotation::nbRotationSteps = 20;

std::ostream &Catoms3D::operator<<(std::ostream &stream, Catoms3DRotation const &rots) {
    stream << rots.axe1 << "/" << rots.angle1 << " -- " << rots.axe2 << "/" << rots.angle2;
    return stream;
}

//===========================================================================================================
//
//          Catoms3DRotationStartEvent  (class)
//
//===========================================================================================================

Catoms3DRotationStartEvent::Catoms3DRotationStartEvent(Time t, Catoms3DBlock *block, const Catoms3DRotation &r)
        : BlockEvent(t, block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION3D_START;
    rot = r;
}

Catoms3DRotationStartEvent::Catoms3DRotationStartEvent(Time t, Catoms3DBlock *m, const Catoms3DBlock *pivot,
                                                       const Cell3DPosition &tPos,
                                                       RotationLinkType faceReq, bool exclusivelyReq)
        : Catoms3DRotationStartEvent(t, m, pivot, pivot ? pivot->getConnectorId(tPos) : -1,
                                     faceReq, exclusivelyReq) {}

Catoms3DRotationStartEvent::Catoms3DRotationStartEvent(Time t, Catoms3DBlock *m,
                                                       const Cell3DPosition &tPos,
                                                       RotationLinkType faceReq, bool exclusively)
        : Catoms3DRotationStartEvent(t, m,
        // If not exclusively, fall back to any face type
                                     (Catoms3DMotionEngine::findMotionPivot(m, tPos, faceReq) == NULL
                                      and (faceReq != RotationLinkType::Any and not exclusively) ?
                                      Catoms3DMotionEngine::findMotionPivot(m, tPos, Any)
                                                                                                 : Catoms3DMotionEngine::findMotionPivot(
                                                     m, tPos, faceReq)),
                                     tPos, (Catoms3DMotionEngine::findMotionPivot(m, tPos, faceReq) == NULL
                                            and (faceReq != RotationLinkType::Any and not exclusively) ?
                                            Any : faceReq), exclusively) {}

Catoms3DRotationStartEvent::Catoms3DRotationStartEvent(Time t, Catoms3DBlock *m, const Catoms3DBlock *pivot,
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
    Cell3DPosition tPos = Cell3DPosition(-1, -1, -1);
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
             << " -> " << tPos << endl;
        throw NoRotationPathForFaceException(m->position, pivot->position, tPos, faceReq);
    }

    // Get valid links on surface of m
    const Catoms3DMotionRulesLink *link =
            Catoms3DMotionEngine::findConnectorLink(m, fromConM, toConM, faceReq);

    if (not link and not exclusively) {
        link = Catoms3DMotionEngine::findConnectorLink(m, fromConM, toConM, RotationLinkType::Any);
    }

    if (link == NULL)
        throw NoRotationPathForFaceException(m->position, pivot->position, tPos, faceReq);
    else rot = link->getRotations(m, pivot);

    rot.conFromP = fromConP;
    rot.conToP = toCon;

    /*if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeCatoms3DMotion(getScheduler()->now(), rot.mobile->blockId,
                                                           Catoms3DRotation::ANIMATION_DELAY *
                                                           Catoms3DRotation::nbRotationSteps / 2,
                                                           rot.pivot->blockId,
                                                           (faceReq == RotationLinkType::HexaFace ? 3 : 4),
                                                           rot.getAxe1(), rot.getAxe2());*/
}

Catoms3DRotationStartEvent::Catoms3DRotationStartEvent(Catoms3DRotationStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Catoms3DRotationStartEvent::~Catoms3DRotationStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Catoms3DRotationStartEvent::consume() {
    EVENT_CONSUME_INFO();
    Scheduler *scheduler = getScheduler();
    // cout << "[t-" << scheduler->now() << "] rotation starts" << endl;
    Catoms3DBlock *catom = (Catoms3DBlock *) concernedBlock;
    catom->setState(BuildingBlock::State::MOVING);

    Cell3DPosition position;
    short orientation;
    rot.getFinalPositionAndOrientation(position, orientation);

    catom->pivot = rot.pivot;

    // Trace module rotation
    stringstream info;
    info.str("");
    info << " starts rotating on pivot #" << rot.pivot->blockId << " ("
         << rot.conFromP << " -> " << rot.conToP << ")";
    scheduler->trace(info.str(), catom->blockId, LIGHTBLUE);

    // Trace pivot actuation
    info.str("");
    info << " starts actuating for module #" << catom->blockId << " ("
         << rot.conFromP << " -> " << rot.conToP << ")";
    scheduler->trace(info.str(), rot.pivot->blockId, YELLOW);

    scheduler->schedule(
            new PivotActuationStartEvent(scheduler->now(), const_cast<Catoms3DBlock *>(rot.pivot),
                                         rot.mobile, rot.conFromP, rot.conToP));

    Catoms3DWorld::getWorld()->disconnectBlock(catom, false);

    concernedBlock->blockCode->processLocalEvent(
            EventPtr(new Catoms3DRotationStartEvent(date + Catoms3DRotation::COM_DELAY, catom, rot)));

//    catom->setColor(DARKGREY);
    rot.init(((Catoms3DGlBlock *) catom->ptrGlBlock)->mat);
    scheduler->schedule(
            new Catoms3DRotationStepEvent(scheduler->now() + Catoms3DRotation::getNextRotationEventDelay(),
                                          catom, rot));

    //TODO ORIENTATION
    Cell3DPosition nil;
    short originOrientation;

    rot.getFinalPositionAndOrientation(position, orientation);
    //rot.getFinalPositionAndOrientation(nil,finalOrientation);
    originOrientation = catom->getOrientationFromMatrix(catom->getGlBlock()->mat);
    //cout<<"Begin orientation : "<<originOrientation<<endl;
    //cout<<"End orientation : "<<orientation<<endl;

    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeCatoms3DMotion(getScheduler()->now(), rot.mobile->blockId,
                                                           Catoms3DRotation::ANIMATION_DELAY, position, orientation,
                                                           catom->position, originOrientation,
                                                           rot.pivot->blockId, rot.getCode(), rot.getAxe1(),
                                                           rot.getAxe2());
    //(link->getMRLT() == HexaFace?3:4)
}

const string Catoms3DRotationStartEvent::getEventName() {
    return ("Catoms3DRotationStart Event");
}

//===========================================================================================================
//
//          Catoms3DRotationStepEvent  (class)
//
//===========================================================================================================

Catoms3DRotationStepEvent::Catoms3DRotationStepEvent(Time t, Catoms3DBlock *block, const Catoms3DRotation &r)
        : BlockEvent(t, block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION3D_STEP;

    rot = r;
}

Catoms3DRotationStepEvent::Catoms3DRotationStepEvent(Catoms3DRotationStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Catoms3DRotationStepEvent::~Catoms3DRotationStepEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Catoms3DRotationStepEvent::consume() {
    EVENT_CONSUME_INFO();
    Catoms3DBlock *catom = (Catoms3DBlock *) concernedBlock;

    Scheduler *scheduler = getScheduler();
    // cout << "[t-" << scheduler->now() << "] rotation step" << endl;

    Matrix mat;
    bool rotationEnd = rot.nextStep(mat);

    Catoms3DWorld::getWorld()->updateGlData(catom, mat);
    if (rotationEnd) {
        scheduler->schedule(
                new Catoms3DRotationStopEvent(scheduler->now(), catom, rot));
    } else {
        scheduler->schedule(new Catoms3DRotationStepEvent(
                scheduler->now() + Catoms3DRotation::getNextRotationEventDelay(),
                catom, rot));
    }
}

const string Catoms3DRotationStepEvent::getEventName() {
    return ("Catoms3DRotationStep Event");
}

//===========================================================================================================
//
//          Catoms3DRotationStepEvent  (class)
//
//===========================================================================================================

Catoms3DRotationStopEvent::Catoms3DRotationStopEvent(Time t, Catoms3DBlock *block, const Catoms3DRotation &r)
        : BlockEvent(t, block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION3D_STOP;
    rot = r;
}

Catoms3DRotationStopEvent::Catoms3DRotationStopEvent(Catoms3DRotationStepEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Catoms3DRotationStopEvent::~Catoms3DRotationStopEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Catoms3DRotationStopEvent::consume() {
    EVENT_CONSUME_INFO();
    Catoms3DBlock *catom = (Catoms3DBlock *) concernedBlock;
    // cout << "[t-" << getScheduler()->now() << "] rotation stop" << endl;

    Catoms3DWorld *wrld = Catoms3DWorld::getWorld();
    Scheduler *scheduler = getScheduler();
    stringstream info;
    info.str("");
    Cell3DPosition position;
    short orientation;

    // Reset pivot
    catom->pivot = NULL;

    /* Transformer les coordonnées GL en coordonnées grille*/
    rot.getFinalPositionAndOrientation(position, orientation);
    catom->setPositionAndOrientation(position, orientation);
    wrld->connectBlock(catom, false);

    info << " finished rotating to " << position << " on pivot #" << rot.pivot->blockId << " ("
         << rot.conFromP << " -> " << rot.conToP << ")";
    scheduler->trace(info.str(), catom->blockId, LIGHTBLUE);

    scheduler->schedule(
            new Catoms3DRotationEndEvent(scheduler->now(), catom));

    info.str("");
    info << " finished actuating for module #" << catom->blockId << " ("
         << rot.conFromP << " -> " << rot.conToP << ")";
    scheduler->trace(info.str(), rot.pivot->blockId, YELLOW);
    scheduler->schedule(
            new PivotActuationEndEvent(scheduler->now(), const_cast<Catoms3DBlock *>(rot.pivot),
                                       rot.mobile, rot.conFromP, rot.conToP));
}

const string Catoms3DRotationStopEvent::getEventName() {
    return ("Catoms3DRotationStop Event");
}

//===========================================================================================================
//
//          Catoms3DRotationEndEvent  (class)
//
//===========================================================================================================

Catoms3DRotationEndEvent::Catoms3DRotationEndEvent(Time t, Catoms3DBlock *block) : BlockEvent(t, block) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_ROTATION3D_END;
}

Catoms3DRotationEndEvent::Catoms3DRotationEndEvent(Catoms3DRotationEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
}

Catoms3DRotationEndEvent::~Catoms3DRotationEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void Catoms3DRotationEndEvent::consume() {
    EVENT_CONSUME_INFO();
    Catoms3DBlock *rb = (Catoms3DBlock *) concernedBlock;
    rb->setState(BuildingBlock::State::ALIVE);
    // cout << "[t-" << getScheduler()->now() << "] rotation ended" << endl;
    concernedBlock->blockCode->processLocalEvent(
            EventPtr(new Catoms3DRotationEndEvent(date + Catoms3DRotation::COM_DELAY, rb)));
    StatsCollector::getInstance().incMotionCount();
    StatsIndividual::incMotionCount(rb->stats);
}

const string Catoms3DRotationEndEvent::getEventName() {
    return ("Catoms3DRotationEnd Event");
}

//===========================================================================================================
//
//          Catoms3DRotation  (class)
//
//===========================================================================================================

void Catoms3DRotation::init(const Matrix &m) {
    firstRotation = true;
    step = 0;
    initialMatrix = m;
    finalMatrixCatom = m * finalMatrixLocal;

    exportMatrix(initialMatrix);
}


void Catoms3DRotation::exportMatrix(const Matrix &m) {
#define ROTATION_STEP_MATRIX_EXPORT
#ifdef ROTATION_STEP_MATRIX_EXPORT
    Catoms3DBlock *block = static_cast<Catoms3DBlock *>
    (BaseSimulator::getWorld()->getBlockById(catomId));

    if ((exportMatrixCount % 2) == 0 or exportMatrixCount == 40) {
        OUTPUT << getScheduler()->now() << "|";

        if (exportMatrixCount == 40) {
            short ori;
            getFinalPositionAndOrientation(block->blockCode->motionDest, ori);
        }

        block->blockCode->onBlockSelected();

        OUTPUT << catomId << "|";
        // OUTPUT << block->color << "|";
        OUTPUT << "(matrix3 "
               << "[" << m.m[0] << "," << m.m[4] << "," << m.m[8] << "] "
               << "[" << m.m[1] << "," << m.m[5] << "," << m.m[9] << "] "
               << "[" << m.m[2] << "," << m.m[6] << "," << m.m[10] << "] "
               << "[" << m.m[3] << "," << m.m[7] << "," << m.m[11] << "])"
               << endl;
    }

    exportMatrixCount++;
#endif
}

Catoms3DRotation::Catoms3DRotation(const Catoms3DBlock *mobile, const Catoms3DBlock *fixe, double rprim,
                                   const Vector3D &ax1, double ang1,
                                   const Vector3D &ax2, double ang2,
                                   short from, short to) : angle1(ang1), angle2(ang2) {
    this->mobile = mobile;
    catomId = mobile->blockId;
    pivot = fixe;

    static const double c_2 = 1.0 / (3 + sqrt(2));
    Matrix MA = ((Catoms3DGlBlock *) mobile->getGlBlock())->mat;
    Matrix MB = ((Catoms3DGlBlock *) fixe->getGlBlock())->mat;
    Matrix MA_1;

    // we calculate AB translation in A referentiel
    MA.inverse(MA_1);
    Matrix m = MA_1 * MB;
    Vector3D AB = m * Vector3D(0, 0, 0, 1);

    Matrix matTAB, matTBA;
    matTAB.setTranslation(AB);
    matTBA.setTranslation(-AB);

    axe1 = ax1.normer();

    double r = AB.norme() / 2.0;
    double shift = (ang1 > 0) ? c_2 * r : -c_2 * r;
    Vector3D V = AB ^ axe1;
    V.normer_interne();

    A0D0 = (0.5 + 0.5 * rprim) * AB + shift * V;
    A0C0 = (0.5 - 0.5 * rprim) * AB + shift * V;

    Matrix mr;
    mr.setRotation(angle1, axe1);
    finalMatrixLocal = matTAB * (mr * (matTBA * mr));

    m = MA * finalMatrixLocal;
    m.inverse(MA_1);
    m = MA_1 * MB;
    AB = m * Vector3D(0, 0, 0, 1);

    finalMatrixLocal.inverse(MA_1);
    axe2 = (MA_1 * ax2).normer();

    matTAB.setTranslation(AB);
    matTBA.setTranslation(-AB);
    mr.setRotation(angle2, axe2);
    m = matTAB * (mr * (matTBA * mr));
    finalMatrixLocal = finalMatrixLocal * m;

    m = MA * finalMatrixLocal;
    m.inverse(MA_1);
    m = MA_1 * MB;
    AB = m * Vector3D(0, 0, 0, 1);

    shift = (ang2 > 0) ? -c_2 * r : c_2 * r;
    V = AB ^ axe2;
    V.normer_interne();

    A1D1 = (0.5 + 0.5 * rprim) * AB + shift * V;
    A1C1 = (0.5 - 0.5 * rprim) * AB + shift * V;
}

/*
bool Catoms3DRotation::setMatrixAt(Time t) {
    if (t>2*ANIMATION_DELAY) return 0;
    if (t<ANIMATION_DELAY) {

    }
    return true;
}*/

bool Catoms3DRotation::nextStep(Matrix &m) {
    if (firstRotation) {
        step++;
        double angle = angle1 * step / Catoms3DRotation::nbRotationSteps;
        //OUTPUT << "step=" << step << "   angle=" << angle << endl;
        Matrix mr;
        mr.setRotation(angle, axe1);

        Matrix matTCA, matTDC, matTAD;
        matTCA.setTranslation(-A0C0);
        matTDC.setTranslation(-A0D0 + A0C0);
        matTAD.setTranslation(A0D0);
        m = matTAD * (mr * (matTDC * (mr * matTCA)));
        m = initialMatrix * m;


        //OUTPUT << m.m[0] << " " << m.m[1] << " " << m.m[2] << " " << m.m[3] << " " << m.m[4] << " " << m.m[5] << " " << m.m[6] << " " << m.m[7] << " " << m.m[8] << " " << m.m[9] << " " << m.m[10] << " " << m.m[11] << " " << m.m[12] << " " << m.m[13] << " " << m.m[14] << " " << m.m[15] << endl;

        if (step == Catoms3DRotation::nbRotationSteps)
            firstRotation = false;
    } else {
        step--;
        double angle = -angle2 * step / Catoms3DRotation::nbRotationSteps;
        // TRT-1R
        Matrix mr;
        mr.setRotation(angle, axe2);

        Matrix matTCA, matTDC, matTAD;
        matTCA.setTranslation(-A1C1);
        matTDC.setTranslation(-A1D1 + A1C1);
        matTAD.setTranslation(A1D1);
        m = matTAD * (mr * (matTDC * (mr * matTCA)));
        m = finalMatrixCatom * m;
        //OUTPUT << m.m[0] << " " << m.m[1] << " " << m.m[2] << " " << m.m[3] << " " << m.m[4] << " " << m.m[5] << " " << m.m[6] << " " << m.m[7] << " " << m.m[8] << " " << m.m[9] << " " << m.m[10] << " " << m.m[11] << " " << m.m[12] << " " << m.m[13] << " " << m.m[14] << " " << m.m[15] << endl;
    }
    exportMatrix(m);
    return step == 0;
}

void Catoms3DRotation::getFinalPositionAndOrientation(Cell3DPosition &position, short &orientation) {
    Vector3D p(0, 0, 0, 1), q = finalMatrixCatom * p;

//    OUTPUT << "final=" << q << endl;
    position = Catoms3D::getWorld()->lattice->worldToGridPosition(q);
//    OUTPUT << "final grid=" << position << endl;
    orientation = Catoms3DBlock::getOrientationFromMatrix(finalMatrixCatom);
}


//===========================================================================================================
//
//          PivotActuationStartEvent  (class)
//
//===========================================================================================================

PivotActuationStartEvent::PivotActuationStartEvent(Time t,
                                                   BuildingBlock *conBlock,
                                                   const BuildingBlock *m,
                                                   short from,
                                                   short to) : BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_PIVOT_ACTUATION_START;
    fromConP = from;
    toConP = to;
    mobile = m;
}

PivotActuationStartEvent::PivotActuationStartEvent(PivotActuationStartEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    fromConP = ev->fromConP;
    toConP = ev->toConP;
    mobile = ev->mobile;
}

PivotActuationStartEvent::~PivotActuationStartEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void PivotActuationStartEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new PivotActuationStartEvent(this)));
    concernedBlock->setState(BuildingBlock::State::ACTUATING);
}

const string PivotActuationStartEvent::getEventName() {
    return ("PivotActuationStart Event");
}

//===========================================================================================================
//
//          PivotActuationEndEvent  (class)
//
//===========================================================================================================

PivotActuationEndEvent::PivotActuationEndEvent(Time t,
                                               BuildingBlock *conBlock,
                                               const BuildingBlock *m,
                                               short from,
                                               short to) : BlockEvent(t, conBlock) {
    EVENT_CONSTRUCTOR_INFO();
    eventType = EVENT_PIVOT_ACTUATION_END;
    fromConP = from;
    toConP = to;
    mobile = m;
}

PivotActuationEndEvent::PivotActuationEndEvent(PivotActuationEndEvent *ev) : BlockEvent(ev) {
    EVENT_CONSTRUCTOR_INFO();
    fromConP = ev->fromConP;
    toConP = ev->toConP;
    mobile = ev->mobile;
}

PivotActuationEndEvent::~PivotActuationEndEvent() {
    EVENT_DESTRUCTOR_INFO();
}

void PivotActuationEndEvent::consumeBlockEvent() {
    EVENT_CONSUME_INFO();
    concernedBlock->scheduleLocalEvent(EventPtr(new PivotActuationEndEvent(this)));
    concernedBlock->setState(BuildingBlock::State::ALIVE);
}

const string PivotActuationEndEvent::getEventName() {
    return ("PivotActuationEnd Event");
}
