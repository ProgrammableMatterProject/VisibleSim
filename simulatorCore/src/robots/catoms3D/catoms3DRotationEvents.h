/*!
 * \file catoms3DRotationEvents.h
 * \brief Motion events for 3D rotating blocks
 * \date 15/02/2015
 * \author Benoît Piranda, Pierre Thalamy
 *
 * For now, only Catoms3D implement this kind of movements, if other blocks are added, we may consider creating subclasses of BuildingBlocks depending on the type of motion they implement.
 *
 */

#ifndef ROTATION3DEVENTS_H_
#define ROTATION3DEVENTS_H_

#include <random>
#include "../../math/matrix44.h"
#include "../../events/events.h"
#include "../../utils/global.h"
#include "catoms3DBlock.h"

using namespace Catoms3D;

namespace Catoms3D {

    class Catoms3DRotation {
        static std::mt19937 rng;
        static uniform_int_distribution<mt19937::result_type> randomAnimationDelay;
    public:
        static const int ANIMATION_DELAY;
        static const int COM_DELAY;
        static const int nbRotationSteps; //<! @attention MUST BE AN EVEN NUMBER!!!

        const Catoms3DBlock *mobile = NULL;
        const Catoms3DBlock *pivot = NULL;
        short conFromP, conToP;

#define RANDOM_ROTATION_TIME 0

        static Time getNextRotationEventDelay() {
            int rad;
#if RANDOM_ROTATION_TIME == 1
            rad = (int)randomAnimationDelay(rng);
#else
            rad = 0;
#endif
            // cout << rad << endl;

            return BaseSimulator::motionDelayMultiplier *
                   ((ANIMATION_DELAY + rad) / (2 * nbRotationSteps));
        }

/**
   \brief Create a couple of rotations
   \param p : fixed pivot catom
   \param ax1 : rotation axe for the first rotation
   \param ang1 : rotation angle for the first rotation
   \param ax2 : rotation axe for the second rotation
   \param ang2 : rotation angle for the second rotation
*/
        Catoms3DRotation(const Catoms3DBlock *mobile, const Catoms3DBlock *fixe, double rprim,
                         const Vector3D &ax1, double ang1,
                         const Vector3D &ax2, double ang2, short from = -1, short to = -1);

        Catoms3DRotation() {};

        void init(const Matrix &m);

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

        void exportMatrix(const Matrix &m);

        [[nodiscard]] inline Vector3D getAxe1() const { return axe1; };

        [[nodiscard]] inline Vector3D getAxe2() const { return axe2; };

        uint8_t getCode() { return (angle1 == 45 ? 4 : 3); }

    protected :
        short exportMatrixCount = 0;
        bool firstRotation;
        short step;
        Matrix initialMatrix, finalMatrixLocal, finalMatrixCatom;
        Vector3D A0C0, A0D0, A1C1, A1D1;
        Vector3D axe1;
        Vector3D axe2;
        double angle1;
        double angle2;
        bID catomId;

        friend std::ostream &operator<<(std::ostream &stream, Catoms3DRotation const &rots);
    };

    std::ostream &operator<<(std::ostream &stream, Catoms3DRotation const &rots);
}

//===========================================================================================================
//
//          RotationExceptions
//
//===========================================================================================================
class NoAvailableRotationPivotException : public VisibleSimException {
public:
    NoAvailableRotationPivotException(const Cell3DPosition &mPos) {
        stringstream ss;
        ss << "Cannot find pivot for rotating " << mPos << " to target position" << endl;
        m_msg = ss.str();
    }
};


class NoRotationPathForFaceException : public VisibleSimException {
public:
    NoRotationPathForFaceException(const Cell3DPosition &mPos,
                                   const Cell3DPosition &pivotPos,
                                   const Cell3DPosition &tPos,
                                   RotationLinkType ft) {
        stringstream ss;
        ss << "Cannot find path for rotating module at " << mPos
           << " to " << tPos << " using ";
        switch (ft) {
            case HexaFace:
                ss << "hexa ";
                break;
            case OctaFace:
                ss << "octa ";
                break;
            case Any:
                ss << "any ";
                break;
            case None:
                ss << "NONE ";
                break;
            default:
                ss << "#INVALID ";
                break;
        }
        ss << "face of pivot at " << pivotPos << endl;
        m_msg = ss.str();
    }
};

//===========================================================================================================
//
//          Catoms3DRotationStartEvent  (class)
//
//===========================================================================================================

class Catoms3DRotationStartEvent : public BlockEvent {
    Catoms3DRotation rot;
public:
    Catoms3DRotationStartEvent(Time t, Catoms3DBlock *block, const Catoms3DRotation &r);

    /**
     * Attemps to create a single rotation that moves module m on the surface of a pivot
     *  module, from its current anchor connector to pivot connector conToId
     * @param t time
     * @param m mobile module
     * @param pivot pivot module
     * @param conToId target connector on the surface of the pivot
     * @param faceReq link type to be used (Any by default)
     * @param exclusively in use only when faceReq has been provided. If set to true
     *  (by default), the motion will not be performed if it cannot be done using the requested
     *  face but could be done using another type.
     * @throw NoRotationPathForFaceException if no feasible rotations can be found.
     * @throw InvalidArgumentException if one of the arguments is invalid
     */
    Catoms3DRotationStartEvent(Time t, Catoms3DBlock *m, const Catoms3DBlock *pivot, short conToId,
                               RotationLinkType faceReq = RotationLinkType::Any,
                               bool exclusively = true);

    /**
     * Attemps to create a single rotation that moves module m from its current position
     *  to position tPos using module a pivot module.
     * @param t time
     * @param m mobile module
     * @param pivot pivot module
     * @param tPos target position that must be a neighbor position of pivot
     * @param ft link type to be used (Any by default)
     * @param exclusively in use only when faceReq has been provided. If set to true
     *  (by default), the motion will not be performed if it cannot be done using the requested
     *  face but could be done using another type.
     * @throw NoRotationPathForFaceException if no feasible rotations can be found.
     * @throw InvalidArgumentException if one of the arguments is invalid
     * @attention constructor chaining with Catoms3DRotationStartEvent(Time t, Catoms3DBlock*m, Catoms3DBlock *, short, RotationLinkType)
     */
    Catoms3DRotationStartEvent(Time t, Catoms3DBlock *m, const Catoms3DBlock *pivot,
                               const Cell3DPosition &tPos,
                               RotationLinkType faceReq = RotationLinkType::Any,
                               bool exclusively = true);

    /**
     * Attemps to create a single rotation that moves module m on the surface of _an autonomously selected_ pivot module, from its current anchor connector to neighbor position tPos
     * @param t time
     * @param m mobile module
     * @param tPos destination position
     * @param ft link type to be used (Any by default)
     * @param exclusively in use only when faceReq has been provided. If set to true
     *  (by default), the motion will not be performed if it cannot be done using the requested
     *  face but could be done using another type.
     * @throw NoRotationPathForFaceException if no feasible rotations can be found.
     * @throw InvalidArgumentException if one of the arguments is invalid
     * @attention constructor chaining with Catoms3DRotationStartEvent(Time t, Catoms3DBlock*m, Catoms3DBlock *, const Cell3DPosition&, RotationLinkType)
     */
    Catoms3DRotationStartEvent(Time t, Catoms3DBlock *m, const Cell3DPosition &tPos,
                               RotationLinkType faceReq = RotationLinkType::Any,
                               bool exclusively = true);

    Catoms3DRotationStartEvent(Catoms3DRotationStartEvent *ev);

    ~Catoms3DRotationStartEvent();

    void consumeBlockEvent() override {};

    void consume() override;

    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          Catoms3DRotationStepEvent  (class)
//
//===========================================================================================================

class Catoms3DRotationStepEvent : public BlockEvent {
    Catoms3DRotation rot;
public:
    Catoms3DRotationStepEvent(Time, Catoms3DBlock *block, const Catoms3DRotation &r);

    Catoms3DRotationStepEvent(Catoms3DRotationStepEvent *ev);

    ~Catoms3DRotationStepEvent();

    void consumeBlockEvent() override {};

    void consume() override;

    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          Catoms3DRotationStopEvent  (class)
//
//===========================================================================================================

class Catoms3DRotationStopEvent : public BlockEvent {
    Catoms3DRotation rot;
public:
    Catoms3DRotationStopEvent(Time, Catoms3DBlock *block, const Catoms3DRotation &r);

    Catoms3DRotationStopEvent(Catoms3DRotationStepEvent *ev);

    ~Catoms3DRotationStopEvent();

    void consumeBlockEvent() override {}

    void consume() override;

    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          Catoms3DRotationEndEvent  (class)
//
//===========================================================================================================

class Catoms3DRotationEndEvent : public BlockEvent {
public:
    Catoms3DRotationEndEvent(Time, Catoms3DBlock *block);

    Catoms3DRotationEndEvent(Catoms3DRotationEndEvent *ev);

    ~Catoms3DRotationEndEvent();

    void consumeBlockEvent() override {}

    void consume() override;

    const virtual string getEventName() override;
};


//===========================================================================================================
//
//          PivotActuationStartEvent  (class)
//
//===========================================================================================================

class PivotActuationStartEvent : public BlockEvent {
public:
    const BaseSimulator::BuildingBlock *mobile;
    short fromConP;
    short toConP;

    PivotActuationStartEvent(Time t, BuildingBlock *conBlock, const BuildingBlock *mobile,
                             short from, short to);

    PivotActuationStartEvent(PivotActuationStartEvent *ev);

    ~PivotActuationStartEvent();

    void consumeBlockEvent() override;

    const virtual string getEventName() override;
};

//===========================================================================================================
//
//          PivotActuationEndEvent  (class)
//
//===========================================================================================================

class PivotActuationEndEvent : public BlockEvent {
public:
    const BaseSimulator::BuildingBlock *mobile;
    short fromConP;
    short toConP;

    PivotActuationEndEvent(Time t, BuildingBlock *conBlock, const BuildingBlock *mobile,
                           short from, short to);

    PivotActuationEndEvent(PivotActuationEndEvent *ev);

    ~PivotActuationEndEvent();

    void consumeBlockEvent() override;

    const virtual string getEventName() override;
};


#endif /* ROTATIONS3DEVENTS_H_ */
