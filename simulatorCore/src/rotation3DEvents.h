/*!
 * \file rotation3DEvents.h
 * \brief Motion events for 3D rotating blocks
 * \date 15/02/2015
 * \author Benoît Piranda, Pierre Thalamy
 *
 * For now, only Catoms3D implement this kind of movements, if other blocks are added, we may consider creating subclasses of BuildingBlocks depending on the type of motion they implement.
 *
 */

#ifndef ROTATION3DEVENTS_H_
#define ROTATION3DEVENTS_H_

#include "matrix44.h"
#include "catoms3DBlock.h"
#include "events.h"

const int nbRotationSteps = 20;

using namespace Catoms3D;

namespace Catoms3D {

class Rotations3D {    
public :
    static float rotationDelayMultiplier;
    
/**
   \brief Create a couple of rotations
   \param p : fixed pivot catom
   \param ax1 : rotation axe for the first rotation
   \param ang1 : rotation angle for the first rotation
   \param ax2 : rotation axe for the second rotation
   \param ang2 : rotation angle for the second rotation
*/
    Rotations3D(Catoms3DBlock *mobile,Catoms3DBlock *fixe,double rprim,const Vector3D &ax1,double ang1,const Vector3D &ax2,double ang2);
    Rotations3D() {};

    void init(const Matrix& m) {
        firstRotation=true;
        step=0;
        initialMatrix=m;
        finalMatrix=m*finalMatrix;
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
    void getFinalPositionAndOrientation(Cell3DPosition &position, short &orientation);
protected :
    bool firstRotation;
    short step;
    Matrix initialMatrix,finalMatrix;
    Vector3D A0C0,A0D0,A1C1,A1D1;
    Vector3D axe1;
    Vector3D axe2;
    double angle1;
    double angle2;
};
};

//===========================================================================================================
//
//          RotationExceptions
//
//===========================================================================================================
class NoRotationPathForFaceException : public VisibleSimException {
public:
    NoRotationPathForFaceException(const Cell3DPosition& mPos,
                                   const Cell3DPosition& pivotPos,
                                   const Cell3DPosition& tPos,
                                   RotationLinkType ft)
        {
            stringstream ss;
            ss << "Cannot find path for rotating module at " << mPos
               << " to " << tPos << " using ";
            switch (ft) {
                case HexaFace: ss << "hexa "; break;
                case OctaFace: ss << "octa "; break;
                case Any: ss << "any "; break;
                case None: ss << "NONE "; break;
                default: ss << "#INVALID "; break;
            }
            ss << "face of pivot at " << pivotPos << endl;
            m_msg = ss.str();
        }        
};

//===========================================================================================================
//
//          Rotation3DStartEvent  (class)
//
//===========================================================================================================

class Rotation3DStartEvent : public BlockEvent {
    Rotations3D rot;
public:
	Rotation3DStartEvent(Time, Catoms3DBlock *block,const Rotations3D& r);

    /** 
     * Attemps to create a single rotation that moves module m from its current position     
     *  to position tPos using module a pivot module.
     * @param t time
     * @param m mobile module
     * @param pivot pivot module
     * @param tPos target position that must be a neighbor position of pivot
     * @param ft link type to be used (Any by default)
     * @throw NoRotationPathForFaceException if no feasible rotations can be found.
     */
    Rotation3DStartEvent(Time t, Catoms3DBlock*m, Catoms3DBlock *pivot,
                         const Cell3DPosition& tPos,
                         RotationLinkType faceType = RotationLinkType::Any);

    /** 
     * Attemps to create a single rotation that moves module m on the surface of a pivot 
     *  module, from its current anchor connector to pivot connector conToId 
     * @param t time
     * @param m mobile module
     * @param pivot pivot module
     * @param conToId target connector on the surface of the pivot
     * @param ft link type to be used (Any by default)
     * @throw NoRotationPathForFaceException if no feasible rotations can be found.
     */
    Rotation3DStartEvent(Time t, Catoms3DBlock *m, Catoms3DBlock *pivot, short conToId,
                         RotationLinkType faceType = RotationLinkType::Any);
    
    Rotation3DStartEvent(Rotation3DStartEvent *ev);
    ~Rotation3DStartEvent();
    void consumeBlockEvent() {};
    void consume();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          Rotation3DStepEvent  (class)
//
//===========================================================================================================

    class Rotation3DStepEvent : public BlockEvent {
        Rotations3D rot;
    public:
        Rotation3DStepEvent(Time, Catoms3DBlock *block,const Rotations3D& r);
        Rotation3DStepEvent(Rotation3DStepEvent *ev);
        ~Rotation3DStepEvent();
        void consumeBlockEvent() {};
        void consume();
        const virtual string getEventName();
    };

//===========================================================================================================
//
//          Rotation3DStopEvent  (class)
//
//===========================================================================================================

    class Rotation3DStopEvent : public BlockEvent {
        Rotations3D rot;
    public:
        Rotation3DStopEvent(Time, Catoms3DBlock *block,const Rotations3D& r);
        Rotation3DStopEvent(Rotation3DStepEvent *ev);
        ~Rotation3DStopEvent();
        void consumeBlockEvent() {};
        void consume();
        const virtual string getEventName();
    };

//===========================================================================================================
//
//          Rotation3DEndEvent  (class)
//
//===========================================================================================================

    class Rotation3DEndEvent : public BlockEvent {
    public:
        Rotation3DEndEvent(Time, Catoms3DBlock *block);
        Rotation3DEndEvent(Rotation3DEndEvent *ev);
        ~Rotation3DEndEvent();
        void consumeBlockEvent() {};
        void consume();
        const virtual string getEventName();
    };
    
#endif /* ROTATIONS3DEVENTS_H_ */
