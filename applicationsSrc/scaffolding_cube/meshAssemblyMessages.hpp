/**
 * @file   messages.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 13:47:20 2018
 *
 * @brief
 *
 *
 */


#ifndef MC3D_MESSAGES_H_
#define MC3D_MESSAGES_H_

#include "network.h"

#include "meshRuleMatcher.hpp"

static const uint MSG_DELAY = 0;

using namespace MeshCoating;

class RoutableScaffoldMessage : public HandleableMessage {
protected:
    const Cell3DPosition srcPos;
    const Cell3DPosition dstPos;
public:
    RoutableScaffoldMessage(const Cell3DPosition& _srcPos,
                            const Cell3DPosition& _dstPos)
        : HandleableMessage(), srcPos(_srcPos), dstPos(_dstPos) {};
    virtual ~RoutableScaffoldMessage() {};

    /**
     * Passes the message to the next hop along the path from srcPos to dstPos
     */
    void route(BaseSimulator::BlockCode *bc);
};

class RequestTargetCellMessage : public RoutableScaffoldMessage {
    bID srcId;
public:
    RequestTargetCellMessage(const Cell3DPosition& _srcPos, const Cell3DPosition& _dstPos,
                             bID _srcId)
        : RoutableScaffoldMessage(_srcPos, _dstPos), srcId(_srcId) {};
    virtual ~RequestTargetCellMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new RequestTargetCellMessage(*this); }
    virtual string getName() const override { return "RequestTargetCell{" + srcPos.to_string()
            + ", " + to_string(srcId) + "}"; }
};

class ProvideTargetCellMessage : public RoutableScaffoldMessage {
    const Cell3DPosition tPos;
public:
    ProvideTargetCellMessage(const Cell3DPosition& _srcPos,
                             const Cell3DPosition& _dstPos,
                             const Cell3DPosition& _tPos)
        : RoutableScaffoldMessage(_srcPos, _dstPos), tPos(_tPos) {};
    virtual ~ProvideTargetCellMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new ProvideTargetCellMessage(*this); }
    virtual string getName() const override { return "ProvideTargetCell{" + tPos.to_string()
            + ", " + dstPos.to_string() + "}"; }
};

/**
 * Sent by a coordinator that has just arrived in order to ask waiting module to
 *  resend their RequestTargetCell
 */
class CoordinatorReadyMessage : public RoutableScaffoldMessage {
public:
    CoordinatorReadyMessage(const Cell3DPosition& _srcPos, const Cell3DPosition& _dstPos)
        : RoutableScaffoldMessage(_srcPos, _dstPos) {};

    virtual ~CoordinatorReadyMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new CoordinatorReadyMessage(*this); }
    virtual string getName() const override { return "CoordinatorReady{" + srcPos.to_string()
            + ", " + dstPos.to_string() + "}"; }
};


// Cannot use RoutableScaffoldMessage as is because it is used in a scenario where
//  routing cannot rely on TRs as they are not yet in place.
class TileInsertionReadyMessage : public HandleableMessage {
public:
    TileInsertionReadyMessage() : HandleableMessage() {};
    virtual ~TileInsertionReadyMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new TileInsertionReadyMessage(*this); }
    virtual string getName() const override { return "TileInsertionReady"; }
};

/////////////////////////////////////////////////////////////////
///////////////////// MOTION COORDINATION ///////////////////////
/////////////////////////////////////////////////////////////////

/**
 * This message should be routed through the line until it reaches the dstPos, which as a pivot
 *  module must check whether its light is on and either give a go, or wait until its status
 *  clears and send a go at that time.
 */
// class ProbePivotLightStateMessage : public RoutableScaffoldMessage {
//     const Cell3DPosition targetPos; // Next position the module is seeking to reach
//     /**
//      * Final component to be reached by a series of intermediate motions such as this one
//      */
//     const MeshComponent finalComponent;

// public:
//     ProbePivotLightStateMessage(const Cell3DPosition& _srcPos,
//                                 const Cell3DPosition& _dstPos,
//                                 const Cell3DPosition& _targetPos,
//                                 const MeshComponent _finalComponent)
//                                 // const Cell3DPosition& _finalPos)
//         : RoutableScaffoldMessage(_srcPos,_dstPos),
//           targetPos(_targetPos), finalComponent(_finalComponent) {};
//     virtual ~ProbePivotLightStateMessage() {};

//     virtual void handle(BaseSimulator::BlockCode*);
//     virtual Message* clone() const { return new ProbePivotLightStateMessage(*this); }
//     virtual string getName() const { return "ProbePivotLightState{" + srcPos.to_string()
//             + ", " + targetPos.to_string()
//             + ", " + MeshRuleMatcher::component_to_string(finalComponent)
//             + "}";
//     }
class ProbePivotLightStateMessage : public HandleableMessage {
    const Cell3DPosition srcPos;
    const Cell3DPosition targetPos; // Next position the module is seeking to reach
    /**
     * Final component to be reached by a series of intermediate motions such as this one
     */
    const MeshComponent finalComponent;

public:
    ProbePivotLightStateMessage(const Cell3DPosition& _srcPos,
                                const Cell3DPosition& _targetPos,
                                const MeshComponent _finalComponent)
                                // const Cell3DPosition& _finalPos)
        : HandleableMessage(), srcPos(_srcPos),
          targetPos(_targetPos), finalComponent(_finalComponent) {};
    virtual ~ProbePivotLightStateMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new ProbePivotLightStateMessage(*this); }
    virtual string getName() const override { return "ProbePivotLightState{" + srcPos.to_string()
            + ", " + targetPos.to_string()
            + ", " + MeshRuleMatcher::component_to_string(finalComponent)
            + "}";
    }

};

/**
 * This message is routed back from the pivot module to the module awaiting motion
 *  in order to notify it that it can be safely performed.
 */
class GreenLightIsOnMessage : public RoutableScaffoldMessage {
public:
    GreenLightIsOnMessage(const Cell3DPosition& _srcPos,
                          const Cell3DPosition& _dstPos)
        : RoutableScaffoldMessage(_srcPos, _dstPos) {};
    virtual ~GreenLightIsOnMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new GreenLightIsOnMessage(*this); }
    virtual string getName() const override { return "GreenLightIsOn{" + srcPos.to_string()
            + ", " + dstPos.to_string() + "}";
    }
};


/**
 * This message is sent to a RED light pivot when a module it actuated has
 *  reached its final rotation in the scaffold, instructing it to turn back green.
 */
class FinalTargetReachedMessage : public HandleableMessage {
    const Cell3DPosition finalPos;
public:
    FinalTargetReachedMessage(const Cell3DPosition& _finalPos)
        : HandleableMessage(), finalPos(_finalPos) {};
    virtual ~FinalTargetReachedMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override{ return new FinalTargetReachedMessage(*this); }
    virtual string getName() const override { return "FinalTargetReached{" + finalPos.to_string() +"}";
    }
};
#endif /* MC3D_MESSAGES_H_ */
