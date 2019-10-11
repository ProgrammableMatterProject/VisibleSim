/**
 * @file   coatingMessages.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Thu Oct 10 10:09:10 2019
 *
 * @brief
 *
 *
 */

#ifndef COATING_MESSAGES_H_
#define COATING_MESSAGES_H_

#include "network.h"

#include "coatingRuleMatcher.hpp"

static const uint MSG_DELAY = 0;

using namespace MeshCoating;

class CoaTrainRequest : public HandleableMessage {
public:
    CoaTrainRequest() : HandleableMessage() {};
    virtual ~CoaTrainRequest() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new CoaTrainRequest(*this); }
    virtual string getName() const override { return "CoaTrainRequest";
    }
};

class GetOnBoard : public HandleableMessage {
    int layer;
public:
    GetOnBoard(int _layer) : HandleableMessage(), layer(_layer) {};
    virtual ~GetOnBoard() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new GetOnBoard(*this); }
    virtual string getName() const override { return "GetOnBoard(" + to_string(layer) +")";}

    static inline const Cell3DPosition& defaultDst = Cell3DPosition(-1, -1, 1);
};

class CoaTrainIsFull : public HandleableMessage {
public:
    CoaTrainIsFull() : HandleableMessage() {};
    virtual ~CoaTrainIsFull() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new CoaTrainIsFull(*this); }
    virtual string getName() const override { return "CoaTrainIsFull";
    }
};

class ProceedToNextLayer : public HandleableMessage {
public:
    ProceedToNextLayer() : HandleableMessage() {};
    virtual ~ProceedToNextLayer() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new ProceedToNextLayer(*this); }
    virtual string getName() const override { return "ProceedToNextLayer";
    }
};

class ProbePivotLightStateMessage : public HandleableMessage {
    const Cell3DPosition srcPos;
    const Cell3DPosition targetPos; // Next position the module is seeking to reach
    /**
     * Final component to be reached by a series of intermediate motions such as this one
     */
    const ScafComponent finalComponent;

public:
    ProbePivotLightStateMessage(const Cell3DPosition& _srcPos,
                                const Cell3DPosition& _targetPos,
                                const ScafComponent _finalComponent)
                                // const Cell3DPosition& _finalPos)
        : HandleableMessage(), srcPos(_srcPos),
          targetPos(_targetPos), finalComponent(_finalComponent) {};
    virtual ~ProbePivotLightStateMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new ProbePivotLightStateMessage(*this); }
    virtual string getName() const override { return "ProbePivotLightState{" + srcPos.to_string()
            + ", " + targetPos.to_string()
            + ", " + CoatingRuleMatcher::component_to_string(finalComponent)
            + "}";
    }

};

/**
 * This message is routed back from the pivot module to the module awaiting motion
 *  in order to notify it that it can be safely performed.
 */
class GreenLightIsOnMessage : public HandleableMessage {
    const Cell3DPosition srcPos;
    const Cell3DPosition dstPos;
public:
    GreenLightIsOnMessage(const Cell3DPosition& _srcPos,
                          const Cell3DPosition& _dstPos)
        : srcPos(_srcPos), dstPos(_dstPos) {};
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

#endif /* COATING_MESSAGES_H_ */
