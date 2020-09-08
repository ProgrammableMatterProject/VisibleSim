/**
 * @file   lightWalkMessages.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Mon Dec 10 15:35:02 2018
 *
 * @brief
 *
 *
 */

#ifndef LIGHTWALKCATOMS3D_MESSAGES_H_
#define LIGHTWALKCATOMS3D_MESSAGES_H_

#include "comm/network.h"

static const uint MSG_DELAY_MC = 5000;

/**
 * This message should be routed through the line until it reaches the dstPos, which as a pivot
 *  module must check whether its light is on and either give a go, or wait until its status
 *  clears and send a go at that time.
 */
class ProbePivotLightStateMessage : public HandleableMessage {
    const Cell3DPosition srcPos;
    const Cell3DPosition targetPos;
public:
    ProbePivotLightStateMessage(const Cell3DPosition& _srcPos,
                                const Cell3DPosition& _targetPos)
        : HandleableMessage(), srcPos(_srcPos), targetPos(_targetPos) {};
    virtual ~ProbePivotLightStateMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override {

        adjustClonedMessageCount();
        adjustClonedMessageCount()
            ;
        return new ProbePivotLightStateMessage(*this);
    }
    virtual string getName() const override { return "ProbePivotLightState{" + srcPos.to_string()
            + ", " + targetPos.to_string() + "}";
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
        : HandleableMessage(), srcPos(_srcPos), dstPos(_dstPos) {};
    virtual ~GreenLightIsOnMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override {
        adjustClonedMessageCount();
        return new GreenLightIsOnMessage(*this);
    }
    virtual string getName() const override { return "GreenLightIsOn{" + srcPos.to_string()
            + ", " + dstPos.to_string() + "}";
    }
};

#endif /* LIGHTWALKCATOMS3D_MESSAGES_H_ */
