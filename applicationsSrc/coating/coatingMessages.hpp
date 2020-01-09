/**
 * @file   coatingMessages.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Thu Jan  9 10:23:47 2020
 *
 * @brief
 *
 *
 */

#pragma once

#include "network.h"

static const uint MSG_DELAY = 0;

class SupportSegmentCompleteMessage : public HandleableMessage {
public:
    SupportSegmentCompleteMessage() {};
    virtual ~SupportSegmentCompleteMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new SupportSegmentCompleteMessage(*this);}
    virtual string getName() const override { return "SupportSegmentComplete"; }
};

/**
 * Message sent across a coating layer that has been precompleted by the insertion of
 *  supports and blocked positions.
 * Each module checks if the next module along the coating is in place and forwards the
 *  message to it if that's the case. Otherwise, it attracts it.
 */
class BorderCompletionMessage : public HandleableMessage {
public:
    BorderCompletionMessage() {};
    virtual ~BorderCompletionMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new BorderCompletionMessage(*this);}
    virtual string getName() const override { return "BorderCompletionMessage"; }
};

class NextPlaneSegmentDetectionMessage : public HandleableMessage {
    bool segmentDetected;
public:
    NextPlaneSegmentDetectionMessage(bool _det) : segmentDetected(_det) {};
    virtual ~NextPlaneSegmentDetectionMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override {
        return new NextPlaneSegmentDetectionMessage(*this);
    }
    virtual string getName() const override { return "NextPlaneSegmentDetectionMessage"; }
};
