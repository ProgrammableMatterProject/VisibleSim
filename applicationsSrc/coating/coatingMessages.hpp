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
 * Used as a reply by a support that receives a SupportSegmentComplete message when it has
 *  not built a segment. This is used to signify that the SegmentCompletion ack must be
 *  propagated further along the border in order to reach the parent support
 */
class SegmentCompleteWrongSupport : public HandleableMessage {
public:
    SegmentCompleteWrongSupport() {};
    virtual ~SegmentCompleteWrongSupport() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new SegmentCompleteWrongSupport(*this);}
    virtual string getName() const override { return "SegmentCompleteWrongSupport"; }
};

/**
 * Message sent across a coating layer that has been precompleted by the insertion of
 *  supports and blocked positions.
 * Each module checks if the next module along the coating is in place and forwards the
 *  message to it if that's the case. Otherwise, it attracts it.
 */
class BorderCompletionMessage : public HandleableMessage {
    bool stopAtCorner;
public:
    BorderCompletionMessage() : stopAtCorner(false) {};
    BorderCompletionMessage(bool _stopAtCorner) : stopAtCorner(_stopAtCorner) {};
    virtual ~BorderCompletionMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override { return new BorderCompletionMessage(*this);}
    virtual string getName() const override { return "BorderCompletionMessage("
            + string(stopAtCorner ? "true" : "false") + ")";
    }
};

/**
 * This message is sent from a seed around the border of the object.
 *  It stops under each support module and waits until that support has arrived, and finished
 *  building it segments. Once the message gets back to the sender, it means that the modules
 *  are all initialized, and the seed can then decide on normal assembly or border completion
 */
class NextPlaneSupportsReadyMessage : public HandleableMessage {
    bool segmentsDetected;
public:
    NextPlaneSupportsReadyMessage(bool _det) : segmentsDetected(_det) {};
    virtual ~NextPlaneSupportsReadyMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;

    virtual Message* clone() const override {
        return new NextPlaneSupportsReadyMessage(*this);
    }

    virtual string getName() const override {
        return "NextPlaneSupportsReadyMessage("
            + string(segmentsDetected ? "true" : "false") + ")";
    }
};

class SupportReadyRequest : public HandleableMessage {
public:
    SupportReadyRequest() {};
    virtual ~SupportReadyRequest() {};

    virtual void handle(BaseSimulator::BlockCode*) override;

    virtual Message* clone() const override { return new SupportReadyRequest(*this); }
    virtual string getName() const override { return "SupportReadyRequest"; }
};

class SupportReadyResponse : public HandleableMessage {
    bool hasSegments;
public:
    SupportReadyResponse(bool _hasSegments) : hasSegments(_hasSegments) {};
    virtual ~SupportReadyResponse() {};

    virtual void handle(BaseSimulator::BlockCode*) override;

    virtual Message* clone() const override { return new SupportReadyResponse(*this); }
    virtual string getName() const override { return "SupportReadyResponse"; }
};
