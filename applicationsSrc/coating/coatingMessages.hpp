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
