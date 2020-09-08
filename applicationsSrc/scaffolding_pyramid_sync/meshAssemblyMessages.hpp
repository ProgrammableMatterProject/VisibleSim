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

#include "comm/network.h"

#include "meshRuleMatcher.hpp"

static const uint MSG_DELAY = 0;

using namespace MeshCoating;

class RequestTargetCellMessage : public HandleableMessage {
    const Cell3DPosition srcPos;
public:
    RequestTargetCellMessage(const Cell3DPosition& _srcPos)
        : HandleableMessage(), srcPos(_srcPos) {};
    virtual ~RequestTargetCellMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override {
        adjustClonedMessageCount();
        return new RequestTargetCellMessage(*this);
    }
    virtual string getName() const override { return "RequestTargetCell"; }
};

class ProvideTargetCellMessage : public HandleableMessage {
    const Cell3DPosition tPos;
    const Cell3DPosition dstPos;
public:
    ProvideTargetCellMessage(const Cell3DPosition& _tPos, const Cell3DPosition& _dstPos)
        : HandleableMessage(), tPos(_tPos), dstPos(_dstPos) {};
    virtual ~ProvideTargetCellMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override {
        adjustClonedMessageCount();
        return new ProvideTargetCellMessage(*this);
    }
    virtual string getName() const override { return "ProvideTargetCell"; }
};

class TileInsertionReadyMessage : public HandleableMessage {
public:
    TileInsertionReadyMessage() : HandleableMessage() {};
    virtual ~TileInsertionReadyMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override {
        adjustClonedMessageCount();
        return new TileInsertionReadyMessage(*this);
    }
    virtual string getName() const override { return "TileInsertionReady"; }
};

class InitiateFeedingMechanismMessage : public HandleableMessage {
    std::array<bool, 7> requirements;
    unsigned int level;
public:
    InitiateFeedingMechanismMessage(const std::array<bool, 7>& _requirements,
                                    unsigned int _level)
        : HandleableMessage(), requirements(_requirements), level(_level) {};
    virtual ~InitiateFeedingMechanismMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) override;
    virtual Message* clone() const override {
        adjustClonedMessageCount();
        return new InitiateFeedingMechanismMessage(*this);
    }
    virtual string getName() const override { return "InitiateFeedingMechanism"; }
};


#endif /* MC3D_MESSAGES_H_ */
