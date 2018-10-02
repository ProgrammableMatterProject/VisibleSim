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

static const uint MSG_DELAY = 1;

using namespace MeshCoating;

class RequestTargetCellMessage : public HandleableMessage {
public:
    RequestTargetCellMessage()
        : HandleableMessage() {};
    virtual ~RequestTargetCellMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new RequestTargetCellMessage(*this); }
    virtual string getName() { return "RequestTargetCell"; }
};

class ProvideTargetCellMessage : public HandleableMessage {
    const Cell3DPosition tPos;
public:
    ProvideTargetCellMessage(const Cell3DPosition& _tPos)
        : HandleableMessage(), tPos(_tPos) {};
    virtual ~ProvideTargetCellMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new ProvideTargetCellMessage(*this); }
    virtual string getName() { return "ProvideTargetCell"; }
};

#endif /* MC3D_MESSAGES_H_ */
