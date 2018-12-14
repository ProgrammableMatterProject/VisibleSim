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

class RequestTargetCellMessage : public HandleableMessage {
    const Cell3DPosition srcPos;
public:
    RequestTargetCellMessage(const Cell3DPosition& _srcPos)
        : HandleableMessage(), srcPos(_srcPos) {};
    virtual ~RequestTargetCellMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() const { return new RequestTargetCellMessage(*this); }
    virtual string getName() const { return "RequestTargetCell"; }
};

class ProvideTargetCellMessage : public HandleableMessage {
    const Cell3DPosition tPos;
    const Cell3DPosition dstPos;
public:
    ProvideTargetCellMessage(const Cell3DPosition& _tPos, const Cell3DPosition& _dstPos)
        : HandleableMessage(), tPos(_tPos), dstPos(_dstPos) {};
    virtual ~ProvideTargetCellMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() const { return new ProvideTargetCellMessage(*this); }
    virtual string getName() const { return "ProvideTargetCell"; }
};

class TileNotReadyMessage : public HandleableMessage {
    const Cell3DPosition dstPos;
public:
    TileNotReadyMessage(const Cell3DPosition& _dstPos)
        : HandleableMessage(), dstPos(_dstPos) {};
    virtual ~TileNotReadyMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() const { return new TileNotReadyMessage(*this); }
    virtual string getName() const { return "TileNotReady"; }
};

class TileInsertionReadyMessage : public HandleableMessage {    
public:
    TileInsertionReadyMessage() : HandleableMessage() {};
    virtual ~TileInsertionReadyMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() const { return new TileInsertionReadyMessage(*this); }
    virtual string getName() const { return "TileInsertionReady"; }
};

/////////////////////////////////////////////////////////////////
///////////////////// MOTION COORDINATION ///////////////////////
/////////////////////////////////////////////////////////////////

class ProbeMotionValidityMessage : public HandleableMessage {
    const Cell3DPosition sender;
public:
    ProbeMotionValidityMessage(const Cell3DPosition& _sender)
        : HandleableMessage(), sender(_sender) {};
    virtual ~ProbeMotionValidityMessage() {};

    void forwardToFAOrReturnClearForMotion(BaseSimulator::BlockCode *bc) const;
    
    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() const { return new ProbeMotionValidityMessage(*this); }
    virtual string getName() const {
        return string("ProbeMotionValidityMessage(" + sender.config_print() + ")");
    }
};

class ClearForMotionMessage : public HandleableMessage {
    const Cell3DPosition sender; 
    const Cell3DPosition receiver; 
public:
    ClearForMotionMessage(const Cell3DPosition& _sender, const Cell3DPosition& _receiver)
        : HandleableMessage(), sender(_sender) {};
    virtual ~ClearForMotionMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() const { return new ClearForMotionMessage(*this); }
    virtual string getName() const {
        return string("ClearForMotionMessage(" + sender.config_print() + ','
                      + receiver.config_print() + ")");
    }
};

#endif /* MC3D_MESSAGES_H_ */
