/**
 * @file   messages.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 13:47:20 2018
 *
 * @brief
 *
 *
 */


#ifndef C3DR_MESSAGES_H_
#define C3DR_MESSAGES_H_

#include "network.h"

class MoveNextModuleMessage : public HandleableMessage {
    const Cell3DPosition nextBlockPos;
public:
    MoveNextModuleMessage(const Cell3DPosition& _nextBlockPos)
        : HandleableMessage(), nextBlockPos(_nextBlockPos) {};
    virtual ~MoveNextModuleMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() const { return new MoveNextModuleMessage(*this); }
    virtual string getName() const { return "MoveNextModule"; }
};

#endif /* C3DR_MESSAGES_H_ */
