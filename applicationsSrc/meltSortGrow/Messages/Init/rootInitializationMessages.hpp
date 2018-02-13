/*
 * rootInitializationMessages.hpp
 *
 *  Created on: 12/02/2018
 *      Author: pthalamy
 */

#ifndef ROOT_INITIALIZATION_MESSAGES_H_
#define ROOT_INITIALIZATION_MESSAGES_H_

#include "network.h"

#include "../../meltSortGrowBlockCode.hpp"
#include "../meltSortGrowMessage.hpp"
#include "../../pathHop.hpp"

class RootUpdateMessage : public MeltSortGrowMessage {
    Cell3DPosition candidateRootPosition;
public:
    RootUpdateMessage(Cell3DPosition pos) : candidateRootPosition(pos) {
        type = MSG_ROOT_UPDATE;
    };
    // RootUpdateMessage(RootUpdateMessage& msg) :
    //     candidateRootPosition(msg->candidateRootPosition) {};
    
    virtual ~RootUpdateMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new RootUpdateMessage(*this); }
    virtual string getName() { return "RootUpdate"; }
};


class RootConfirmationMessage : public MeltSortGrowMessage {
    Cell3DPosition confirmationRootPosition;
public:
    RootConfirmationMessage(Cell3DPosition pos) : confirmationRootPosition(pos) {
        type = MSG_ROOT_CONFIRM;
    };
    virtual ~RootConfirmationMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new RootConfirmationMessage(*this); }
    virtual string getName() { return "RootConfirmation"; }
};


class RootRefusalMessage : public MeltSortGrowMessage {
    Cell3DPosition refusedRootPosition;
public:
    RootRefusalMessage(Cell3DPosition pos) : refusedRootPosition(pos) {
        type = MSG_ROOT_NCONFIRM;
    };
    virtual ~RootRefusalMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new RootRefusalMessage(*this); }
    virtual string getName() { return "RootRefusal"; }
};

#endif /* ROOT_INITIALIZATION_MESSAGE_H_ */
