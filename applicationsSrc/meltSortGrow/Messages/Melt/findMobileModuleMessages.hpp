/*
 * findMobileModuleMessages.hpp
 *
 *  Created on: 10/01/2018
 *      Author: pthalamy
 */

#ifndef FIND_MOBILE_MODULE_MESSAGES_H_
#define FIND_MOBILE_MODULE_MESSAGES_H_

#include "network.h"

#include "../../meltSortGrowBlockCode.hpp"
#include "../meltSortGrowMessage.hpp"
#include "../../pathHop.hpp"

class FindMobileModuleMessage : public MeltSortGrowMessage {
    list<PathHop> path; //!< Description of all the previous hop in the path
    // short parentConId; //!< Id of the connector of the sender of this message that is connected to the receiving module
public:
    FindMobileModuleMessage(list<PathHop> path);
    virtual ~FindMobileModuleMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindMobileModuleMessage(*this); }
    virtual string getName() { return "FindMobileModule"; }
};


class FindMobileModuleFoundMessage : public MeltSortGrowMessage {
    Catoms3DBlock *sender; //!<
public:
    FindMobileModuleFoundMessage() {};
    virtual ~FindMobileModuleFoundMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindMobileModuleFoundMessage(*this); }
    virtual string getName() { return "FindMobileModuleFound"; }
};

class FindMobileModuleIgnoreMessage : public MeltSortGrowMessage {
    Catoms3DBlock *sender; //!<
public:
    FindMobileModuleIgnoreMessage() {};
    virtual ~FindMobileModuleIgnoreMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindMobileModuleIgnoreMessage(*this); }
    virtual string getName() { return "FindMobileModuleIgnore"; }
};

class FindMobileModuleNotFoundMessage : public MeltSortGrowMessage {
    Catoms3DBlock *sender; //!<
public:
    FindMobileModuleNotFoundMessage() {};
    virtual ~FindMobileModuleNotFoundMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindMobileModuleNotFoundMessage(*this); }
    virtual string getName() { return "FindMobileModuleNotFound"; }
};

class FindMobileModuleBlockedMessage : public MeltSortGrowMessage {
    Catoms3DBlock *sender; //!<
public:
    FindMobileModuleBlockedMessage() {};
    virtual ~FindMobileModuleBlockedMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindMobileModuleBlockedMessage(*this); }
    virtual string getName() { return "FindMobileModuleBlocked"; }
};

#endif /* FIND_MOBILE_MODULE_MESSAGE_H_ */
