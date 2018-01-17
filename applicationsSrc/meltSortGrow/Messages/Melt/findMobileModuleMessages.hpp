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

class FindMobileModuleMessage : public MeltSortGrowMessage {
    Catoms3DBlock *sender; //!<
    short senderOriCode; //!< id of the connector to which the message sender is connected
    set<short> pathCons; //!< set of path connectors of the parent
public:
    FindMobileModuleMessage(short _senderOriCode,
                            set<short>pathCons);
    virtual ~FindMobileModuleMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
};


class FindMobileModuleFoundMessage : public MeltSortGrowMessage {
    Catoms3DBlock *sender; //!<
public:
    FindMobileModuleFoundMessage() {};
    virtual ~FindMobileModuleFoundMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
};

class FindMobileModuleIgnoreMessage : public MeltSortGrowMessage {
    Catoms3DBlock *sender; //!<
public:
    FindMobileModuleIgnoreMessage() {};
    virtual ~FindMobileModuleIgnoreMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
};

class FindMobileModuleNotFoundMessage : public MeltSortGrowMessage {
    Catoms3DBlock *sender; //!<
public:
    FindMobileModuleNotFoundMessage() {};
    virtual ~FindMobileModuleNotFoundMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
};

#endif /* FIND_MOBILE_MODULE_MESSAGE_H_ */
