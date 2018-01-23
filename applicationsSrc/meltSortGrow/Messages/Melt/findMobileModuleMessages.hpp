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
    Catoms3DBlock *sender; //!< A pointer to the sender catom \attention {(PTHA: this should not be necessary as it clearly violates distributed code)}
    list<PathHop> path; //!< Description of all the previous hop in the path
    // short parentConId; //!< Id of the connector of the sender of this message that is connected to the receiving module
public:
    FindMobileModuleMessage(list<PathHop> path);
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
