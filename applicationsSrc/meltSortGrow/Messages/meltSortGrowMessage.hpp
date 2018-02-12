/*
 * MeltSortGrowMessage.hpp
 *
 *  Created on: 16/01/2018
 *      Author: pthalamy
 */

#ifndef MELT_SORT_GROW_MESSAGE_H_
#define MELT_SORT_GROW_MESSAGE_H_

#include "network.h"
#include "../meltSortGrowBlockCode.hpp"

class MeltSortGrowMessage : public HandleableMessage {
public:
    MeltSortGrowMessage() {};
    virtual ~MeltSortGrowMessage() {};

    virtual void handle(BaseSimulator::BlockCode*) = 0;
};

#endif /* FIND_MOBILE_MODULE_MESSAGE_H_ */
