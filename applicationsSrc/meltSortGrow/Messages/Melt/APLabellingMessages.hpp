/*
 * APLabellingMessages.hpp
 *
 *  Created on: 12/02/2018
 *      Author: pthalamy
 */

#ifndef APLABELLING_MESSAGES_H_
#define APLABELLING_MESSAGES_H_

#include "network.h"

#include "../../meltSortGrowBlockCode.hpp"
#include "../meltSortGrowMessage.hpp"
#include "../../pathHop.hpp"

class APLabellingTokenMessage : public MeltSortGrowMessage {
    int fatherDfnCnt;
public:
    APLabellingTokenMessage(int _fatherDfnCnt) : fatherDfnCnt(_fatherDfnCnt) {
        type = MSG_MELT_APL_TOKEN;
    };
    
    virtual ~APLabellingTokenMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new APLabellingTokenMessage(*this); }
    virtual string getName() { return "APLabellingToken"; }
};


class APLabellingEchoMessage : public MeltSortGrowMessage {
    int lDfn;
    int dfnCnt;
public:
    APLabellingEchoMessage(int _lDfn, int _dfnCnt) : lDfn(_lDfn), dfnCnt(_dfnCnt) {
        type = MSG_MELT_APL_ECHO;
    };
    
    virtual ~APLabellingEchoMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new APLabellingEchoMessage(*this); }
    virtual string getName() { return "APLabellingEcho"; }
};

class APLabellingVisitedMessage : public MeltSortGrowMessage {
    int receivedDfn;
public:
    APLabellingVisitedMessage(int dfn) : receivedDfn(dfn) {
        type = MSG_MELT_APL_VISITED;
    };
    
    virtual ~APLabellingVisitedMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new APLabellingVisitedMessage(*this); }
    virtual string getName() { return "APLabellingVisited"; }
};


#endif /* APLABELLING_MESSAGE_H_ */
