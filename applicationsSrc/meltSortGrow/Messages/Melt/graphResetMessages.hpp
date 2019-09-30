/**
 * @file   GraphResetMessages.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Feb 13 09:44:31 2018
 * 
 * @brief  Messages and handlers for the graph reset step of the Melt phase
 */

#ifndef GRAPH_RESET_MESSAGES_H_
#define GRAPH_RESET_MESSAGES_H_

#include "../../meltSortGrowBlockCode.hpp"
#include "../meltSortGrowMessage.hpp"

class ResetGraphMessage : public MeltSortGrowMessage {
public:
    ResetGraphMessage() {
        type = MSG_MELT_RESET_GRAPH;
    };
    
    virtual ~ResetGraphMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new ResetGraphMessage(*this); }
    virtual string getName() { return "ResetGraph"; }
};

class ResetGraphDoneMessage : public MeltSortGrowMessage {
public:
    ResetGraphDoneMessage() {
        type = MSG_MELT_RESET_GRAPH_DONE;
    };
    
    virtual ~ResetGraphDoneMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new ResetGraphDoneMessage(*this); }
    virtual string getName() { return "ResetGraphDone"; }
};

class ResetGraphNACKMessage : public MeltSortGrowMessage {
public:
    ResetGraphNACKMessage() {
        type = MSG_MELT_RESET_GRAPH_NACK;
    };
    
    virtual ~ResetGraphNACKMessage() {};
    
    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new ResetGraphNACKMessage(*this); }
    virtual string getName() { return "ResetGraphNACK"; }
};
#endif /* GRAPH_RESET_MESSAGES_H_ */
