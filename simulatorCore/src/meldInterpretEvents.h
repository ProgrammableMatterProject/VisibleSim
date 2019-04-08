#ifndef MELDINTERPEVENTS_H_
#define MELDINTERPEVENTS_H_

#include "buildingBlock.h"
#include "meldInterpretVM.h"
#include "events.h"
#include "network.h"

//We'll see later for a debugger
//#include "meldProcessDebugger.h"

namespace MeldInterpret {

//===========================================================================================================
//
//          VMSetIdEvent  (class)
//
//===========================================================================================================

class VMSetIdEvent : public BlockEvent {
public:

    VMSetIdEvent(Time, BaseSimulator::BuildingBlock *conBlock);
    VMSetIdEvent(VMSetIdEvent *ev);
    ~VMSetIdEvent();
    void consumeBlockEvent() {};
    void consume();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          VMStopEvent  (class)
//
//===========================================================================================================

class VMStopEvent : public BlockEvent {
public:

    VMStopEvent(Time, BaseSimulator::BuildingBlock *conBlock);
    VMStopEvent(VMStopEvent *ev);
    ~VMStopEvent();
    void consumeBlockEvent() {};
    void consume();
    const virtual string getEventName();
};


//===========================================================================================================
//
//          VMSendMessageEvent  (class)
//
//===========================================================================================================

class VMSendMessageEvent : public BlockEvent {
public:
    MessagePtr message;
    P2PNetworkInterface *sourceInterface;

    VMSendMessageEvent(Time, BaseSimulator::BuildingBlock *conBlock, MessagePtr mes, P2PNetworkInterface *ni);
    VMSendMessageEvent(VMSendMessageEvent *ev);
    ~VMSendMessageEvent();
    void consumeBlockEvent();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          VMSendMessageEvent2  (class)
//
//===========================================================================================================
// PTHY: TEMPORARY: FOR WIRELESS
class VMSendMessageEvent2 : public BlockEvent {
public:
    MessagePtr message;
    BaseSimulator::BuildingBlock* target;

    VMSendMessageEvent2(Time, BaseSimulator::BuildingBlock *conBlock,
                        MessagePtr mes, BaseSimulator::BuildingBlock* sento);
    VMSendMessageEvent2(VMSendMessageEvent2 *ev);
    ~VMSendMessageEvent2();
    void consumeBlockEvent();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          VMReceiveMessageEvent2  (class)
//
//===========================================================================================================
// PTHY: TEMPORARY: FOR WIRELESS
class VMReceiveMessageEvent2 : public BlockEvent {
public:
    MessagePtr message;
    VMReceiveMessageEvent2(Time, BaseSimulator::BuildingBlock *conBlock, MessagePtr mes);
    VMReceiveMessageEvent2(VMReceiveMessageEvent2* ev);
    ~VMReceiveMessageEvent2();
    void consumeBlockEvent();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          VMHandleDebugCommandEvent  (class)
//
//===========================================================================================================

class VMHandleDebugCommandEvent : public BlockEvent {

public:

    VMHandleDebugCommandEvent(Time, BaseSimulator::BuildingBlock *conBlock);
    VMHandleDebugCommandEvent(VMHandleDebugCommandEvent *ev);
    ~VMHandleDebugCommandEvent();
    void consumeBlockEvent();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          VMDebugMessageEvent  (class)
//
//===========================================================================================================

class VMDebugPauseSimEvent : public Event {

public:

    VMDebugPauseSimEvent(Time);
    VMDebugPauseSimEvent(VMDebugPauseSimEvent *ev);
    ~VMDebugPauseSimEvent();
    void consume();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          VMEndPollEvent  (class)
//
//===========================================================================================================

class VMEndPollEvent : public BlockEvent {
public:

    VMEndPollEvent(Time, BaseSimulator::BuildingBlock *conBlock);
    VMEndPollEvent(VMEndPollEvent *ev);
    ~VMEndPollEvent();
    void consumeBlockEvent();
    const virtual string getEventName();
};


//===========================================================================================================
//
//          ComputePredicateEvent  (class)
//
//===========================================================================================================

class ComputePredicateEvent : public BlockEvent {
public:
    //No idea for members for the moment

    ComputePredicateEvent(Time, BaseSimulator::BuildingBlock *conBlock);
    ComputePredicateEvent(ComputePredicateEvent *ev);
    ~ComputePredicateEvent();
    void consumeBlockEvent();
    const virtual string getEventName();
};

//===========================================================================================================
//
//          AddTupleEvent  (class)
//
//===========================================================================================================

class AddTupleEvent : public BlockEvent {
public:
    tuple_t tuple;
    meld_byte face;

    AddTupleEvent(Time, BaseSimulator::BuildingBlock *conBlock, tuple_t tpl, meld_byte f);
    AddTupleEvent(AddTupleEvent *ev);
    ~AddTupleEvent();
    void consumeBlockEvent();
    const virtual string getEventName();
};


//===========================================================================================================
//
//          RemoveTupleEvent  (class)
//
//===========================================================================================================

class RemoveTupleEvent : public BlockEvent {
public:
    tuple_t tuple;
    meld_byte face;

    RemoveTupleEvent(Time, BaseSimulator::BuildingBlock *conBlock, tuple_t tpl, meld_byte f);
    RemoveTupleEvent(RemoveTupleEvent *ev);
    ~RemoveTupleEvent();
    void consumeBlockEvent();
    const virtual string getEventName();
};


} // MeldInterpret namespace


#endif /* MELDINTERPEVENTS_H_ */
