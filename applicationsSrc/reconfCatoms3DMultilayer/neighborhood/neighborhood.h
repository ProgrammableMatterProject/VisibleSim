/*
 *  neighborhood.h
 *
 *  Created on: 28 February 2017
 *  Author: Thadeu
 */

#ifndef NEIGHBOR_H_
#define NEIGHBOR_H_

#define CANFILLLEFTRESPONSE_MESSAGE_ID 12002
#define CANFILLRIGHTRESPONSE_MESSAGE_ID 12003
#define CANFILLNEXTFLOOR_MESSAGE_ID 12004

#define ADDNEXTLINE_EVENT_ID 12005
#define ADDPREVIOUSLINE_EVENT_ID 12006
#define ADDLEFTBLOCK_EVENT_ID 12007
#define ADDRIGHTBLOCK_EVENT_ID 12008
#define ADDNEXTPLANE_EVENT_ID 12009
#define ADDPREVIOUSPLANE_EVENT_ID 12010

#define NEXTPLANECONFIRMATION_NORTHLEFT_MESSAGE_ID 12100
#define NEXTPLANECONFIRMATION_NORTHRIGHT_MESSAGE_ID 12101
#define NEXTPLANECONFIRMATION_WESTLEFT_MESSAGE_ID 12102
#define NEXTPLANECONFIRMATION_WESTRIGHT_MESSAGE_ID 12103
#define NEXTPLANECONFIRMATION_SOUTHLEFT_MESSAGE_ID 12104
#define NEXTPLANECONFIRMATION_SOUTHRIGHT_MESSAGE_ID 12105
#define NEXTPLANECONFIRMATION_EASTLEFT_MESSAGE_ID 12106
#define NEXTPLANECONFIRMATION_EASTRIGHT_MESSAGE_ID 12107

#include "cell3DPosition.h"
#include "directions.h"
#include "../reconf.h"
#include "../sync/syncNext.h"
#include "../sync/syncPrevious.h"

class Neighborhood {
private:
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;
    BlockCodeBuilder blockCodeBuilder;

    SyncNext *syncNext;
    SyncPrevious *syncPrevious;

public:
    static int numberBlockedModules;
    static int numberMessagesToAddBlock;
    Neighborhood(Catoms3D::Catoms3DBlock *catom, Reconf *reconf, SyncNext *sn, SyncPrevious *sp, BlockCodeBuilder bcb);

    void addAllNeighbors();
    bool addFirstNeighbor();
    bool addNeighbor(Cell3DPosition pos);

    void addNeighborToLeft();
    void addNeighborToRight();
    void addNextLineNeighbor();
    void addPreviousLineNeighbor();
    void addNeighborToNextPlane();
    void addNeighborToPreviousPlane();

    bool isFirstCatomOfLine();
    bool isFirstCatomOfPlane();

    void sendResponseMessageToAddLeft();
    void sendResponseMessageToAddRight();

    void addNeighbors();
    void addNext();
    void addPrevious();
    void addLeft();
    void addRight();
    void addNextPlane();


    void checkDependencies();
    void sendMessageToNextPlaneNorthLeft();
    void sendMessageToNextPlaneNorthRight();
    void sendMessageToNextPlaneWestLeft();
    void sendMessageToNextPlaneWestRight();
    void sendMessageToNextPlaneSouthLeft();
    void sendMessageToNextPlaneSouthRight();
    void sendMessageToNextPlaneEastLeft();
    void sendMessageToNextPlaneEastRight();

    void sendMessageCanFillNextFloor();
};

class AddNextLine_event : public BlockEvent {
public:
    AddNextLine_event(Time t, BaseSimulator::BuildingBlock *conBlock) : BlockEvent(t, conBlock) {
        eventType = ADDNEXTLINE_EVENT_ID;
    }
    AddNextLine_event(AddNextLine_event *conBlock) : BlockEvent(conBlock) {
    }

    void consumeBlockEvent() {
        concernedBlock->scheduleLocalEvent(EventPtr(new AddNextLine_event(this)));
    }

    const string getEventName() { return "Add next line block event"; }
};

class AddPreviousLine_event : public BlockEvent {
public:
    AddPreviousLine_event(Time t, BaseSimulator::BuildingBlock *conBlock) : BlockEvent(t, conBlock) {
        eventType = ADDPREVIOUSLINE_EVENT_ID;
    }
    AddPreviousLine_event(AddPreviousLine_event *conBlock) : BlockEvent(conBlock) {
    }

    void consumeBlockEvent() {
        concernedBlock->scheduleLocalEvent(EventPtr(new AddPreviousLine_event(this)));
    }

    const string getEventName() { return "Add previous line block event"; }
};

class AddLeftBlock_event : public BlockEvent {
public:
    AddLeftBlock_event(Time t, BaseSimulator::BuildingBlock *conBlock) : BlockEvent(t, conBlock) {
        eventType = ADDLEFTBLOCK_EVENT_ID;
    }
    AddLeftBlock_event(AddLeftBlock_event *conBlock) : BlockEvent(conBlock) {
    }

    void consumeBlockEvent() {
        concernedBlock->scheduleLocalEvent(EventPtr(new AddLeftBlock_event(this)));
    }

    const string getEventName() { return "Add left block event"; }
};

class AddRightBlock_event : public BlockEvent {
public:
    AddRightBlock_event(Time t, BaseSimulator::BuildingBlock *conBlock) : BlockEvent(t, conBlock) {
        eventType = ADDRIGHTBLOCK_EVENT_ID;
    }
    AddRightBlock_event(AddRightBlock_event *conBlock) : BlockEvent(conBlock) {
    }

    void consumeBlockEvent() {
        concernedBlock->scheduleLocalEvent(EventPtr(new AddRightBlock_event(this)));
    }

    const string getEventName() { return "Add right block event"; }
};

class AddNextPlane_event : public BlockEvent {
public:
    AddNextPlane_event(Time t, BaseSimulator::BuildingBlock *conBlock) : BlockEvent(t, conBlock) {
        eventType = ADDNEXTPLANE_EVENT_ID;
    }
    AddNextPlane_event(AddNextPlane_event *conBlock) : BlockEvent(conBlock) {
    }

    void consumeBlockEvent() {
        concernedBlock->scheduleLocalEvent(EventPtr(new AddNextPlane_event(this)));
    }

    const string getEventName() { return "Add next plane block event"; }
};

class AddPreviousPlane_event : public BlockEvent {
public:
    AddPreviousPlane_event(Time t, BaseSimulator::BuildingBlock *conBlock) : BlockEvent(t, conBlock) {
        eventType = ADDPREVIOUSPLANE_EVENT_ID;
    }
    AddPreviousPlane_event(AddPreviousPlane_event *conBlock) : BlockEvent(conBlock) {
    }

    void consumeBlockEvent() {
        concernedBlock->scheduleLocalEvent(EventPtr(new AddPreviousPlane_event(this)));
    }

    const string getEventName() { return "Add previous plane block event"; }
};

class CanFillLeftResponse_message : public Message {
public:
    CanFillLeftResponse_message() {
        this->id = CANFILLLEFTRESPONSE_MESSAGE_ID;
    }
};

class CanFillRightResponse_message : public Message {
public:
    CanFillRightResponse_message() {
        this->id = CANFILLRIGHTRESPONSE_MESSAGE_ID;
    }
};

class CanFillNextFloor_message : public Message {
public:
    CanFillNextFloor_message() {
        this->id = CANFILLNEXTFLOOR_MESSAGE_ID;
    }
};

class NextPlaneConfirmationNorthLeft_message : public Message {
public:
    NextPlaneConfirmationNorthLeft_message() {
        this->id = NEXTPLANECONFIRMATION_NORTHLEFT_MESSAGE_ID;
    }
};

class NextPlaneConfirmationNorthRight_message : public Message {
public:
    NextPlaneConfirmationNorthRight_message() {
        this->id = NEXTPLANECONFIRMATION_NORTHRIGHT_MESSAGE_ID;
    }
};

class NextPlaneConfirmationWestLeft_message : public Message {
public:
    NextPlaneConfirmationWestLeft_message() {
        this->id = NEXTPLANECONFIRMATION_WESTLEFT_MESSAGE_ID;
    }
};

class NextPlaneConfirmationWestRight_message : public Message {
public:
    NextPlaneConfirmationWestRight_message() {
        this->id = NEXTPLANECONFIRMATION_WESTRIGHT_MESSAGE_ID;
    }
};

class NextPlaneConfirmationSouthLeft_message : public Message {
public:
    NextPlaneConfirmationSouthLeft_message() {
        this->id = NEXTPLANECONFIRMATION_SOUTHLEFT_MESSAGE_ID;
    }
};

class NextPlaneConfirmationSouthRight_message : public Message {
public:
    NextPlaneConfirmationSouthRight_message() {
        this->id = NEXTPLANECONFIRMATION_SOUTHRIGHT_MESSAGE_ID;
    }
};

class NextPlaneConfirmationEastLeft_message : public Message {
public:
    NextPlaneConfirmationEastLeft_message() {
        this->id = NEXTPLANECONFIRMATION_EASTLEFT_MESSAGE_ID;
    }
};

class NextPlaneConfirmationEastRight_message : public Message {
public:
    NextPlaneConfirmationEastRight_message() {
        this->id = NEXTPLANECONFIRMATION_EASTRIGHT_MESSAGE_ID;
    }
};
#endif /* NEIGHBORHOOD_H_ */
