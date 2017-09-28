/*
 *  neighborhood.h
 *
 *  Created on: 28 February 2017
 *  Author: Thadeu
 */

#ifndef NEIGHBOR_H_
#define NEIGHBOR_H_

#define CANFILLLEFT_MESSAGE_ID 12001
#define CANFILLLEFTRESPONSE_MESSAGE_ID 12002
#define CANFILLRIGHT_MESSAGE_ID 12003
#define CANFILLRIGHTRESPONSE_MESSAGE_ID 12004
#define ADDNEXTLINE_EVENT_ID 12005
#define ADDPREVIOUSLINE_EVENT_ID 12006

#include "cell3DPosition.h"
#include "directions.h"
#include "../reconf.h"
#include "../sync/syncNext.h"
#include "../sync/syncPrevious.h"

class Neighborhood {
private:
    Catoms3D::Catoms3DBlock *catom;
    Reconf *reconf;
    SyncNext *syncNext;
    SyncPrevious *syncPrevious;
    BlockCodeBuilder blockCodeBuilder;

    bool addNeighbor(Cell3DPosition pos);

public:
    static int numberBlockedModules;
    Neighborhood(Catoms3D::Catoms3DBlock *catom, Reconf *reconf, SyncNext *sn, SyncPrevious *sp, BlockCodeBuilder bcb);

    void addNeighborsWithoutSync();
    void addAllNeighbors();
    bool addFirstNeighbor();
    void addNeighborToLeft();
    void addNeighborToRight();
    void addNextLineNeighbor();
    void addPreviousLineNeighbor();

    void tryAddNeighborToLeft();
    void tryAddNeighborToRight();
    void tryAddNeighbors();
    void checkSyncAndTryAddNeighbors();

    bool isOnLeftBorder();
    bool isOnRightBorder();
    bool isFirstCatomOfLine();
    bool isFirstCatomOfPlane();

    void addNeighborToNextPlane();
    void addNeighborToPreviousPlane();

    void canFill();
    void addEventAddNextLineNeighbor();
    void addEventAddPreviousLineNeighbor();
    void sendMessageToAddLeft();
    void sendMessageToAddRight();
    void sendResponseMessageToAddLeft();
    void sendResponseMessageToAddRight();
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

    const string getEventName() { return "ADD NEXT LINE BLOCK EVENT"; }
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

    const string getEventName() { return "ADD PREVIOUS LINE BLOCK EVENT"; }
};

class CanFillLeft_message : public Message {
public:
    CanFillLeft_message() {
        this->id = CANFILLLEFT_MESSAGE_ID;
    }
};

class CanFillLeftResponse_message : public Message {
public:
    CanFillLeftResponse_message() {
        this->id = CANFILLLEFTRESPONSE_MESSAGE_ID;
    }
};

class CanFillRight_message : public Message {
public:
    CanFillRight_message() {
        this->id = CANFILLRIGHT_MESSAGE_ID;
    }
};

class CanFillRightResponse_message : public Message {
public:
    CanFillRightResponse_message() {
        this->id = CANFILLRIGHTRESPONSE_MESSAGE_ID;
    }
};

#endif /* NEIGHBORHOOD_H_ */
