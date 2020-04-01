/**
 * @file teleportationEvents.h
 * @brief This file implements the realization of a teleportation event for C3D modules.
 *   Its purpose is to serve as a placeholder motion to test algorithm while proper
 *  Catoms3D movement primitives are being developed.
 *
 *  Created on: 13/11/2017
 *      Author: Pierre Thalamy
 */

#ifndef NODES2DMOTIONEVENTS_H_
#define NODES2DMOTIONEVENTS_H_

#include "base/buildingBlock.h"
#include "events/events.h"
#include "grid/lattice.h"

namespace BaseSimulator {

//===========================================================================================================
//
//          Nodes2DMotionStartEvent  (class)
//
//===========================================================================================================

    class Nodes2DMotionStartEvent : public BlockEvent {
        Cell3DPosition finalPosition;
        SLattice::Direction finalOrientation;
    public:
        Nodes2DMotionStartEvent(Time, BuildingBlock*, const Cell3DPosition &fpos, SLattice::Direction forient);
        Nodes2DMotionStartEvent(Nodes2DMotionStartEvent *ev);
        ~Nodes2DMotionStartEvent();
        void consumeBlockEvent() override {}
        void consume() override;
        const virtual string getEventName() override;
    };

//===========================================================================================================
//
//          Nodes2DMotionStopEvent  (class)
//
//===========================================================================================================

    class Nodes2DMotionStopEvent : public BlockEvent {
        Cell3DPosition finalPosition;
        SLattice::Direction finalOrientation;
    public:
        Nodes2DMotionStopEvent(Time, BuildingBlock *block,const Cell3DPosition &fpos, SLattice::Direction forient);
        ~Nodes2DMotionStopEvent();
        void consumeBlockEvent() override {}
        void consume() override;
        const virtual string getEventName() override;
    };

//===========================================================================================================
//
//          Nodes2DMotionEndEvent  (class)
//
//===========================================================================================================

    class Nodes2DMotionEndEvent : public BlockEvent {
    public:
        Nodes2DMotionEndEvent(Time, BuildingBlock *block);
        Nodes2DMotionEndEvent(Nodes2DMotionEndEvent *ev);
        ~Nodes2DMotionEndEvent();
        void consumeBlockEvent() override {}
        void consume() override;
        const virtual string getEventName() override;
    };

}
#endif /* NODES2DMOTIONEVENTS_H_ */
