/**
 * @file teleportationEvents.h
 * @brief This file implements the realization of a teleportation event for C3D modules.
 *   Its purpose is to serve as a placeholder motion to test algorithm while proper
 *  Catoms3D movement primitives are being developed.
 *
 *  Created on: 13/11/2017
 *      Author: Pierre Thalamy
 */

#ifndef HEXANODESMOTIONEVENTS_H_
#define HEXANODESMOTIONEVENTS_H_

#include "../../base/buildingBlock.h"
#include "../../events/events.h"
#include "../../grid/lattice.h"

namespace BaseSimulator {

//===========================================================================================================
//
//          HexanodesMotionStartEvent  (class)
//
//===========================================================================================================

    class HexanodesMotionStartEvent : public BlockEvent {
        Cell3DPosition finalPosition;
        HHLattice::Direction finalOrientation;
    public:
        HexanodesMotionStartEvent(Time, BuildingBlock*, const Cell3DPosition &fpos, HHLattice::Direction forient);
        HexanodesMotionStartEvent(HexanodesMotionStartEvent *ev);
        ~HexanodesMotionStartEvent();
        void consumeBlockEvent() override {}
        void consume() override;
        const virtual string getEventName() override;
    };

//===========================================================================================================
//
//          HexanodesMotionStopEvent  (class)
//
//===========================================================================================================

    class HexanodesMotionStopEvent : public BlockEvent {
        Cell3DPosition finalPosition;
        HHLattice::Direction finalOrientation;
    public:
        HexanodesMotionStopEvent(Time, BuildingBlock *block,const Cell3DPosition &fpos, HHLattice::Direction forient);
        ~HexanodesMotionStopEvent();
        void consumeBlockEvent() override {}
        void consume() override;
        const virtual string getEventName() override;
    };

//===========================================================================================================
//
//          HexanodesMotionEndEvent  (class)
//
//===========================================================================================================

    class HexanodesMotionEndEvent : public BlockEvent {
    public:
        HexanodesMotionEndEvent(Time, BuildingBlock *block);
        HexanodesMotionEndEvent(HexanodesMotionEndEvent *ev);
        ~HexanodesMotionEndEvent();
        void consumeBlockEvent() override {}
        void consume() override;
        const virtual string getEventName() override;
    };

}
#endif /* HEXANODESMOTIONEVENTS_H_ */
