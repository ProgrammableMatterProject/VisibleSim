/**
 * @file teleportationEvents.h
 * @brief This file implements the realization of a teleportation event for C3D modules.
 *   Its purpose is to serve as a placeholder motion to test algorithm while proper
 *  Catoms3D movement primitives are being developed.
 *
 *  Created on: 13/11/2017
 *      Author: Pierre Thalamy
 */

#ifndef HEXANODEMOTIONEVENTS_H_
#define HEXANODEMOTIONEVENTS_H_

#include "buildingBlock.h"
#include "events.h"
#include "lattice.h"

namespace BaseSimulator {

//===========================================================================================================
//
//          HexanodeMotionStartEvent  (class)
//
//===========================================================================================================

	class HexanodeMotionStartEvent : public BlockEvent {
		Cell3DPosition finalPosition;
		HHLattice::Direction finalOrientation;
	public:
		HexanodeMotionStartEvent(Time, BuildingBlock*, const Cell3DPosition &fpos, HHLattice::Direction forient);
		HexanodeMotionStartEvent(HexanodeMotionStartEvent *ev);
		~HexanodeMotionStartEvent();
		void consumeBlockEvent() override {}
		void consume() override;
		const virtual string getEventName() override;
	};

//===========================================================================================================
//
//          HexanodeMotionStopEvent  (class)
//
//===========================================================================================================

	class HexanodeMotionStopEvent : public BlockEvent {
		Cell3DPosition finalPosition;
		HHLattice::Direction finalOrientation;
	public:
		HexanodeMotionStopEvent(Time, BuildingBlock *block,const Cell3DPosition &fpos, HHLattice::Direction forient);
		~HexanodeMotionStopEvent();
		void consumeBlockEvent() override {}
		void consume() override;
		const virtual string getEventName() override;
	};

//===========================================================================================================
//
//          HexanodeMotionEndEvent  (class)
//
//===========================================================================================================

	class HexanodeMotionEndEvent : public BlockEvent {
	public:
		HexanodeMotionEndEvent(Time, BuildingBlock *block);
		HexanodeMotionEndEvent(HexanodeMotionEndEvent *ev);
		~HexanodeMotionEndEvent();
		void consumeBlockEvent() override {}
		void consume() override;
		const virtual string getEventName() override;
	};

}
#endif /* HEXANODEMOTIONEVENTS_H_ */
