/**
 * @file teleportationEvents.h
 * @brief This file implements the realization of a teleportation event for C3D modules.
 *   Its purpose is to serve as a placeholder motion to test algorithm while proper
 *  Catoms3D movement primitives are being developed.
 *
 *  Created on: 13/11/2017
 *      Author: Pierre Thalamy
 */

#ifndef NODEMOTIONEVENTS_H_
#define NODEMOTIONEVENTS_H_

#include "buildingBlock.h"
#include "events.h"
#include "lattice.h"

namespace BaseSimulator {

//===========================================================================================================
//
//          NodeMotionStartEvent  (class)
//
//===========================================================================================================

	class NodeMotionStartEvent : public BlockEvent {
		Cell3DPosition finalPosition;
		SLattice::Direction finalOrientation;
	public:
		NodeMotionStartEvent(Time, BuildingBlock*, const Cell3DPosition &fpos, SLattice::Direction forient);
		NodeMotionStartEvent(NodeMotionStartEvent *ev);
		~NodeMotionStartEvent();
		void consumeBlockEvent() override {}
		void consume() override;
		const virtual string getEventName() override;
	};

//===========================================================================================================
//
//          NodeMotionStopEvent  (class)
//
//===========================================================================================================

	class NodeMotionStopEvent : public BlockEvent {
		Cell3DPosition finalPosition;
		SLattice::Direction finalOrientation;
	public:
		NodeMotionStopEvent(Time, BuildingBlock *block,const Cell3DPosition &fpos, SLattice::Direction forient);
		~NodeMotionStopEvent();
		void consumeBlockEvent() override {}
		void consume() override;
		const virtual string getEventName() override;
	};

//===========================================================================================================
//
//          NodeMotionEndEvent  (class)
//
//===========================================================================================================

	class NodeMotionEndEvent : public BlockEvent {
	public:
		NodeMotionEndEvent(Time, BuildingBlock *block);
		NodeMotionEndEvent(NodeMotionEndEvent *ev);
		~NodeMotionEndEvent();
		void consumeBlockEvent() override {}
		void consume() override;
		const virtual string getEventName() override;
	};

}
#endif /* NODEMOTIONEVENTS_H_ */
