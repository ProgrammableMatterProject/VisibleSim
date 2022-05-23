/**
 * @file teleportationEvents.h
 * @brief This file implements the realization of a teleportation event for C3D modules.
 *   Its purpose is to serve as a placeholder motion to test algorithm while proper
 *  Catoms3D movement primitives are being developed.
 *
 *  Created on: 13/11/2017
 *      Author: Pierre Thalamy
 */

#ifndef TELEPORTATIONEVENTS_H_
#define TELEPORTATIONEVENTS_H_

#include "../base/buildingBlock.h"
#include "../events/events.h"

namespace BaseSimulator {

//===========================================================================================================
//
//          TeleportationStartEvent  (class)
//
//===========================================================================================================

     class TeleportationStartEvent : public BlockEvent {
          Cell3DPosition finalPosition;
         uint8_t finalOrientation;
     public:
         TeleportationStartEvent(Time, BuildingBlock*, const Cell3DPosition &fpos);
         TeleportationStartEvent(Time, BuildingBlock*, const Cell3DPosition &fpos, uint8_t orient);
         TeleportationStartEvent(TeleportationStartEvent *ev);
          ~TeleportationStartEvent();
          void consumeBlockEvent() override {}
          void consume() override;
          const virtual string getEventName() override;
     };

//===========================================================================================================
//
//          TeleportationStopEvent  (class)
//
//===========================================================================================================

     class TeleportationStopEvent : public BlockEvent {
          Cell3DPosition finalPosition;
         uint8_t finalOrientation;
     public:
          TeleportationStopEvent(Time, BuildingBlock *block,const Cell3DPosition &fpos, uint8_t orient);
          ~TeleportationStopEvent();
          void consumeBlockEvent() override {}
          void consume() override;
          const virtual string getEventName() override;
     };

//===========================================================================================================
//
//          TeleportationEndEvent  (class)
//
//===========================================================================================================

     class TeleportationEndEvent : public BlockEvent {
     public:
          TeleportationEndEvent(Time, BuildingBlock *block);
          TeleportationEndEvent(TeleportationEndEvent *ev);
          ~TeleportationEndEvent();
          void consumeBlockEvent() override {}
          void consume() override;
          const virtual string getEventName() override;
     };

}
#endif /* TELEPORTATIONEVENTS_H_ */
