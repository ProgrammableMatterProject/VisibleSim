/*
 * reconfiguration.h
 *
 *  Created on: 12 avril 2013
 *      Author: andre
 */

#ifndef RECONFIGURATION_MSG_H_
#define RECONFIGURATION_MSG_H_

#include <map>
#include <string>
#include <memory>
#include "reconfiguration.h"
#include "comm/network.h"
#include "coordinate.h"

#define RECONFIGURATION_MSG 15004

class ReconfigurationMsg;
typedef std::shared_ptr<ReconfigurationMsg> ReconfigurationMsg_ptr;

class ReconfigurationMsg : public Message {
   public:
  enum subtype_t {STATE_QUERY = 0, STATE_UPDATE, START_MOVING, START_MOVING_ACK, STOP_MOVING, PLEASE_MOVE};
      subtype_t subtype;

      ReconfigurationMsg() : Message() {
         type = RECONFIGURATION_MSG;
      }

      ReconfigurationMsg(ReconfigurationMsg *m) : Message() {
         type = m->type;
         subtype = m->subtype;
      }

      ~ReconfigurationMsg() {}

      unsigned int size() {return 17;};

      virtual std::string toString() = 0;
};

class ReconfigurationStateQueryMsg;
typedef std::shared_ptr<ReconfigurationStateQueryMsg> ReconfigurationStateQueryMsg_ptr;

class ReconfigurationStateQueryMsg : public ReconfigurationMsg {
   public:
      Coordinate cell;

      ReconfigurationStateQueryMsg(Coordinate c) : ReconfigurationMsg() {
         subtype = STATE_QUERY;
         cell = c;
      }

      ReconfigurationStateQueryMsg(ReconfigurationStateQueryMsg *m) : ReconfigurationMsg(m) {
         cell = m->cell;
      }

      ~ReconfigurationStateQueryMsg() {}

      unsigned int size() {return 17;};

      std::string toString() {return "STATE_QUERY: <c=" + cell.toString() + ">";}
};

class ReconfigurationStateUpdateMsg;
typedef std::shared_ptr<ReconfigurationStateUpdateMsg> ReconfigurationStateUpdateMsg_ptr;

class ReconfigurationStateUpdateMsg : public ReconfigurationMsg {
   public:
      PerimeterCaseState states;

      ReconfigurationStateUpdateMsg(PerimeterCaseState s) : ReconfigurationMsg() {
         subtype = STATE_UPDATE;
         states = s;
      }

      ReconfigurationStateUpdateMsg(ReconfigurationStateUpdateMsg *m) : ReconfigurationMsg(m) {
         states = m->states;
      }

      ~ReconfigurationStateUpdateMsg() {}

      std::string toString() {return "STATE_UPDATE: <states=" + states.toString() + ">";}
};

class ReconfigurationStartMovingMsg;
typedef std::shared_ptr<ReconfigurationStartMovingMsg> ReconfigurationStartMovingMsg_ptr;

class ReconfigurationStartMovingMsg : public ReconfigurationMsg {
   public:

      ReconfigurationStartMovingMsg() : ReconfigurationMsg() {
         subtype = START_MOVING;
      }

      ReconfigurationStartMovingMsg(ReconfigurationStartMovingMsg *m) : ReconfigurationMsg(m) {}

      ~ReconfigurationStartMovingMsg() {}

      std::string toString() {return "START_MOVING";}
};

class ReconfigurationStartMovingAckMsg;
typedef std::shared_ptr<ReconfigurationStartMovingAckMsg> ReconfigurationStartMovingAckMsg_ptr;

class ReconfigurationStartMovingAckMsg : public ReconfigurationMsg {
   public:

      ReconfigurationStartMovingAckMsg() : ReconfigurationMsg() {
         subtype = START_MOVING_ACK;
      }

      ReconfigurationStartMovingAckMsg(ReconfigurationStartMovingAckMsg *m) : ReconfigurationMsg(m) {}

      ~ReconfigurationStartMovingAckMsg() {}

      std::string toString() {return "START_MOVING_ACK";}
};


class ReconfigurationStopMovingMsg;
typedef std::shared_ptr<ReconfigurationStopMovingMsg> ReconfigurationStopMovingMsg_ptr;

class ReconfigurationStopMovingMsg : public ReconfigurationMsg {
   public:

     Coordinate cell;

      ReconfigurationStopMovingMsg(Coordinate c) : ReconfigurationMsg() {
         subtype = STOP_MOVING;
     cell = c;
      }

      ReconfigurationStopMovingMsg(ReconfigurationStopMovingMsg *m) : ReconfigurationMsg(m) {
         cell = m->cell;
      }

      ~ReconfigurationStopMovingMsg() {}

      std::string toString() {return "STOP_MOVING";}

};

#endif
