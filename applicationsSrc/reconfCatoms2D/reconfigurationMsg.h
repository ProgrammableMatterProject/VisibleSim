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
#include "reconfiguration.h"
#include "network.h"
#include "coordinate.h"

#define RECONFIGURATION_MSG 15004

class ReconfigurationMsg;
typedef boost::shared_ptr<ReconfigurationMsg> ReconfigurationMsg_ptr;

/*class ReconfigurationStartMsg;
typedef boost::shared_ptr<ReconfigurationStartMsg> ReconfigurationStartMsg_ptr;
*/

class ReconfigurationMsg : public Message {
   public:
  enum subtype_t {STATE_QUERY = 0, STATE_UPDATE, START_MOVING, STOP_MOVING, PLEASE_MOVE};  
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
typedef boost::shared_ptr<ReconfigurationStateQueryMsg> ReconfigurationStateQueryMsg_ptr;

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
typedef boost::shared_ptr<ReconfigurationStateUpdateMsg> ReconfigurationStateUpdateMsg_ptr;

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
typedef boost::shared_ptr<ReconfigurationStartMovingMsg> ReconfigurationStartMovingMsg_ptr;

class ReconfigurationStartMovingMsg : public ReconfigurationMsg {
   public:
       
      ReconfigurationStartMovingMsg() : ReconfigurationMsg() { 
         subtype = START_MOVING;
      }

      ReconfigurationStartMovingMsg(ReconfigurationStartMovingMsg *m) : ReconfigurationMsg(m) {}
  
      ~ReconfigurationStartMovingMsg() {}
      
      std::string toString() {return "START_MOVING";}
};

class ReconfigurationStopMovingMsg;
typedef boost::shared_ptr<ReconfigurationStopMovingMsg> ReconfigurationStopMovingMsg_ptr;

class ReconfigurationStopMovingMsg : public ReconfigurationMsg {
   public: 

      ReconfigurationStopMovingMsg() : ReconfigurationMsg() { 
         subtype = STOP_MOVING;
      }

      ReconfigurationStopMovingMsg(ReconfigurationStopMovingMsg *m) : ReconfigurationMsg(m) {}
  
      ~ReconfigurationStopMovingMsg() {}

      std::string toString() {return "STOP_MOVING";}
      
};

#endif
