/*
 * reconfiguration.h
 *
 *  Created on: 12 avril 2013
 *      Author: andre
 */

#ifndef RECONFIGURATION_MSG_H_
#define RECONFIGURATION_MSG_H_

#include <map>

#include "reconfiguration.h"
#include "network.h"
#include "coordinate.h"

#define RECONFIGURATION_MSG 15004

class ReconfigurationMsg;
typedef boost::shared_ptr<ReconfigurationMsg> ReconfigurationMsg_ptr;

//class ReconfigurationStartMsg;
//typedef boost::shared_ptr<ReconfigurationStartMsg> ReconfigurationStartMsg_ptr;

class ReconfigurationStateQueryMsg;
typedef boost::shared_ptr<ReconfigurationStateQueryMsg> ReconfigurationStateQueryMsg_ptr;

class ReconfigurationStateAnswerMsg;
typedef boost::shared_ptr<ReconfigurationStateAnswerMsg> ReconfigurationStateAnswerMsg_ptr;

class ReconfigurationMsg : public Message {
   public:
  enum subtype_t {STATE_QUERY = 0, STATE_ANSWER};  
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
};

class ReconfigurationStateQueryMsg : public ReconfigurationMsg {
   public:
      int sourceId;
      Coordinate cell;
      
      ReconfigurationStateQueryMsg(int i, Coordinate c) : ReconfigurationMsg() { 
         subtype = STATE_QUERY;
         cell = c;
         sourceId = i;
      }

      ReconfigurationStateQueryMsg(ReconfigurationStateQueryMsg *m) : ReconfigurationMsg(m) { 
         cell = m->cell;
         sourceId = m->sourceId;
      }
  
      ~ReconfigurationStateQueryMsg() {}
 
      unsigned int size() {return 17;};
};

class ReconfigurationStateAnswerMsg : public ReconfigurationMsg {
   public:
      Coordinate cell;
      int destinationId;
      std::map<int,Reconfiguration::state_t> neighborStates;
       
      ReconfigurationStateAnswerMsg(int i, Coordinate c, std::map<int, Reconfiguration::state_t> n) : ReconfigurationMsg() { 
         subtype = STATE_ANSWER;
         cell = c;
         destinationId = i;
         neighborStates = n;
      }

      ReconfigurationStateAnswerMsg(ReconfigurationStateAnswerMsg *m) : ReconfigurationMsg(m) { 
         cell = m->cell;
         destinationId = m->destinationId;
         neighborStates = m->neighborStates;
      }
  
      ~ReconfigurationStateAnswerMsg() {}
      
};

#endif
