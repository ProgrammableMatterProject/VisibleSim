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
#include "network.h"
#include "coordinate.h"

#define RECONFIGURATION_MSG 15004

class ReconfigurationMsg;
typedef std::shared_ptr<ReconfigurationMsg> ReconfigurationMsg_ptr;

class ReconfigurationMsg : public Message {
 public:
  static std::vector<unsigned int> hopCountStats;
  unsigned int hopCounter;
  
  enum subtype_t {CLEARANCE_REQUEST = 0, CLEARANCE, DELAYED_CLEARANCE_REQUEST, START_TO_MOVE, START_TO_MOVE_ACK, END_OF_MOVE};  
  subtype_t subtype;
      
 ReconfigurationMsg(unsigned int h) : Message() { 
    type = RECONFIGURATION_MSG;
    hopCounter = h;
    incrHopCountStats(h);
  }

 ReconfigurationMsg(ReconfigurationMsg *m) : Message() { 
    type = m->type; 
    subtype = m->subtype;
    hopCounter = m->hopCounter;
  }
  
  ~ReconfigurationMsg() {}
 
  virtual unsigned int size() {
    return 3*sizeof(unsigned int);
    // symbolizes the fields 2*"type" and "size" of the message
  }

  virtual std::string toString() = 0;

  static void incrHopCountStats(unsigned int index);
  static void printHopCountStats();
};

/******************************************
 * ReconfigurationClearanceRequestMsg
*******************************************/

class ReconfigurationClearanceRequestMsg;
typedef std::shared_ptr<ReconfigurationClearanceRequestMsg> ReconfigurationClearanceRequestMsg_ptr;

class ReconfigurationClearanceRequestMsg : public ReconfigurationMsg {
 public:
  ClearanceRequest request;
  
 ReconfigurationClearanceRequestMsg(ClearanceRequest &cr, unsigned int h) : ReconfigurationMsg(h) { 
    subtype = CLEARANCE_REQUEST;
    request = cr;
  }

 ReconfigurationClearanceRequestMsg(ReconfigurationClearanceRequestMsg *m) : ReconfigurationMsg(m) { 
    request = m->request;
  }
  
  ~ReconfigurationClearanceRequestMsg() {}
 
  unsigned int size() {
    return ReconfigurationMsg::size() + sizeof(ClearanceRequest);
  }

  std::string toString() {return "CLEARANCE_REQUEST: <request=" + request.toString() + ">";}
};

/******************************************
 * ReconfigurationClearanceMsg
*******************************************/

class ReconfigurationClearanceMsg;
typedef std::shared_ptr<ReconfigurationClearanceMsg> ReconfigurationClearanceMsg_ptr;

class ReconfigurationClearanceMsg : public ReconfigurationMsg {
 public:
  Clearance clearance;
  
 ReconfigurationClearanceMsg(Clearance &c, unsigned int h) : ReconfigurationMsg(h) { 
    subtype = CLEARANCE;
    clearance = c;
  }
  
 ReconfigurationClearanceMsg(ReconfigurationClearanceMsg *m) : ReconfigurationMsg(m) { 
    clearance = m->clearance;
  }
  
  ~ReconfigurationClearanceMsg() {}
 
  unsigned int size() {
    return ReconfigurationMsg::size() + sizeof(Clearance);
  }

  std::string toString() {return "CLEARANCE: <clearance=" + clearance.toString() + ">";}
};

/******************************************
 * ReconfigurationDelayedClearanceRequestMsg
*******************************************/

class ReconfigurationDelayedClearanceRequestMsg;
typedef std::shared_ptr<ReconfigurationDelayedClearanceRequestMsg> ReconfigurationDelayedClearanceRequestMsg_ptr;

class ReconfigurationDelayedClearanceRequestMsg : public ReconfigurationMsg {
 public:
  ClearanceRequest request;
  
 ReconfigurationDelayedClearanceRequestMsg(ClearanceRequest &cr, unsigned int h) : ReconfigurationMsg(h) { 
    subtype = DELAYED_CLEARANCE_REQUEST;
    request = cr;
  }

 ReconfigurationDelayedClearanceRequestMsg(ReconfigurationDelayedClearanceRequestMsg *m) : ReconfigurationMsg(m) { 
    request = m->request;
  }
  
  ~ReconfigurationDelayedClearanceRequestMsg() {}
 
  unsigned int size() {
    return ReconfigurationMsg::size() + sizeof(ClearanceRequest);
  }

  std::string toString() {return "DELAYED_CLEARANCE_REQUEST: <request=" + request.toString() + ">";}
};

/******************************************
 * ReconfigurationStartMoveMsg
*******************************************/

class ReconfigurationStartMoveMsg;
typedef std::shared_ptr<ReconfigurationStartMoveMsg>  ReconfigurationStartMoveMsg_ptr;

class  ReconfigurationStartMoveMsg : public ReconfigurationMsg {
 public:
  
 ReconfigurationStartMoveMsg(unsigned int h) : ReconfigurationMsg(h) { 
    subtype = START_TO_MOVE;
  }

 ReconfigurationStartMoveMsg(ReconfigurationStartMoveMsg *m) : ReconfigurationMsg(m) { 
  }
  
  ~ReconfigurationStartMoveMsg() {}
 
  unsigned int size() {
    return ReconfigurationMsg::size();
  }

  std::string toString() {return "START_TO_MOVE";}
};

/******************************************
 * ReconfigurationStartMoveAckMsg
*******************************************/

class ReconfigurationStartMoveAckMsg;
typedef std::shared_ptr<ReconfigurationStartMoveAckMsg>  ReconfigurationStartMoveAckMsg_ptr;

class  ReconfigurationStartMoveAckMsg : public ReconfigurationMsg {
 public:
  
 ReconfigurationStartMoveAckMsg(unsigned int h) : ReconfigurationMsg(h) { 
    subtype = START_TO_MOVE_ACK;
  }

 ReconfigurationStartMoveAckMsg(ReconfigurationStartMoveAckMsg *m) : ReconfigurationMsg(m) { 
  }
  
  ~ReconfigurationStartMoveAckMsg() {}
 
  unsigned int size() {
    return ReconfigurationMsg::size();
  }

  std::string toString() {return "START_MOVE_ACK";}
};

/******************************************
 * ReconfigurationEndMoveMsg
*******************************************/

class ReconfigurationEndMoveMsg;
typedef std::shared_ptr<ReconfigurationEndMoveMsg>  ReconfigurationEndMoveMsg_ptr;

class  ReconfigurationEndMoveMsg : public ReconfigurationMsg {
 public:
  Clearance clearance;
  
 ReconfigurationEndMoveMsg(Clearance &c, unsigned int h) : ReconfigurationMsg(h) { 
    subtype = END_OF_MOVE;
    clearance = c;
  }

 ReconfigurationEndMoveMsg(ReconfigurationEndMoveMsg *m) : ReconfigurationMsg(m) { 
    clearance = m->clearance;
  }
  
  ~ReconfigurationEndMoveMsg() {}
 
  unsigned int size() {
    return ReconfigurationMsg::size() + sizeof(Clearance);
  }

  std::string toString() {return "END_OF_MOVE: <clearance=" + clearance.toString() + ">";}
};

#endif
