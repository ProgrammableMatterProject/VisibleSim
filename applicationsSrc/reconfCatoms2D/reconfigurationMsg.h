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
  enum subtype_t {CLEARANCE_REQUEST = 0, CLEARANCE, DELAYED_CLEARANCE_REQUEST, START_TO_MOVE, START_TO_MOVE_ACK, END_OF_MOVE};  
  subtype_t subtype;
      
 ReconfigurationMsg() : Message() { 
    type = RECONFIGURATION_MSG;
  }

 ReconfigurationMsg(ReconfigurationMsg *m) : Message() { 
    type = m->type; 
    subtype = m->subtype;     
  }
  
  ~ReconfigurationMsg() {}
 
  unsigned int size() {return sizeof(ReconfigurationMsg);};

  virtual std::string toString() = 0;
};

/******************************************
 * ReconfigurationClearanceRequestMsg
*******************************************/

class ReconfigurationClearanceRequestMsg;
typedef std::shared_ptr<ReconfigurationClearanceRequestMsg> ReconfigurationClearanceRequestMsg_ptr;

class ReconfigurationClearanceRequestMsg : public ReconfigurationMsg {
 public:
  ClearanceRequest request;
  
 ReconfigurationClearanceRequestMsg(ClearanceRequest &cr) : ReconfigurationMsg() { 
    subtype = CLEARANCE_REQUEST;
    request = cr;
  }

 ReconfigurationClearanceRequestMsg(ReconfigurationClearanceRequestMsg *m) : ReconfigurationMsg(m) { 
    request = m->request;
  }
  
  ~ReconfigurationClearanceRequestMsg() {}
 
  unsigned int size() {return sizeof(ReconfigurationClearanceRequestMsg);};

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
  
 ReconfigurationClearanceMsg(Clearance &c) : ReconfigurationMsg() { 
    subtype = CLEARANCE;
    clearance = c;
  }
  
 ReconfigurationClearanceMsg(ReconfigurationClearanceMsg *m) : ReconfigurationMsg(m) { 
    clearance = m->clearance;
  }
  
  ~ReconfigurationClearanceMsg() {}
 
  unsigned int size() {return sizeof(ReconfigurationClearanceMsg);};

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
  
 ReconfigurationDelayedClearanceRequestMsg(ClearanceRequest &cr) : ReconfigurationMsg() { 
    subtype = DELAYED_CLEARANCE_REQUEST;
    request = cr;
  }

 ReconfigurationDelayedClearanceRequestMsg(ReconfigurationDelayedClearanceRequestMsg *m) : ReconfigurationMsg(m) { 
    request = m->request;
  }
  
  ~ReconfigurationDelayedClearanceRequestMsg() {}
 
  unsigned int size() {return sizeof(ReconfigurationDelayedClearanceRequestMsg);};

  std::string toString() {return "DELAYED_CLEARANCE_REQUEST: <request=" + request.toString() + ">";}
};

/******************************************
 * ReconfigurationStartMoveMsg
*******************************************/

class ReconfigurationStartMoveMsg;
typedef std::shared_ptr<ReconfigurationStartMoveMsg>  ReconfigurationStartMoveMsg_ptr;

class  ReconfigurationStartMoveMsg : public ReconfigurationMsg {
 public:
  
 ReconfigurationStartMoveMsg() : ReconfigurationMsg() { 
    subtype = START_TO_MOVE;
  }

 ReconfigurationStartMoveMsg(ReconfigurationStartMoveMsg *m) : ReconfigurationMsg(m) { 
  }
  
  ~ReconfigurationStartMoveMsg() {}
 
  unsigned int size() {return sizeof(ReconfigurationStartMoveMsg);};

  std::string toString() {return "START_TO_MOVE";}
};

/******************************************
 * ReconfigurationStartMoveAckMsg
*******************************************/

class ReconfigurationStartMoveAckMsg;
typedef std::shared_ptr<ReconfigurationStartMoveAckMsg>  ReconfigurationStartMoveAckMsg_ptr;

class  ReconfigurationStartMoveAckMsg : public ReconfigurationMsg {
 public:
  
 ReconfigurationStartMoveAckMsg() : ReconfigurationMsg() { 
    subtype = START_TO_MOVE_ACK;
  }

 ReconfigurationStartMoveAckMsg(ReconfigurationStartMoveAckMsg *m) : ReconfigurationMsg(m) { 
  }
  
  ~ReconfigurationStartMoveAckMsg() {}
 
  unsigned int size() {return sizeof(ReconfigurationStartMoveAckMsg);};

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
  
 ReconfigurationEndMoveMsg(Clearance &c) : ReconfigurationMsg() { 
    subtype = END_OF_MOVE;
    clearance = c;
  }

 ReconfigurationEndMoveMsg(ReconfigurationEndMoveMsg *m) : ReconfigurationMsg(m) { 
    clearance = m->clearance;
  }
  
  ~ReconfigurationEndMoveMsg() {}
 
  unsigned int size() {return sizeof(ReconfigurationEndMoveMsg);};

  std::string toString() {return "END_OF_MOVE: <clearance=" + clearance.toString() + ">";}
};

#endif
