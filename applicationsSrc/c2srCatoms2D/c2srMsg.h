/*
 * c2srMsg.h
 *
 *  Created on: 25 Nov 2015
 *      Author: Andre Naz
 */

#ifndef C2SR_MSG_H_
#define C2SR_MSG_H_

#include <map>
#include <string>
#include <memory>
#include "c2sr.h"
#include "network.h"
#include "coordinate.h"

#define C2SR_MSG 15004

class C2SRMsg;
typedef std::shared_ptr<C2SRMsg> C2SRMsg_ptr;

class C2SRMsg : public Message {
 public:
  static std::vector<unsigned int> hopCountStats;
  unsigned int hopCounter;

  enum subtype_t {CLEARANCE_REQUEST = 0,
          CLEARANCE,
          DELAYED_CLEARANCE_REQUEST,
          START_TO_MOVE,
          START_TO_MOVE_ACK,
          END_OF_MOVE};

  subtype_t subtype;

 C2SRMsg(unsigned int h) : Message() {
    type = C2SR_MSG;
    hopCounter = h;
    incrHopCountStats(h);
  }

 C2SRMsg(C2SRMsg *m) : Message() {
    type = m->type;
    subtype = m->subtype;
    hopCounter = m->hopCounter;
  }

  ~C2SRMsg() {}

  virtual unsigned int size() {
    return 3*sizeof(unsigned int);
    // symbolizes the fields 2*"type" and "size" of the message
  }

  virtual std::string toString() = 0;

  static void incrHopCountStats(unsigned int index);
  static void printHopCountStats();
};

/******************************************
 * C2SRClearanceRequestMsg
*******************************************/

class C2SRClearanceRequestMsg;
typedef std::shared_ptr<C2SRClearanceRequestMsg> C2SRClearanceRequestMsg_ptr;

class C2SRClearanceRequestMsg : public C2SRMsg {
 public:
  ClearanceRequest request;

 C2SRClearanceRequestMsg(ClearanceRequest &cr, unsigned int h) : C2SRMsg(h) {
    subtype = CLEARANCE_REQUEST;
    request = cr;
  }

 C2SRClearanceRequestMsg(C2SRClearanceRequestMsg *m) : C2SRMsg(m) {
    request = m->request;
  }

  ~C2SRClearanceRequestMsg() {}

  unsigned int size() override {
    return C2SRMsg::size() + sizeof(ClearanceRequest);
  }

  std::string toString() override { return "CLEARANCE_REQUEST: <request=" + request.toString() + ">"; }
};

/******************************************
 * C2SRClearanceMsg
*******************************************/

class C2SRClearanceMsg;
typedef std::shared_ptr<C2SRClearanceMsg> C2SRClearanceMsg_ptr;

class C2SRClearanceMsg : public C2SRMsg {
 public:
  Clearance clearance;

 C2SRClearanceMsg(Clearance &c, unsigned int h) : C2SRMsg(h) {
    subtype = CLEARANCE;
    clearance = c;
  }

 C2SRClearanceMsg(C2SRClearanceMsg *m) : C2SRMsg(m) {
    clearance = m->clearance;
  }

  ~C2SRClearanceMsg() {}

  unsigned int size() override {
    return C2SRMsg::size() + sizeof(Clearance);
  }

  std::string toString() override { return "CLEARANCE: <clearance=" + clearance.toString() + ">"; }
};

/******************************************
 * C2SRDelayedClearanceRequestMsg
*******************************************/

class C2SRDelayedClearanceRequestMsg;
typedef std::shared_ptr<C2SRDelayedClearanceRequestMsg> C2SRDelayedClearanceRequestMsg_ptr;

class C2SRDelayedClearanceRequestMsg : public C2SRMsg {
 public:
  ClearanceRequest request;

 C2SRDelayedClearanceRequestMsg(ClearanceRequest &cr, unsigned int h) : C2SRMsg(h) {
    subtype = DELAYED_CLEARANCE_REQUEST;
    request = cr;
  }

 C2SRDelayedClearanceRequestMsg(C2SRDelayedClearanceRequestMsg *m) : C2SRMsg(m) {
    request = m->request;
  }

  ~C2SRDelayedClearanceRequestMsg() {}

  unsigned int size()  override {
    return C2SRMsg::size() + sizeof(ClearanceRequest);
  }

  std::string toString() override { return "DELAYED_CLEARANCE_REQUEST: <request=" + request.toString() + ">"; }
};

/******************************************
 * C2SRStartMoveMsg
*******************************************/

class C2SRStartMoveMsg;
typedef std::shared_ptr<C2SRStartMoveMsg>  C2SRStartMoveMsg_ptr;

class  C2SRStartMoveMsg : public C2SRMsg {
 public:

 C2SRStartMoveMsg(unsigned int h) : C2SRMsg(h) {
    subtype = START_TO_MOVE;
  }

 C2SRStartMoveMsg(C2SRStartMoveMsg *m) : C2SRMsg(m) {
  }

  ~C2SRStartMoveMsg() {}

  unsigned int size()  override {
    return C2SRMsg::size();
  }

  std::string toString() override { return "START_TO_MOVE"; }
};

/******************************************
 * C2SRStartMoveAckMsg
*******************************************/

class C2SRStartMoveAckMsg;
typedef std::shared_ptr<C2SRStartMoveAckMsg>  C2SRStartMoveAckMsg_ptr;

class  C2SRStartMoveAckMsg : public C2SRMsg {
 public:

 C2SRStartMoveAckMsg(unsigned int h) : C2SRMsg(h) {
    subtype = START_TO_MOVE_ACK;
  }

 C2SRStartMoveAckMsg(C2SRStartMoveAckMsg *m) : C2SRMsg(m) {
  }

  ~C2SRStartMoveAckMsg() {}

  unsigned int size()  override {
    return C2SRMsg::size();
  }

  std::string toString() override { return "START_MOVE_ACK";}
};

/******************************************
 * C2SREndMoveMsg
*******************************************/

class C2SREndMoveMsg;
typedef std::shared_ptr<C2SREndMoveMsg>  C2SREndMoveMsg_ptr;

class  C2SREndMoveMsg : public C2SRMsg {
 public:
  Clearance clearance;

 C2SREndMoveMsg(Clearance &c, unsigned int h) : C2SRMsg(h) {
    subtype = END_OF_MOVE;
    clearance = c;
  }

 C2SREndMoveMsg(C2SREndMoveMsg *m) : C2SRMsg(m) {
    clearance = m->clearance;
  }

  ~C2SREndMoveMsg() {}

  unsigned int size()  override {
    return C2SRMsg::size() + sizeof(Clearance);
  }

  std::string toString() override { return "END_OF_MOVE: <clearance=" +
      clearance.toString() + ">";}
};

#endif
