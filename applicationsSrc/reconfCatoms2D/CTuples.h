#ifndef CTUPLES_H_
#define CTUPLES_H_

#include "gpsr.h"
#include "map.h"
#include "tuple.hpp"
#include "localTupleSpace.hpp"

#define CTUPLES_MESSAGE 15003
class CTuplesMessage;
typedef boost::shared_ptr<CTuplesMessage> CTuplesMessage_ptr;

class CTuples {
 private:
  LocalTupleSpace tuples;
  void localOut(Tuple *t);
  Tuple* localInp(Tuple *t);

  GPSR &gpsr;
  Map &map;

 public:
  CTuples(GPSR &g, Map &m);
  CTuples(const CTuples &c);
  ~CTuples();

  void handleCTuplesMessage(CTuplesMessage_ptr m);

  // drop off a tuple
  void out(Tuple *t);

  // withdraw a tuple (blocking)
  Tuple* in(Tuple *t);
  
  /*
  // withdraw a tuple (non blocking)
  Tuple* inp(Tuple *t);
  
  // read a tuple (blocking)
  Tuple* read(Tuple *t);

  // withdraw a tuple (non blocking)
  Tuple* readp(Tuple *t);*/
};

class CTuplesMessage : public Message {
 public:
  enum mode_t {OUT = 0, IN, INP, READ, READP, ANSWER};
 protected:  
  // data information (payload)
  Tuple tuple;
  mode_t mode;
  
 public :
 CTuplesMessage(mode_t m, Tuple t) : Message() { 
    type = CTUPLES_MESSAGE;
    mode = m;
    tuple = t;
  };

  CTuplesMessage(CTuplesMessage *m) : Message() { 
    type = m->type; 
    mode = m->mode;
    tuple = m->tuple;
  };
  
  ~CTuplesMessage() {};

  Tuple getTuple() {return tuple;};
  mode_t getMode() {return mode;};
};

#endif
