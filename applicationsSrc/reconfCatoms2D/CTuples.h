#ifndef CTUPLES_H_
#define CTUPLES_H_

#include "map.h"
#include "CTuple.hpp"
#include "catom2D1BlockCode.h"
#include "localCTupleSpace.hpp"

#define CTUPLES_MSG 15003
class CTuplesMessage;
typedef boost::shared_ptr<CTuplesMessage> CTuplesMessage_ptr;

class Catoms2D1BlockCode;

class CTuples {
 private:
  LocalCTupleSpace localCTuples;
  void localOut(CTuple t);

  Map *map;
  Catoms2D1BlockCode *blockCode;
  
 public:
  CTuples(Catoms2D1BlockCode *bc);
  CTuples(const CTuples &c);
  ~CTuples();

  void handleCTuplesMessage(MessagePtr msg);

  // drop off a tuple
  void out(Tuple t);
  void out(ContextTuple t);
  void out(CTuple t);

  // withdraw a tuple (blocking)
  //CTuple* in(CTuple *t);
  
  
  // withdraw a tuple (non blocking)
  void inp(CTuple t);
  CTuple* localInp(CTuple t);
  
  /*
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
  CTuple ctuple;
  mode_t mode;
  
 public:
 CTuplesMessage(mode_t m, CTuple ct) : Message(), ctuple(ct) { 
    type = CTUPLES_MSG;
    mode = m;
  };

 CTuplesMessage(CTuplesMessage *m) : Message(), ctuple(m->ctuple) { 
    type = m->type;
    mode = m->mode;
  };
  
  ~CTuplesMessage() {};

  CTuple getCTuple() {return ctuple;};
  mode_t getMode() {return mode;};
};

#endif
