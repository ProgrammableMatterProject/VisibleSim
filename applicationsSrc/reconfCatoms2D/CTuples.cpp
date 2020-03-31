#include "CTuples.h"
//#include "gpsr.h"

//using namespace Catoms2D;

CTuples::CTuples(Catoms2D1BlockCode *bc) {
  blockCode = bc;
  map = bc->map;
}

CTuples::CTuples(CTuples const &c): blockCode(c.blockCode), map(c.map) {}

CTuples::~CTuples() {}

void CTuples::handleCTuplesMessage(MessagePtr msg) {
  CTuplesMessage_ptr m = std::static_pointer_cast<CTuplesMessage>(msg);
  switch(m->getMode()) {
  case CTuplesMessage::OUT: {
    localOut(m->getCTuple());
  }
    break;
  case CTuplesMessage::IN:
  case CTuplesMessage::INP:
  case CTuplesMessage::READ:
  case CTuplesMessage::READP:
    {
      //ContextTuple q = m->getTuple();
      //ContextTuple *r = localInp(&q);
      //gpsr.send(map.getPosition(),m->getSource(),*r,GeoMessage::ANSWER);
      
    }
    break;
  case CTuplesMessage::ANSWER: {
    //ContextTuple *r = new ContextTuple(m->getTuple());
    //getScheduler->schedule(new TupleQueryResponseEvent(scheduler->now(),catom2D,r));
  }
    break;
    }
}

void CTuples::localOut(CTuple t) {
  cout << map->position << " stores " << t << endl; 
  localCTuples.out(new CTuple(t));
}

CTuple* CTuples::localInp(CTuple t) {
  return localCTuples.inp(t);
}

void CTuples::out(Tuple t) {
  out(CTuple(t));
}

void CTuples::out(ContextTuple t) {
  out(CTuple(t));
}

void CTuples::out(CTuple t) {
#ifdef TUPLE_DEBUG
  cout << "insert tuple: " << t << endl;
#endif
  // tuple should maybe stored locally
  if (t.getPosition() == map->getPosition()) {
    localOut(t);
  } else {
    // or remotely, send the tuple
    CTuplesMessage *msg = new CTuplesMessage(CTuplesMessage::OUT,t);
    //gpsr.send(map.getPosition(),t.getPosition(),msg);
  }
}

void CTuples::inp(CTuple t) {
#ifdef TUPLE_DEBUG
  cout << "query tuple: " << *t << endl;
#endif
  // first try locally
  /* ContextTuple *r  = localInp(t);
  if (r != NULL) {
    scheduler->schedule(new TupleQueryResponseEvent(scheduler->now(),catom2D,r));
  } else {
    // tuple is maybe stored locally
    if (t->getLocation() == map.getPosition()) {
      // no such tuple
      scheduler->schedule(new TupleQueryResponseEvent(scheduler->now(),catom2D,NULL));
    } else {
      // tuple is maybe be stored remotely
#ifdef TUPLE_DEBUG
      cout << "remotely" << endl;
#endif
      GeoMessage * msg = new GeoMessage(map.getPosition(),t->getLocation(),*t,GeoMessage::STORE);
      send(msg);
    }
    }*/
}
