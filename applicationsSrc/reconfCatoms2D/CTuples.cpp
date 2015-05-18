#include "CTuples.h"
#include "gsrp.h"

CTuples::CTuples(GPSR &g, Map &m): gpsr(g), map(m) {}

CTuples::CTuples(CTuples const &c) {}

CTuples::~CTuples() {}

void CTuples::handleCTuplesMessage(CTuplesMessage_ptr m) {
  switch(m->getMode()) {
  case CTuplesMessage::OUT: {
    localOut(new ContextTuple(m->getTuple()));
  }
    break;
  case CTuplesMessage::IN:
    {
      ContextTuple q = m->getTuple();
      ContextTuple *r = localInp(&q);
      gpsr.send(map.getPosition(),m->getSource(),*r,GeoMessage::ANSWER);
      
    }
    break;
  case CTuplesMessage::ANSWER: {
    ContextTuple *r = new ContextTuple(m->getTuple());
    getScheduler->schedule(new TupleQueryResponseEvent(scheduler->now(),catom2D,r));
  }
    break;
  }
}

void Catoms2D1BlockCode::localOut(ContextTuple *t) {
  localTuples.out(t);
}

ContextTuple* Catoms2D1BlockCode::localInp(ContextTuple *t) {
  return (ContextTuple*) localTuples.in(t);
}

void Catoms2D1BlockCode::out(ContextTuple *t) {
#ifdef TUPLE_DEBUG
  cout << "insert tuple: " << *t << endl;
#endif
 // tuple should maybe stored locally
  if (t->getLocation() == map.getPosition()) {
    localOut(t);
  } else {
    // or remotely, send the tuple
    GeoMessage * msg = new GeoMessage(map.getPosition(),t->getLocation(),*t,GeoMessage::STORE);
    send(msg);
  }
}

void Catoms2D1BlockCode::inp(ContextTuple *t) {
#ifdef TUPLE_DEBUG
  cout << "insert tuple: " << *t << endl;
#endif
  // first try locally
  ContextTuple *r  = localInp(t);
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
  }
}
