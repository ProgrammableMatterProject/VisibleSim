#include "localTupleSpace.hpp"
#include <unistd.h>

using namespace std;

LocalTupleSpace::LocalTupleSpace() {}
  
LocalTupleSpace::~LocalTupleSpace() {}

void LocalTupleSpace::out(Tuple* t) {
  int key = t->hash();
  tuples.insert(pair<int,Tuple*>(key,t));
}

Tuple* LocalTupleSpace::in(const Tuple &q) {
  Tuple *res = NULL;
  while (true) {
    res = inp(q);
    if (res != NULL) {
      break;
    }
    sleep(5);
  }
  return res;
}


Tuple* LocalTupleSpace::inp(const Tuple &q) {
  int key = q.hash();
  Tuple *res = NULL; 

  //multimap<int, Tuple*>::iterator it = tuples.find(key);
  pair <multimap<int,Tuple*>::iterator, multimap<int,Tuple*>::iterator> range;
  range = tuples.equal_range(key);

  for (multimap<int,Tuple*>::iterator it=range.first; it!=range.second; ++it) {
    res = it->second;
    if (res->match(q)) { // check matching to circumvent collisions
      tuples.erase(it);
      break;
    }
  }
  return res;
}

size_t LocalTupleSpace::size() const {
  return tuples.size();
}
