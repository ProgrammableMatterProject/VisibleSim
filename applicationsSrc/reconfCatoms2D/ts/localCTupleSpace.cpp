#include "localCTupleSpace.hpp"
#include <unistd.h>

using namespace std;

LocalCTupleSpace::LocalCTupleSpace() {}
  
LocalCTupleSpace::~LocalCTupleSpace() {}

void LocalCTupleSpace::out(CTuple* t) {
  size_t key = t->getTuple().hash();
  ctuples.insert(pair<size_t,CTuple*>(key,t));
}

CTuple* LocalCTupleSpace::inp(CTuple &q) {
  size_t key = q.getTuple().hash();
  CTuple *res = NULL; 
  
  pair <multimap<size_t,CTuple*>::iterator, multimap<size_t,CTuple*>::iterator> range;
  range = ctuples.equal_range(key);

  for (multimap<size_t,CTuple*>::iterator it=range.first; it!=range.second; ++it) {
    res = it->second;
    if (res->match(q)) { // check matching to circumvent collisions
      ctuples.erase(it);
      return res;
    }
  }
  return NULL;
}

size_t LocalCTupleSpace::size() const {
  return ctuples.size();
}
