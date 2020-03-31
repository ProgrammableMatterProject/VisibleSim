#ifndef LOCAL_CTUPLE_SPACE_H_
#define LOCAL_CTUPLE_SPACE_H_

#include "CTuple.hpp"
#include <map>

class LocalCTupleSpace {
private:
  std::multimap<size_t,CTuple*> ctuples;

public:
  LocalCTupleSpace();
  ~LocalCTupleSpace();

  void out(CTuple *t);
  CTuple* inp(CTuple &q);
  size_t size() const;
};

#endif
