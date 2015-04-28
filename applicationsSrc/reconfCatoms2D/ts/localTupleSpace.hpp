#ifndef LOCAL_TUPLE_SPACE_H_
#define LOCAL_TUPLE_SPACE_H_

#include "tupleSpace.hpp"
#include <map>

class LocalTupleSpace {
private:
  std::multimap<int,Tuple*> tuples;

public:
  LocalTupleSpace();
  ~LocalTupleSpace();

  void out(Tuple *t);
  Tuple* in(const Tuple &q);
  Tuple* inp(const Tuple &q);
};

#endif
