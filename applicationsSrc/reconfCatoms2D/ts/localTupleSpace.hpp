#ifndef LOCAL_TUPLE_SPACE_H_
#define LOCAL_TUPLE_SPACE_H_

#include "tupleSpace.hpp"
#include <map>

class LocalTupleSpace {
private:
  std::map<int,Tuple> tuples;

public:
  LocalTupleSpace();
  ~LocalTupleSpace();

  void out(Tuple t);
  Tuple in(Tuple p);
};

#endif
