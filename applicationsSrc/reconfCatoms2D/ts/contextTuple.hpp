#ifndef CONTEXT_TUPLE_H
#define CONTEXT_TUPLE_H

#include <string>
#include <iostream>
#include "coordinate.hpp"

typedef uint64_t Time;

class ContextTuple : public Tuple {
public:
  template<typename ... Types>
  ContextTuple(Coordinate c, Types ... fs) : Tuple(c,0,fs...) {};

  template<typename ... Types>
  ContextTuple(Coordinate c, Time t,Types ... fs) : Tuple(c,t,fs...) {};

  // unable to call Tuple(t), runtime error. 
  ContextTuple(const ContextTuple& t) : Tuple() {
    fields = t.fields;
  };

  ~ContextTuple() {}

  Coordinate getLocation() {return get<Coordinate>(0);}; 
  Time getTime() {return get<Time>(1);};
};

#endif
