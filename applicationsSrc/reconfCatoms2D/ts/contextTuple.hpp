#ifndef CONTEXT_TUPLE_H
#define CONTEXT_TUPLE_H

#include <string>
#include <iostream>
#include "../coordinate.h"

//typedef uint64_t Time;

class ContextTuple : public Tuple {
public:
  template<typename ... Types>
  ContextTuple(std::string n, Coordinate c, Types ... fs) : Tuple(n,c,fs...) {};

  // unable to call Tuple(t), runtime error. 
  ContextTuple(const ContextTuple& t) : Tuple() {
    name = t.name;
    fields = t.fields;
  };

  ~ContextTuple() {}

  virtual Coordinate getPosition() const {return get<Coordinate>(0);};
  
};

#endif
