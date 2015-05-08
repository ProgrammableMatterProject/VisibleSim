#ifndef CONTEXT_TUPLE_H
#define CONTEXT_TUPLE_H

#include <string>
#include <iostream>
#include "coordinate.hpp"

typedef uint64_t Time;

class ContextTuple : public Tuple {
public:
  template<typename ... Types>
  ContextTuple(Coordinate c, std::string n, Types ... fs) : Tuple(c,0,n,fs...) {};

  template<typename ... Types>
  ContextTuple(Coordinate c, Time t, std::string n, Types ... fs) : Tuple(c,t,n,fs...) {};

  // unable to call Tuple(t), runtime error. 
  ContextTuple(const ContextTuple& t) : Tuple() {
    fields = t.fields;
  };

  ~ContextTuple() {}

  Coordinate getLocation() {return get<Coordinate>(0);}; 
  Time getTime() {return get<Time>(1);};
  std::string getName() {return get<std::string>(2);};
  
};

#endif
