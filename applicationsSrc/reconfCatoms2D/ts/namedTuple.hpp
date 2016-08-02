#ifndef NAMED_TUPLE_H
#define NAMED_TUPLE_H

#include <string>
#include <iostream>

class NamedTuple : public Tuple {
public:
  
  template<typename ... Types>
  NamedTuple(std::string name, Types ... fs) : Tuple(name,fs...) {};
  
  // unable to call Tuple(t), runtime error. 
  NamedTuple(const NamedTuple& t) : Tuple() { fields = t.fields; };

  ~NamedTuple() {}

  std::string getName() { return get<std::string>(0); }; 
};

#endif
