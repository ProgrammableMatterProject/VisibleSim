#ifndef CTUPLE_H
#define CTUPLE_H

#include <string>
#include <iostream>
#include "tuple.hpp"
#include "contextTuple.hpp"
#include "../coordinate.h"
#include "../rectangle.h"

typedef uint64_t Time;

class CTuple {
protected:
  Coordinate position;
  Tuple tuple;
  static Rectangle bounds;
public:
  
  CTuple(ContextTuple t);
  CTuple(Tuple t);
  
  // unable to call Tuple(t), runtime error. 
  CTuple(const CTuple& c);
  ~CTuple();

  Tuple& getTuple();
  Coordinate getPosition() const;
  bool match(CTuple &t) const;

  friend std::ostream& operator<<(std::ostream& os, const CTuple &ct);
  static void setBounds(Rectangle &r);
};

#endif
