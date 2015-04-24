#include "tuple.hpp"

Tuple::Tuple(int k, int d) {
  key = k;
  data = d;
}

Tuple::Tuple(const Tuple& t) {
  key = t.key;
  data = t.data;
}

Tuple::~Tuple() {

}

int Tuple::getKey() {
  return key;
}

int Tuple::getData() {
  return data;
}
