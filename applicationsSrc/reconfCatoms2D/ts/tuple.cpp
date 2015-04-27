#include "tuple.hpp"
#include <iostream>

using namespace std;

Tuple::Tuple(const Tuple& t) {
  cout << "tuple (&t)" << endl;
  fields = t.fields;
}

Tuple::~Tuple() {}

size_t Tuple::size() const {
  return fields.size();
}

const std::type_info& Tuple::getType(size_t index) {
  return fields[index].getType();
}

void Tuple::print() {
  cout << "Tuple: ";
  for (unsigned i = 0; i < fields.size(); i++) {
    cout << getType(i).name() << " ";
    cout << " ";
  }
  cout << endl;
}

void Tuple::concatenate(Tuple &t) {
  fields.insert(fields.end(), t.fields.begin(), t.fields.end());
}

bool Tuple::match(const Tuple &t) const {
  //return fields == t.fields;
  return true;
}

bool Tuple::equal(const Tuple &t) const {
  if (size() != t.size()) {
    return false;
  }
  return fields == t.fields;
}

void Tuple::erase(size_t index) {
  if (index >= size()) {
    return;
  }
  fields.erase(fields.begin()+index);
}
