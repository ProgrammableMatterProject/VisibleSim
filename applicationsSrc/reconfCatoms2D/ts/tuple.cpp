#include "tuple.hpp"
#include <iostream>

using namespace std;

Tuple::Tuple(const Tuple& t) {
  name = t.name;
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
  cout << *this << endl;
}

void Tuple::concatenate(const Tuple &t) {
  fields.insert(fields.end(), t.fields.begin(), t.fields.end());
}

bool Tuple::match(const Tuple &t) const {
  vector<Field>::const_iterator it1;
  vector<Field>::const_iterator it2;
  bool res = true;
  
  if (name != t.name) {
    return false;
  }
  
  if (size() != t.size()) {
    return false;
  }

  int i = 0;
  for (it1 = fields.begin(), it2 = t.fields.begin(); it1 != fields.end(), it2 != t.fields.end(); ++it1, ++it2) {
    res = res && it1->match(*it2);
    i++;
  }
  return res;
}

bool Tuple::equal(const Tuple &t) const {
  if (name != t.name) {
    return false;
  }

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

bool Tuple::operator==(const Tuple& rhs) const {
  return this->equal(rhs);
}

bool Tuple::operator!=(const Tuple& rhs) const {
  return !this->equal(rhs);
} 

std::ostream& operator<<(std::ostream& os, const Tuple &t) {
  vector<Field>::const_iterator it;
  os << "(";
  os << t.name << ",";
  for (it = t.fields.begin(); it != t.fields.end(); ++it) {
    os << *it;
    if (it+1 != t.fields.end()) {
      os << ",";
    }
  }
  os << ")";
  return os;
}

size_t Tuple::hash() const{
  std::hash<std::string> hash_function;
  return hash_function(name);
}
