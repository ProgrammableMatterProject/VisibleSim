#include "type.hpp"
#include <iostream>
#include <string>

using namespace std;

Type::Type(const std::type_info& t) : typeInfo(t) {}

bool Type::equal(const Type& t) const {
  return typeInfo == t.typeInfo;
}

bool Type::operator ==(const Type &t) const {
  return this->equal(t);
}
bool Type::operator !=(const Type &t) const {
  return !this->equal(t);
}

string Type::name() const { 
  return typeInfo.name();
}

std::ostream& operator<<(std::ostream& os, const Type &t) {
  os << t.name();
  return os;
}
