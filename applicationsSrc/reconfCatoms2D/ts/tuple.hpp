#ifndef TUPLE_H_
#define TUPLE_H_

#include <vector>
#include <iostream>
#include <string>

#include "../coordinate.h"
#include "field.hpp"

class Tuple {
protected:
  std::string name;
  std::vector<Field> fields;
public:  

  Tuple() {};
  template<typename ... Types>
  Tuple(std::string n, Types ... fs) {fields =  {fs...}; name = n;};

  Tuple(const Tuple& t);
  ~Tuple();

  size_t size() const;
  const std::type_info& getType(size_t index);

  std::string getName() {return name;};
  
  template <typename T> T& get(size_t index) const {return fields[index].get<T>();};

  template <typename T> void set(size_t index, T v) {fields[index] = Field(v);};
  
  void erase(size_t index);

  template<typename ... Types>
  void add(Types ... fs) {
    Tuple t (std::string("noname"), fs...); 
    this->concatenate(t);
  };

  void concatenate(const Tuple &t);
  bool match(const Tuple &t) const;
  bool equal(const Tuple &t) const;

  void print();

  bool operator==(const Tuple& rhs) const;
  bool operator!=(const Tuple& rhs) const;
  friend std::ostream& operator<<(std::ostream& os, const Tuple &t);
  
  std::string getName() const {return getName();};
  size_t hash() const;
};

#endif
